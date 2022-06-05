use crate::any_ref::DynBox;
use crate::hooks::{HookContext, RenderContext};
use crate::renderer::{DispatchNativeEvent, NativeEvent};
use crate::{
    hooks::Gen,
    renderer::{NativeHandle, NativeType, Renderer, Scheduler},
};
use crate::{ChildId, Component, ComponentPos, View};

use crate::shared::Shared;
use std::num::NonZeroU64;
use std::{any::Any, cell::RefCell, collections::HashMap, hash::Hash, panic::Location, rc::Rc};

use self::wrappers::ComponentStateAccess;

const DYNAMIC_CHILDREN_ERR: &str = "Dynamic components must be provided keys.";

/// Holds all the component nodes for a given root, as well as state information
/// for allowing updates and the dataflow tracking system to function.
pub(crate) struct VDom {
    /// Stores all the nodes in the vdom, addressable by their `ComponentId`.
    pub(crate) children: HashMap<ComponentId, VNode>,
    /// Yields the `ComponentId` for the next created `VNode`, available by calling
    /// the `create_next` method.
    pub(crate) curr_component_id: ComponentId,
    /// The renderer used to convert avalanche representations of native components into
    /// actual native components.
    pub(crate) renderer: Box<dyn Renderer>,
    /// The current state update generation.
    pub(crate) gen: Gen,
    /// Updates the vdom by rendering the root component and all descendents that are dirty.
    pub(crate) update_vdom:
        fn(&mut VDom, &Shared<VDom>, &Shared<dyn Scheduler>, Option<(NativeEvent, ComponentId)>),
}

// separate wrapper types to keep data structures maintaining safety invariants as isolated as possible
pub(crate) mod wrappers {
    use std::{any::Any, marker::PhantomData, panic::Location};

    /// A wrapper over a `Box`, with a raw pointer to its memory, so that
    /// references derived from it do not have a `Box`'s provenance and
    /// remain valid when the `SharedBox` is moved.
    pub struct SharedBox<T: ?Sized> {
        value: *mut T,
        _marker: PhantomData<T>,
    }

    impl<T: ?Sized> SharedBox<T> {
        pub fn new(value: Box<T>) -> Self {
            Self {
                value: Box::into_raw(value),
                _marker: PhantomData,
            }
        }

        /// safety: Caller must ensure that the box is destroyed only after the end of
        /// the provided lifetime 'a.
        /// a reference from `get_mut` must not be active while
        /// a reference returned from this method is
        unsafe fn get_ref<'a>(&self) -> &'a T {
            &*self.value
        }

        pub fn get_mut(&mut self) -> &mut T {
            // safety: as the receiver of this method is &mut,
            // Rust reference invariants ensure this is safe
            unsafe { &mut *self.value }
        }
    }

    impl<T: ?Sized> Drop for SharedBox<T> {
        fn drop(&mut self) {
            // safety: by construction of `self.value` in `new`, it is
            // a valid `Box`-allocated memory location, and thus can be converted back
            // into a `Box`.
            unsafe {
                drop(Box::from_raw(self.value));
            }
        }
    }

    /// A wrapper over `ComponentState` allowing for safe additions and immutable access of state duing component rendering.
    pub(crate) struct ComponentStateAccess<'a> {
        /// `inner`'s `Rc` elements MUST NOT be removed or destroyed in any fashion
        /// during the lifetime `'a`. In addition, `&mut` references pointing to the
        /// interior of `Rc` elements MUST NOT be created or accessed during the lifetime `'a`.
        /// Violating this leads to memory unsafety.
        inner: &'a mut super::ComponentState,
    }

    impl<'a> ComponentStateAccess<'a> {
        pub fn new(inner: &'a mut super::ComponentState) -> Self {
            Self { inner }
        }

        pub fn get_or_insert_with(
            &mut self,
            key: Location<'static>,
            value: impl FnOnce() -> SharedBox<dyn Any>,
        ) -> &'a dyn Any {
            let elem = self.inner.entry(key).or_insert_with(value);

            // safety: The box cannot be destroyed or mutably dereferenced until the end of the lifetime
            // 'a, as per the guarantees on inner.
            unsafe { elem.get_ref() }
        }
    }
}

pub(crate) type ComponentState = HashMap<Location<'static>, wrappers::SharedBox<dyn Any>>;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
/// Represents a unique instance of a component within the lifetime
/// of a particular vdom tree.
pub(crate) struct ComponentId {
    pub id: NonZeroU64,
}

impl ComponentId {
    /// Returns the next instance of a `ComponentId`, modifying the given instance
    /// to allow generating new ids.
    pub fn create_next(&mut self) -> Self {
        let id = self.id;
        self.id = NonZeroU64::try_from(self.id.get() + 1).unwrap();
        ComponentId { id }
    }

    pub fn new() -> Self {
        Self {
            id: NonZeroU64::new(1).unwrap(),
        }
    }
}

/// Holds data specific to avalanche components representing native components.
struct NativeComponent {
    native_handle: NativeHandle,
    native_type: NativeType,
    native_parent: Option<ComponentId>,
    native_children: Vec<ComponentId>,
}

/// Data associated with a child rendered within the body of another function.
struct BodyChild {
    /// The component id associated with a child id.
    id: ComponentId,
    /// Whether a component has been rendered over.
    used: bool,
}

/// State and other data associated with a non-empty component within the `VDom`.
pub(crate) struct VNode {
    /// The component in which the VNode's component was rendered in, if any.
    pub body_parent: Option<ComponentId>,
    /// The components rendered within the render function of the component, if any.
    body_children: HashMap<ChildId, BodyChild>,
    /// The direct children of the component, where `()`-derived ones are represented
    /// by `None`.
    pub children: Vec<Option<ComponentId>>,
    /// The native information of the given component, if it is native.
    native_component: Option<NativeComponent>,
    /// The hook state of the given component.
    pub(crate) state: ComponentState,
    /// Whether the VNode is in need of updating due to a change in state in
    /// itself or a descendent.
    pub(crate) dirty: bool,
    /// Memoized value of the component's `ComponentId` and its closest native descendent.
    pub(crate) view: View,
}

/// A type-erased container for `Component` allowing virtualization
/// to avoid code bloat.
struct DynComponent<'a> {
    updated: bool,
    native_type: Option<NativeType>,
    inner: DynBox<'a>,
    get_children: fn(DynBox<'a>) -> Vec<View>,
    get_render: fn(DynBox<'a>, RenderContext, HookContext) -> View,
    get_child_id: fn(&DynBox<'a>) -> ChildId,
}

impl<'a> DynComponent<'a> {
    fn new<C: Component<'a>>(component: C) -> Self {
        fn get_children<'a, C: Component<'a>>(c: DynBox<'a>) -> Vec<View> {
            c.downcast::<C>().unwrap().children()
        }

        fn get_render<'a, C: Component<'a>>(
            c: DynBox<'a>,
            render_ctx: RenderContext,
            hook_ctx: HookContext,
        ) -> View {
            c.downcast::<C>().unwrap().render(render_ctx, hook_ctx)
        }

        fn get_child_id<'a, C: Component<'a>>(c: &DynBox<'a>) -> ChildId {
            let c_ref = c.as_ref().downcast_ref::<C>().unwrap();
            ChildId {
                location: c_ref.location().unwrap_or_default(),
                key: c_ref.key(),
            }
        }

        Self {
            updated: component.updated(),
            native_type: component.native_type(),
            inner: DynBox::new(component),
            get_children: get_children::<C>,
            get_render: get_render::<C>,
            get_child_id: get_child_id::<C>,
        }
    }

    fn updated(&self) -> bool {
        self.updated
    }

    fn native_type(&self) -> Option<NativeType> {
        self.native_type
    }

    fn children(self) -> Vec<View> {
        (self.get_children)(self.inner)
    }

    fn render(self, render_ctx: RenderContext, hook_ctx: HookContext) -> View {
        (self.get_render)(self.inner, render_ctx, hook_ctx)
    }

    fn child_id(&self) -> ChildId {
        (self.get_child_id)(&self.inner)
    }
}

/// Renders a child of a component, given a reference to that component's `VNode` and
/// the `Component` instance describing the child. Avoids re-rendering if the child was
/// rendered before and neither its data nor the state of itself or one of its descendents
/// were updated since the last render.
/// Called within the code generated by `#[component]`. Library consumers should avoid calling this
/// themselves, as this interface is not guaranteed to be stable.
#[doc(hidden)]
pub fn render_child<'a>(component: impl Component<'a>, context: &mut RenderContext) -> View {
    /// A non-monomorphized implementation of the function to avoid a heavy code size penalty
    /// for every component rendered in an application.
    fn render_child(component: DynComponent, context: &mut RenderContext) -> View {
        let child_id = component.child_id();
        // Gets the `ComponentId` of the child if it existed previously,
        // and generates a new one otherwise.
        let body_child = context
            .vnode
            .body_children
            .entry(child_id)
            .or_insert(BodyChild {
                id: context.vdom.curr_component_id.create_next(),
                used: true,
            });

        // Marks the child as used so its body parent will not destroy it.
        body_child.used = true;

        let child_component_id = body_child.id;
        // Gets the `VNode` associated with the `ComponentId` and removes it to
        // allow for concurrent modification of the vdom and vnode. If the vnode does not
        // exist yet, create a default value.
        let mut child_vnode = context
            .vdom
            .children
            .remove(&child_component_id)
            .unwrap_or_else(|| {
                let native_type = component.native_type();
                let dispatch_native_event = DispatchNativeEvent {
                    component_id: child_component_id,
                    vdom: context.component_pos.vdom.clone(),
                    scheduler: context.scheduler.clone(),
                };
                let native_component = native_type.map(|native_type| {
                    let native_handle = context.vdom.renderer.create_component(
                        &native_type,
                        component.inner.as_ref(),
                        dispatch_native_event,
                    );
                    NativeComponent {
                        native_handle,
                        native_type,
                        native_parent: None,
                        native_children: Vec::new(),
                    }
                });
                VNode {
                    body_parent: Some(context.component_pos.component_id),
                    body_children: HashMap::new(),
                    children: Vec::new(),
                    native_component,
                    state: HashMap::new(),
                    dirty: true,
                    view: View {
                        id: Some(child_component_id),
                        // will be overwritten in update code
                        native_component_id: None,
                    },
                }
            });

        child_vnode.body_parent = Some(context.component_pos.component_id);
        let view = if child_vnode.dirty || component.updated() {
            let native_component_id = match &mut child_vnode.native_component {
                Some(native_component) => {
                    let current_native_event = match &mut context.current_native_event {
                        Some((_, id)) => {
                            if *id == child_component_id {
                                context.current_native_event.take().map(|(event, _)| event)
                            } else {
                                None
                            }
                        }
                        None => None,
                    };
                    context.vdom.renderer.update_component(
                        &native_component.native_type,
                        &mut native_component.native_handle,
                        component.inner.as_ref(),
                        current_native_event,
                    );
                    let new_children = component.children();
                    let new_children: Vec<_> =
                        new_children.into_iter().map(|child| child.id).collect();
                    let mut new_native_children: Vec<_> = new_children
                        .iter()
                        .filter_map(|id| {
                            id.and_then(|id| context.vdom.children[&id].view.native_component_id)
                        })
                        .collect();
                    update_native_children(
                        child_component_id,
                        &mut native_component.native_children,
                        &mut new_native_children,
                        &native_component.native_type,
                        &mut native_component.native_handle,
                        context.vdom,
                    );
                    native_component.native_children = new_native_children;
                    child_vnode.children = new_children;

                    Some(child_component_id)
                }
                None => {
                    // swap state out of vnode to allow passing a mut VDom reference down the stack
                    let mut state = ComponentState::new();
                    std::mem::swap(&mut state, &mut child_vnode.state);
                    // wrap state to allow joint hook creation and state consumption
                    let shared_state = Shared::new(ComponentStateAccess::new(&mut state));

                    // Before rendering mark all body children as unused
                    for (_, child) in child_vnode.body_children.iter_mut() {
                        child.used = false;
                    }

                    let child_hook_context = HookContext {
                        gen: context.vdom.gen,
                        state: &shared_state,
                        component_pos: ComponentPos {
                            component_id: child_component_id,
                            vdom: context.component_pos.vdom,
                        },
                        scheduler: context.scheduler,
                    };
                    let child_render_context = RenderContext {
                        vdom: context.vdom,
                        vnode: &mut child_vnode,
                        component_pos: ComponentPos {
                            component_id: child_component_id,
                            vdom: context.component_pos.vdom,
                        },
                        scheduler: context.scheduler,
                        current_native_event: context.current_native_event,
                    };

                    let view: View = component.render(child_render_context, child_hook_context);

                    // After rendering destroy all components that are body children but were not referenced in the render
                    child_vnode
                        .body_children
                        .retain(|_, BodyChild { id, used }| {
                            if !*used {
                                remove_node(context.vdom, *id);
                            };
                            *used
                        });

                    // restore the vnode's state after swapping it out earlier
                    std::mem::swap(&mut state, &mut child_vnode.state);
                    child_vnode.children = view.id.into_iter().map(Some).collect();
                    view.native_component_id
                }
            };
            child_vnode.view.native_component_id = native_component_id;
            child_vnode.dirty = false;
            child_vnode.view.private_copy()
        } else {
            child_vnode.view.private_copy()
        };
        // After removing the child's vnode from the vdom, restore it,
        // or introduce it for the first time
        context
            .vdom
            .children
            .insert(child_component_id, child_vnode);
        view
    }

    // Call the type-erased implementation of the function
    render_child(DynComponent::new(component), context)
}

/// Remove a vnode from `vdom`, along with all of its descendents.
fn remove_node(vdom: &mut VDom, node: ComponentId) {
    let removed_node = vdom.children.remove(&node);
    if let Some(removed_node) = removed_node {
        for child in removed_node.children.into_iter().flatten() {
            remove_node(vdom, child);
        }
    }
}

/// Takes old and new native children, as component ids of native components.
/// `old_native_children` must be a superset of the native children of `parent_handle`.
/// Updates the native children of `parent_handle` such that its native children correspond
/// with the native components of each child in `new_native_child` in order.
pub(crate) fn update_native_children(
    parent_id: ComponentId,
    old_native_children: &mut Vec<ComponentId>,
    new_native_children: &mut [ComponentId],
    parent_type: &NativeType,
    parent_handle: &mut NativeHandle,
    vdom: &mut VDom,
) {
    // Filter out components from old_native_children that have been reparented elsewhere, and thus are no longer
    // present within the given `parent_handle`
    old_native_children.retain(|child| {
        vdom.children[child]
            .native_component
            .as_ref()
            .unwrap()
            .native_parent
            == Some(parent_id)
    });
    let mut old_native_children_map = HashMap::with_capacity(std::cmp::max(
        old_native_children.len(),
        new_native_children.len(),
    ));
    for (i, child) in old_native_children.iter().enumerate() {
        old_native_children_map.insert(*child, (i, *child));
    }

    if old_native_children.len() != old_native_children_map.len() {
        panic!("{}", DYNAMIC_CHILDREN_ERR);
    }

    for (i, new_child) in new_native_children.iter().enumerate() {
        let (j, _) = old_native_children_map
            .entry(*new_child)
            .or_insert_with(|| {
                let new_native_component = vdom
                    .children
                    .get_mut(new_child)
                    .unwrap()
                    .native_component
                    .as_mut()
                    .unwrap();
                new_native_component.native_parent = Some(parent_id);
                vdom.renderer.append_child(
                    parent_type,
                    parent_handle,
                    &new_native_component.native_type,
                    &new_native_component.native_handle,
                );
                let i = old_native_children.len();
                old_native_children.push(*new_child);
                (i, *new_child)
            });
        let j = *j;
        if i != j {
            vdom.renderer
                .swap_children(parent_type, parent_handle, i, j);
            let swap_child = old_native_children[i];
            old_native_children_map.get_mut(&swap_child).unwrap().0 = j;
            old_native_children.swap(i, j);
        }
    }

    if new_native_children.len() != old_native_children.len() {
        vdom.renderer
            .truncate_children(parent_type, parent_handle, new_native_children.len());
    }
}

/// Walks up the VDom upwards and marks the given node and its body parents dirty to allow for running the update algorithm
pub(crate) fn mark_node_dirty(vdom: &mut VDom, mut component_id: ComponentId) {
    loop {
        let vnode = match vdom.children.get_mut(&component_id) {
            Some(vnode) => vnode,
            None => return,
        };
        vnode.dirty = true;
        component_id = match vnode.body_parent {
            Some(body_parent) => body_parent,
            None => break,
        }
    }
}

/// Contains the data structures necessary to support the avalanche vdom abstraction. This struct
/// should only be used by renderer implementation libraries.
///
/// # Usage
///
/// In order to render an avalanche `View`, a renderer library should accept a `View` from the user, then
/// use the `new` method to create a `Root` instance.
pub struct Root {
    _vdom: Shared<VDom>,
}

impl Root {
    /// Creates a new UI tree rooted at `native_parent`, with native handle `native_handle`. That handle will be used
    /// in order to allow rooting an avalanche tree upon
    /// an existing UI component created externally. Renders `child` as the child of `native_handle`.
    pub fn new<'a, R: Renderer + 'static, S: Scheduler + 'static, C: Component<'a> + Default>(
        native_type: NativeType,
        native_handle: NativeHandle,
        renderer: R,
        scheduler: S,
    ) -> Self {
        let mut curr_component_id = ComponentId::new();
        let root_component_id = curr_component_id.create_next();
        let mut children = HashMap::with_capacity(16);
        let vnode = VNode {
            body_parent: None,
            body_children: HashMap::with_capacity(1),
            children: vec![Some(ComponentId {
                id: NonZeroU64::new(2).unwrap(),
            })],
            native_component: Some(NativeComponent {
                native_handle,
                native_type,
                native_parent: None,
                native_children: Vec::new(),
            }),
            state: HashMap::new(),
            dirty: false,
            view: View {
                id: Some(root_component_id),
                native_component_id: Some(root_component_id),
            },
        };
        children.insert(root_component_id, vnode);
        let vdom = VDom {
            children,
            curr_component_id,
            renderer: Box::new(renderer),
            gen: Gen::new(),
            update_vdom: render_vdom::<C>,
        };
        let vdom = Shared::new(vdom);
        let vdom_clone = vdom.clone();
        let scheduler: Shared<dyn Scheduler> = Shared::new_dyn(Rc::new(RefCell::new(scheduler)));
        vdom.exec_mut(|vdom| {
            render_vdom::<C>(vdom, &vdom_clone, &scheduler, None);
            vdom.gen.inc();
        });
        Root { _vdom: vdom }
    }
}

fn render_vdom<'a, C: Component<'a> + Default>(
    vdom: &mut VDom,
    shared_vdom: &Shared<VDom>,
    scheduler: &Shared<dyn Scheduler>,
    mut current_native_event: Option<(NativeEvent, ComponentId)>,
) {
    let mut parent_vnode = vdom.children.remove(&ComponentId::new()).unwrap();
    render_child(
        C::default(),
        &mut RenderContext {
            vdom,
            vnode: &mut &mut parent_vnode,
            component_pos: ComponentPos {
                component_id: ComponentId::new(),
                vdom: shared_vdom,
            },
            scheduler,
            current_native_event: &mut current_native_event,
        },
    );
    vdom.children.insert(ComponentId::new(), parent_vnode);

    // TODO: code duplicated from render_child; factor out into function?
    // furthermore code is not clean: tweak function interfaces to clean up and remove need to remove root vnode
    let new_children: Vec<Option<ComponentId>> =
        vdom.children[&ComponentId::new()].children.clone();
    let mut new_native_children: Vec<_> = new_children
        .iter()
        .filter_map(|id| id.and_then(|id| vdom.children[&id].view.native_component_id))
        .collect();
    let mut vnode = vdom.children.remove(&ComponentId::new()).unwrap();
    let native_component = vnode.native_component.as_mut().unwrap();
    update_native_children(
        ComponentId::new(),
        &mut native_component.native_children,
        &mut new_native_children,
        &native_component.native_type,
        &mut native_component.native_handle,
        vdom,
    );
    native_component.native_children = new_native_children;
    vdom.children.insert(ComponentId::new(), vnode);
}
