use std::cell::Cell;
use std::mem::{swap, ManuallyDrop};
use std::num::NonZeroU64;
use std::{any::Any, cell::RefCell, hash::Hash, panic::Location, rc::Rc};

use rustc_hash::FxHashMap;

use crate::hooks::{HookContext, RenderContext, SharedContext};
use crate::renderer::{DispatchNativeEvent, NativeEvent};
use crate::tracked::Gen;
use crate::{
    renderer::{NativeHandle, Renderer, Scheduler},
    tracked::InternalGen,
};
use crate::{ChildId, Component, ComponentPos, DefaultComponent, View};
use crate::shared::Shared;
use crate::alloc::{Bump, Box as BumpBox, Vec as BumpVec, CollectIn};

use self::wrappers::ComponentStateAccess;

const DYNAMIC_CHILDREN_ERR: &str = "Dynamic components must be provided keys.";

/// Holds all the component nodes for a given root, as well as state information
/// for allowing updates and the dataflow tracking system to function.
pub(crate) struct VDom {
    /// Stores all the nodes in the vdom, addressable by their `ComponentId`.
    pub(crate) children: FxHashMap<ComponentId, VNode>,
    /// Yields the `ComponentId` for the next created `VNode`, available by calling
    /// the `create_next` method.
    pub(crate) curr_component_id: ComponentId,
    /// The renderer used to convert avalanche representations of native components into
    /// actual native components.
    pub(crate) renderer: Box<dyn Renderer>,
    /// The current state update generation.
    pub(crate) gen: InternalGen,
    /// Updates the vdom by rendering the root component and all descendents that are dirty.
    pub(crate) update_vdom:
        fn(&mut VDom, &Shared<VDom>, &Shared<dyn Scheduler>, Option<(NativeEvent, ComponentId)>),
    /// Allows for efficient render-time allocations.
    pub(crate) bump: Bump,
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

pub(crate) type ComponentState = FxHashMap<Location<'static>, wrappers::SharedBox<dyn Any>>;

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
    body_children: FxHashMap<ChildId, BodyChild>,
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

mod dyn_component {
    use std::marker::PhantomData;

    use super::*;
    type NativeUpdateType = unsafe fn(
        *mut (),
        &mut dyn Renderer,
        &NativeHandle,
        Gen,
        Option<NativeEvent>,
    );

    /// Contains pointers for executing methods on an instance of a
    /// `Component` behind a `*mut ()`.
    struct DynComponentVTable {
        native_create: unsafe fn(*mut (), &mut dyn Renderer, DispatchNativeEvent) -> NativeHandle,
        get_render: unsafe fn(*mut (), RenderContext, HookContext) -> View,
        native_update: NativeUpdateType,
        drop: unsafe fn(*mut ()),
    }

    /// A type-erased container for `Component` allowing virtualization
    /// to avoid code bloat.
    pub(crate) struct DynComponent<'a, 'b> {
        updated: bool,
        location: Option<(u32, u32)>,
        /// A pointer to a `Box`-allocated instance of a Component<'a>.
        /// It must never be null and always be valid when dropped.
        inner: *mut (),
        is_native: bool,
        vtable: &'static DynComponentVTable,
        native_children: unsafe fn (*mut ()) -> &'a [View],
        /// whether the component pointed to by inner
        component_dropped: bool,
        /// Ensures the `DynComponent` cannot outlive lifetime of the data
        /// held in `inner`.
        phantom: PhantomData<(&'a (), &'b ())>,
    }

    trait ComponentVTable {
        const VTABLE: DynComponentVTable;
    }

    unsafe fn native_create<'a, C: Component<'a>>(
        c: *mut (),
        renderer: &mut dyn Renderer,
        dispatch_native_event: DispatchNativeEvent,
    ) -> NativeHandle {
        // SAFETY: by the conditions of `native_update`.
        let component_ref = unsafe { &*c.cast::<C>() };
        component_ref.native_create(renderer, dispatch_native_event)
    }

    impl<'a, C: Component<'a>> ComponentVTable for C {
        const VTABLE: DynComponentVTable = DynComponentVTable {
            native_create: native_create::<C>,
            get_render: get_render::<C>,
            native_update: native_update::<C>,
            drop: drop::<C>,
        };
    }

    /// SAFETY: `c` must be a valid pointer to an instance of `C`.
    unsafe fn native_update<'a, C: Component<'a>>(
        c: *mut (),
        renderer: &mut dyn Renderer,
        native_handle: &NativeHandle,
        curr_gen: Gen,
        event: Option<NativeEvent>,
    ) {
        // SAFETY: by the conditions of `native_update`.
        let component_ref = unsafe { &*c.cast::<C>() };
        component_ref.native_update(renderer, native_handle, curr_gen, event);
    }

    /// SAFETY: `c` must be a valid pointer to an instance of `C`.
    /// The value pointed to by `c` must not be used after this
    /// function is called.
    unsafe fn native_children<'a, C: Component<'a>>(
        c: *mut(),
    ) -> &'a [View] {
        let component = unsafe { c.cast::<C>().read() };
        component.native_children()
    }


    /// SAFETY: `c` must be a valid pointer to an instance of `C`.
    /// The value pointed to by `c` must not be used after this
    /// function is called.
    unsafe fn get_render<'a, C: Component<'a>>(
        c: *mut (),
        render_ctx: RenderContext,
        hook_ctx: HookContext,
    ) -> View {
        // SAFETY: by the conditions of `get_render`.
        let component = unsafe { c.cast::<C>().read() };
        component.render(render_ctx, hook_ctx)
    }

    /// SAFETY: `c` must be a valid pointer to an instance of `C`.
    /// The value pointed to by `c` must not be used after this
    /// function is called.
    unsafe fn drop<'a, C: Component<'a>>(c: *mut ()) {
        let _ = unsafe { c.cast::<C>().read() };
    }

    impl<'a, 'b> DynComponent<'a, 'b> {
        pub(super) fn new_in<C: Component<'a>>(
            component: C,
            gen: InternalGen,
            bump: &'b Bump,
        ) -> Self {
            Self {
                updated: component.updated(gen.into()),
                location: component.location(),
                is_native: component.is_native(),
                inner: BumpBox::into_raw(BumpBox::new_in(component, bump)).cast(),
                vtable: &C::VTABLE,
                native_children: native_children::<C>,
                component_dropped: false,
                phantom: PhantomData,
            }
        }

        pub(super) fn updated(&self) -> bool {
            self.updated
        }

        pub(super) fn location(&self) -> Option<(u32, u32)> {
            self.location
        }
        
        pub(super) fn is_native(&self) -> bool {
            self.is_native
        }

        pub(super) fn native_create(
            &self,
            renderer: &mut dyn Renderer,
            dispatch_native_event: DispatchNativeEvent,
        ) -> NativeHandle {
            // SAFETY: self.inner is valid.
            unsafe { (self.vtable.native_create)(self.inner, renderer, dispatch_native_event) }
        }

        pub(super) fn native_update(
            &self,
            renderer: &mut dyn Renderer,
            native_handle: &NativeHandle,
            curr_gen: Gen,
            event: Option<NativeEvent>,
        ) {
            // SAFETY: self.inner is valid.
            unsafe {
                (self.vtable.native_update)(
                    self.inner,
                    renderer,
                    native_handle,
                    curr_gen,
                    event,
                )
            };
        }
        
        pub(super) fn native_children(mut self) -> &'a [View] {
            self.component_dropped = true;
            // SAFETY: self.inner is valid and is not dereferenced after this call.
            unsafe {
                (self.native_children)(self.inner)
            }
        }

        pub(super) fn render(mut self, render_ctx: RenderContext, hook_ctx: HookContext) -> View {
            self.component_dropped = true;
            // SAFETY: self.inner is valid and is not dereferenced after this call.
            let ret = unsafe { (self.vtable.get_render)(self.inner, render_ctx, hook_ctx) };

            ret
        }
    }

    impl<'a, 'b> Drop for DynComponent<'a, 'b> {
        fn drop(&mut self) {
            // SAFETY: inner must always be a valid allocation created by a
            // Box that holds a valid Component instance.
            unsafe {
                if !self.component_dropped {
                    (self.vtable.drop)(self.inner);
                }
            }
        }
    }
}

use dyn_component::DynComponent;

/// Renders a child of a component, given a reference to that component's `VNode` and
/// the `Component` instance describing the child. Avoids re-rendering if the child was
/// rendered before and neither its data nor the state of itself or one of its descendents
/// were updated since the last render.
/// Called within the code generated by `#[component]`. Library consumers should avoid calling this
/// themselves, as this interface is not guaranteed to be stable.
#[doc(hidden)]
pub fn render_child<'a>(component: impl Component<'a>, context: &RenderContext) -> View {
    /// A non-monomorphized implementation of the function to avoid a heavy code size penalty
    /// for every component rendered in an application.
    fn render_child<'a>(component: DynComponent, context: &RenderContext) -> View {
        // Tracks whether the component being rendered, along with its corresponding
        // native handle, was created during this call
        let mut newly_created = false;

        let (child_component_id, child_vnode_dirty, native_component_is_some, original_view) =
            context.vdom.exec_mut(|vdom| {
                let child_id = ChildId {
                    key: context.shared.key.get().map(ToOwned::to_owned),
                    location: component.location().unwrap_or_default(),
                };
                // Gets the `ComponentId` of the child if it existed previously,
                // and generates a new one otherwise.
                let body_child = vdom
                    .children
                    .get_mut(&context.body_parent_id)
                    .unwrap()
                    .body_children
                    .entry(child_id)
                    .or_insert(BodyChild {
                        id: vdom.curr_component_id.create_next(),
                        used: true,
                    });

                // Marks the child as used so its body parent will not destroy it.
                body_child.used = true;

                let child_component_id = body_child.id;
                // Gets the `VNode` associated with the `ComponentId` and removes it to
                // allow for concurrent modification of the vdom and vnode. If the vnode does not
                // exist yet, create a default value.
                let mut child_vnode =
                    vdom.children.entry(child_component_id).or_insert_with(|| {
                        newly_created = true;

                        let dispatch_native_event = DispatchNativeEvent {
                            component_id: child_component_id,
                            vdom: context.component_pos.vdom.downgrade(),
                            scheduler: context.shared.scheduler.clone(),
                        };
                        let native_component = component.is_native().then(|| {
                            let native_handle = component
                                .native_create(vdom.renderer.as_mut(), dispatch_native_event);
                            NativeComponent {
                                native_handle,
                                native_parent: None,
                                native_children: Vec::new(),
                            }
                        });
                        VNode {
                            body_parent: Some(context.component_pos.component_id),
                            body_children: FxHashMap::default(),
                            children: Vec::new(),
                            native_component,
                            state: FxHashMap::default(),
                            dirty: true,
                            view: View {
                                id: Some(child_component_id),
                                // will be overwritten in update code
                                native_component_id: None,
                            },
                        }
                    });
                child_vnode.body_parent = Some(context.component_pos.component_id);
                let native_component_is_some = child_vnode.native_component.is_some();
                (
                    child_component_id,
                    child_vnode.dirty,
                    native_component_is_some,
                    child_vnode.view.private_copy(),
                )
            });

        // If the component's props were updated or it is dirty because it was just
        // created or its state was updated, re-render the child.
        let view = if child_vnode_dirty || component.updated() {
            let native_component_id = if native_component_is_some {
                context.vdom.exec_mut(|vdom| {
                    let native_component = vdom.children[&child_component_id]
                        .native_component
                        .as_ref()
                        .unwrap();

                    // Extract current_native_event
                    let mut current_native_event = context.shared.current_native_event.take();

                    let native_event = match &current_native_event {
                        Some((_, id)) => {
                            if *id == child_component_id {
                                current_native_event.take().map(|(event, _)| event)
                            } else {
                                None
                            }
                        }
                        None => None,
                    };

                    // restore potentially modified current_native_event
                    context.shared.current_native_event.set(current_native_event);
                    
                    // Update only required if the component was not created during
                    // this call
                    if !newly_created {
                        component.native_update(
                            vdom.renderer.as_mut(),
                            &native_component.native_handle,
                            vdom.gen.into(),
                            native_event,
                        );
                    }
                    
                    let new_children = component.native_children();

                    let new_native_children: BumpVec<_> = new_children
                        .iter()
                        .filter_map(|child| child.native_component_id)
                        .collect_in(context.bump);

                    let new_children: BumpVec<_> = new_children
                        .into_iter()
                        .map(|child| child.id)
                        .collect_in(context.bump);

                    let mut old_native_children = native_component
                        .native_children
                        .iter()
                        .cloned()
                        .collect_in(context.bump);

                    update_native_children(
                        child_component_id,
                        &mut old_native_children,
                        &new_native_children,
                        vdom,
                        context.bump,
                    );

                    let child_vnode = vdom.children.get_mut(&child_component_id).unwrap();
                    let native_component = child_vnode.native_component.as_mut().unwrap();

                    // Update children vectors by replacing them with contents of new children
                    // Note that this approach can reduce allocations by reusing the existing
                    // vector's allocation
                    native_component.native_children.clear();
                    native_component.native_children.extend(new_native_children);

                    child_vnode.children.clear();
                    child_vnode.children.extend(new_children);
                });

                Some(child_component_id)
            } else {
                let mut state = ComponentState::default();
                let vdom_gen = context.vdom.exec_mut(|vdom| {
                    let child_vnode = vdom.children.get_mut(&child_component_id).unwrap();
                    // swap state out of vnode to allow passing a mut VDom reference down the stack
                    std::mem::swap(&mut state, &mut child_vnode.state);

                    // Before rendering mark all body children as unused
                    for (_, child) in child_vnode.body_children.iter_mut() {
                        child.used = false;
                    }

                    vdom.gen
                });
                // wrap state to allow joint hook creation and state consumption
                let shared_state = Shared::new(ComponentStateAccess::new(&mut state));

                let child_render_context = RenderContext {
                    vdom: context.vdom,
                    body_parent_id: child_component_id,
                    component_pos: ComponentPos {
                        component_id: child_component_id,
                        vdom: context.component_pos.vdom,
                    },
                    bump: context.bump,
                    shared: context.shared,
                };

                let child_hook_context = HookContext {
                    gen: vdom_gen.into(),
                    state: &shared_state,
                    component_pos: ComponentPos {
                        component_id: child_component_id,
                        vdom: context.component_pos.vdom,
                    },
                    bump: context.bump,
                    shared: context.shared,
                };
                
                // Set key to None, as within the component, it becomes a body parent
                // to its children, which shouldn't inherit the body parent's key
                let key = context.shared.key.replace(None);

                let view: View = component.render(child_render_context, child_hook_context);
                
                // Restore key
                context.shared.key.replace(key);

                context.vdom.exec_mut(|vdom| {
                    let child_vnode = vdom.children.get_mut(&child_component_id).unwrap();
                    // After rendering destroy all components that are body children but were not referenced in the render
                    child_vnode
                        .body_children
                        .retain(|_, BodyChild { id, used }| {
                            if !*used {
                                context.shared.components_to_remove.push(*id, context.bump);
                            };
                            *used
                        });

                    // restore the vnode's state after swapping it out earlier
                    std::mem::swap(&mut state, &mut child_vnode.state);
                    child_vnode.children = view.id.into_iter().map(Some).collect();
                });

                view.native_component_id
            };

            context.vdom.exec_mut(|vdom| {
                let child_vnode = vdom.children.get_mut(&child_component_id).unwrap();

                child_vnode.view.native_component_id = native_component_id;
                child_vnode.dirty = false;
                child_vnode.view.private_copy()
            })
        } else {
            original_view
        };
        view
    }

    // Call the type-erased implementation of the function
    render_child(
        DynComponent::new_in(component, context.vdom.exec(|vdom| vdom.gen), &context.bump),
        context,
    )
}

/// Remove a vnode from `vdom`, along with all of its descendents.
fn remove_node(vdom: &mut VDom, mut to_remove: BumpVec<ComponentId>) {
    while let Some(node) = to_remove.pop() {
        if let Some(node) = vdom.children.remove(&node) {
            to_remove.extend(node.children.into_iter().flatten());
        }
    }
}

/// Takes old and new native children, as component ids of native components.
/// `old_native_children` must be a superset of the native children of `parent_handle`.
/// Updates the native children of `parent_handle` such that its native children correspond
/// with the native components of each child in `new_native_child` in order.
pub(crate) fn update_native_children(
    parent_id: ComponentId,
    old_native_children: &mut BumpVec<ComponentId>,
    new_native_children: &[ComponentId],
    vdom: &mut VDom,
    bump: &Bump,
) {
    let parent_native_component = vdom.children[&parent_id].native_component.as_ref().unwrap();
    let parent_handle = &parent_native_component.native_handle;

    // Fast path for component being completely cleared of children
    if new_native_children.is_empty() {
        if !old_native_children.is_empty() {
            vdom.renderer.truncate_children(parent_handle, 0);
        }
        return;
    }
    
    // TODO: implement better diffing algorithm.
    
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
    let mut old_native_children_map = FxHashMap::default();
    old_native_children_map.reserve(std::cmp::max(
        old_native_children.len(),
        new_native_children.len(),
    ));
    for (i, child) in old_native_children.iter().enumerate() {
        old_native_children_map.insert(*child, (i, *child));
    }

    if old_native_children.len() != old_native_children_map.len() {
        panic!("{}", DYNAMIC_CHILDREN_ERR);
    }

    let mut set_native_parent = BumpVec::new_in(bump);


    for (i, new_child) in new_native_children.iter().enumerate() {
        let (j, _) = old_native_children_map
            .entry(*new_child)
            .or_insert_with(|| {
                let new_native_component =
                    vdom.children[new_child].native_component.as_ref().unwrap();
                if new_native_component.native_parent != Some(parent_id) {
                    set_native_parent.push(*new_child);
                }
                vdom.renderer.append_child(
                    parent_handle,
                    &new_native_component.native_handle,
                );
                let i = old_native_children.len();
                old_native_children.push(*new_child);
                (i, *new_child)
            });
        let j = *j;
        if i != j {
            vdom.renderer
                .swap_children(parent_handle, i, j);
            let swap_child = old_native_children[i];
            old_native_children_map.get_mut(&swap_child).unwrap().0 = j;
            old_native_children.swap(i, j);
        }
    }

    if new_native_children.len() < old_native_children.len() {
        vdom.renderer
            .truncate_children(parent_handle, new_native_children.len());
    }

    for native_id in set_native_parent {
        vdom.children
            .get_mut(&native_id)
            .unwrap()
            .native_component
            .as_mut()
            .unwrap()
            .native_parent = Some(parent_id);
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
    vdom: ManuallyDrop<Shared<VDom>>,
    scheduler: Shared<dyn Scheduler>,
}

impl Root {
    /// Creates a new UI tree rooted at `native_handle`. That handle will be used
    /// in order to allow rooting an avalanche tree upon
    /// an existing UI component created externally. Renders `child` as the child of `native_handle`.
    ///
    /// Removes all current children of `native_handle`. After calling `new`, only avalanche should
    /// manipulate or insert descendents of `native_handle` until and if the created instance of `Root`'s
    /// `unmount` method is called. Modifying those children before `unmount` is called will likely result
    /// in panics.
    pub fn new<'a, R: Renderer + 'static, S: Scheduler + 'static, C: DefaultComponent>(
        mut native_handle: NativeHandle,
        mut renderer: R,
        scheduler: S,
    ) -> Self {
        // Remove all the children of `native_handle`.
        renderer.truncate_children( &mut native_handle, 0);

        let mut curr_component_id = ComponentId::new();
        let root_component_id = curr_component_id.create_next();
        let mut children = FxHashMap::with_capacity_and_hasher(16, Default::default());
        let vnode = VNode {
            body_parent: None,
            body_children: FxHashMap::with_capacity_and_hasher(1, Default::default()),
            children: vec![Some(ComponentId {
                id: NonZeroU64::new(2).unwrap(),
            })],
            native_component: Some(NativeComponent {
                native_handle,
                native_parent: None,
                native_children: Vec::new(),
            }),
            state: FxHashMap::default(),
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
            gen: InternalGen::new(),
            update_vdom: render_vdom::<C>,
            bump: Bump::new(),
        };
        let vdom = Shared::new(vdom);
        let vdom_clone = vdom.clone();
        let scheduler: Shared<dyn Scheduler> = Shared::new_dyn(Rc::new(RefCell::new(scheduler)));
        vdom.exec_mut(|vdom| {
            render_vdom::<C>(vdom, &vdom_clone, &scheduler, None);
        });
        Root {
            vdom: ManuallyDrop::new(vdom),
            scheduler,
        }
    }

    /// Unmounts the tree created by `new`, clearing its root native handle of all the
    /// children created by avalanche. This method also drops all the state of the tree.
    pub fn unmount(self) {
        let vdom = ManuallyDrop::into_inner(self.vdom);
        let is_vdom_borrowed = vdom.borrowed();
        let exec_unmount = move || {
            vdom.exec_mut(|vdom| {
                let root_vnode = vdom.children.get_mut(&ComponentId::new());
                let native_root = root_vnode
                    .and_then(|node| node.native_component.as_mut())
                    .unwrap();

                // Clear children
                vdom.renderer.truncate_children(
                    &mut native_root.native_handle,
                    0,
                );
            });
        };
        if is_vdom_borrowed {
            self.scheduler.exec_mut(|scheduler| {
                scheduler.schedule_on_ui_thread(Box::new(exec_unmount));
            })
        } else {
            exec_unmount();
        }
    }
}

fn render_vdom<'a, C: DefaultComponent>(
    vdom: &mut VDom,
    shared_vdom: &Shared<VDom>,
    scheduler: &Shared<dyn Scheduler>,
    current_native_event: Option<(NativeEvent, ComponentId)>,
) {
    // Extract bump from vdom so it can be borrowed without preventing vdom from being mutably borrowed
    let mut bump = Bump::new();
    swap(&mut vdom.bump, &mut bump);

    let components_to_remove = CellBumpVec::new_in(&bump);
    
    let shared_context = SharedContext {
            scheduler,
            current_native_event: &Cell::new(current_native_event),
            components_to_remove: &components_to_remove,
            key: &Cell::new(None),
    };

    render_child(
        C::new(&bump),
        &RenderContext {
            vdom: &Shared::new(vdom),
            body_parent_id: ComponentId::new(),
            component_pos: ComponentPos {
                component_id: ComponentId::new(),
                vdom: shared_vdom,
            },
            bump: &bump,
            shared: &shared_context,
        },
    );

    // TODO: code duplicated from render_child; factor out into function?
    let new_children: Vec<Option<ComponentId>> =
        vdom.children[&ComponentId::new()].children.clone();
    let new_native_children: BumpVec<_> = new_children
        .iter()
        .filter_map(|id| id.and_then(|id| vdom.children[&id].view.native_component_id))
        .collect_in(&bump);
    let vnode = &vdom.children[&ComponentId::new()];
    let native_component = vnode.native_component.as_ref().unwrap();
    let mut old_native_children = native_component
        .native_children
        .iter()
        .cloned()
        .collect_in(&bump);

    update_native_children(
        ComponentId::new(),
        &mut old_native_children,
        &new_native_children,
        vdom,
        &bump,
    );

    drop(old_native_children);

    // Remove the vnodes marked for deletion
    remove_node(vdom, components_to_remove.into_inner());

    let vnode = vdom.children.get_mut(&ComponentId::new()).unwrap();
    let native_component = vnode.native_component.as_mut().unwrap();

    native_component.native_children.clear();
    native_component.native_children.extend(new_native_children);
    vdom.gen.inc();

    // Reset bump allocations, then restore allocator to vdom
    bump.reset();
    swap(&mut vdom.bump, &mut bump);
}

/// A crate allowing bump-allocated data accumulation with only an immutable reference.
pub(crate) struct CellBumpVec<'a, T>(Cell<BumpVec<'a, T>>);

impl<'a, T> CellBumpVec<'a, T> {
    fn new_in(bump: &'a Bump) -> Self {
        Self(Cell::new(BumpVec::new_in(bump)))
    }

    fn push(&self, elem: T, bump: &'a Bump) {
        let mut inner = self.0.replace(BumpVec::new_in(bump));
        inner.push(elem);
        self.0.set(inner);
    }

    fn into_inner(self) -> BumpVec<'a, T> {
        self.0.into_inner()
    }
}
