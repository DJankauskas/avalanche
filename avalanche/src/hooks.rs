use std::{any::Any, marker::PhantomData, panic::Location};

use crate::{
    renderer::Scheduler,
    shared::Shared,
    tracked,
    vdom::{
        mark_node_dirty,
        wrappers::{ComponentStateAccess, SharedBox},
        ComponentId, VDom, VNode,
    },
    ComponentPos, Tracked,
};

#[derive(Copy, Clone, PartialEq, Debug)]
pub(crate) struct Gen {
    pub(crate) gen: u32,
}

// TODO: support wrap edge cases
impl Gen {
    pub(crate) fn new() -> Self {
        Gen { gen: 0 }
    }
    pub(crate) fn next(mut self) -> Self {
        self.inc();
        Gen { gen: self.gen }
    }

    pub(crate) fn inc(&mut self) {
        self.gen = self.gen.wrapping_add(1);
    }

    pub(crate) fn updated(self, current_gen: Gen) -> bool {
        self.gen >= current_gen.gen
    }
}

/// Provides a hook with component-specific state.
///
/// Accessed by passing `self` as the first parameter in a hook call.
#[derive(Copy, Clone)]
pub struct HookContext<'a> {
    pub(crate) gen: Gen,
    pub(crate) state: &'a Shared<ComponentStateAccess<'a>>,
    pub(crate) component_pos: ComponentPos<'a>,
    pub(crate) scheduler: &'a Shared<dyn Scheduler>,
}

/// Provides a hook with component-specific state.
///
/// Accessed by passing `self` as the first parameter in a hook call.
pub struct RenderContext<'a> {
    pub(crate) vdom: &'a mut VDom,
    // vnode of parent
    pub(crate) vnode: &'a mut VNode,
    pub(crate) component_pos: ComponentPos<'a>,
    pub(crate) scheduler: &'a Shared<dyn Scheduler>,
}

struct State<T: 'static> {
    val: T,
    gen: Gen,
}

#[track_caller]
fn internal_state<'a, T: 'static>(
    ctx: HookContext<'a>,
    f: impl FnOnce() -> T,
) -> (&'a dyn Any, Location<'static>) {
    let location = Location::caller();
    let state_ref = ctx.state.exec_mut(|state| {
        state.get_or_insert_with(*location, move || {
            SharedBox::new(Box::new(State {
                val: f(),
                gen: ctx.gen.next(),
            }))
        })
    });
    (state_ref, *location)
}

/// A hook that allows a component to keep persistent state across renders.
///
/// `state` takes a type parameter `T` specifying the type of the state variable the hook manages, as well as
/// a parameter `F` specifying the type of a function providing a default value for `T`.
/// On first call, the given state will be initialized with `f`; it will not be called on subsequent calls of the same
/// `state` call site.
/// The return value contains a tracked reference to the current state,
/// and the setter [StateSetter](StateSetter). `&T`'s lifetime is only valid within the component's render
/// function, but [StateSetter](StateSetter) may be freely moved and cloned.
///
/// To update the state, use the [set](StateSetter::set) or [update](StateSetter::update) methods on the setter variable.
///
/// # Example
/// ```rust
/// use avalanche::{component, tracked, View, state};
/// use avalanche_web::components::{Div, H2, Button, Text};
///
/// #[component]
/// fn Counter() -> View {
///     let (count, set_count) = state(self, || 0);
///     Div!(
///         children: [
///             H2!(
///                 child: Text!("Counter!"),
///             ),
///             Button!(
///                 on_click: move |_| set_count.update(|count| *count += 1),
///                 child: Text!("+")
///             ),
///             Text!(tracked!(count).to_string())
///         ]
///     )
/// }
/// ```
/// *Adapted from the `avalanche_web`
/// [counter example.](https://github.com/DJankauskas/avalanche/blob/38ec4ccb83f93550c7d444351fa395708505d053/avalanche-web/examples/counter/src/lib.rs)*
#[track_caller]
pub fn state<'a, T: 'static>(
    ctx: HookContext<'a>,
    f: fn() -> T,
) -> (Tracked<&'a T>, StateSetter<T>) {
    let (state, location) = internal_state(ctx, f);
    let state = state.downcast_ref::<State<T>>().unwrap();
    let updated = state.gen.updated(ctx.gen);
    let state_ref = &state.val;
    let tracked_state_ref = Tracked::new(state_ref, updated);
    let updater = StateSetter::new(ctx.component_pos, ctx.scheduler.clone(), location);

    (tracked_state_ref, updater)
}

/// Provides a setter for a piece of state managed by [state].
pub struct StateSetter<T: 'static> {
    vdom: Shared<VDom>,
    component_id: ComponentId,
    scheduler: Shared<dyn Scheduler>,
    location: Location<'static>,
    phantom: PhantomData<T>,
}

impl<T: 'static> Clone for StateSetter<T> {
    fn clone(&self) -> Self {
        Self {
            vdom: self.vdom.clone(),
            component_id: self.component_id,
            scheduler: self.scheduler.clone(),
            location: self.location,
            phantom: PhantomData,
        }
    }
}

impl<T: 'static> StateSetter<T> {
    fn new(
        component_pos: ComponentPos,
        scheduler: Shared<dyn Scheduler>,
        location: Location<'static>,
    ) -> Self {
        Self {
            vdom: component_pos.vdom.clone(),
            component_id: component_pos.component_id,
            scheduler,
            location,
            phantom: PhantomData,
        }
    }

    /// Takes a function that modifies the state associated with the setter and
    /// triggers a rerender of its associated component.
    ///
    /// The update is not performed immediately; its effect will only be accessible
    /// on its component's rerender. Note that `update` always triggers a rerender, and the state value
    /// is marked as updated, even if the given function performs no mutations.
    pub fn update<F: FnOnce(&mut T) + 'static>(&self, f: F) {
        self.update_with_gen(|val, _| f(val))
    }

    /// Same as `update`, but also provides the `Gen` the root is on before the state update completes
    fn update_with_gen<F: FnOnce(&mut T, Gen) + 'static>(&self, f: F) {
        let vdom_clone = self.vdom.clone();
        let vdom_clone_2 = vdom_clone.clone();
        let scheduler_clone = self.scheduler.clone();
        let location_copy = self.location;
        let component_id_copy = self.component_id;

        self.scheduler.exec_mut(move |scheduler| {
            scheduler.schedule_on_ui_thread(Box::new(move || {
                vdom_clone.exec_mut(|vdom| {
                    let vdom_gen = vdom.gen;
                    let vnode = &mut vdom.children.get_mut(&component_id_copy).unwrap();
                    let shared_box = match vnode.state.get_mut(&location_copy) {
                        Some(vnode) => vnode,
                        None => {
                            // TODO: emit warning of leak with callback associated with
                            // removed node
                            return;
                        }
                    };
                    let any_mut = shared_box.get_mut();
                    let state = any_mut
                        .downcast_mut::<State<T>>()
                        .expect("state with setter's type");
                    f(&mut state.val, vdom_gen);
                    state.gen = vdom_gen.next();

                    mark_node_dirty(vdom, component_id_copy);
                    (vdom.update_vdom)(vdom, &vdom_clone_2, &scheduler_clone);

                    vdom.gen.inc();
                })
            }));
        });
    }

    /// Sets the state to the given value.
    ///
    /// The update is not performed immediately; its effect will only be accessible
    /// on its component's rerender. Note that `set` always triggers a rerender, and the state value
    /// is marked as updated, even if the new state is equal to the old.
    pub fn set(&self, val: T) {
        self.update(move |state| *state = val);
    }
}

/// Like [state], but returns a reference to a [tracked::Vec].
///
/// Takes in a function `F` that returns
/// a default [Vec](std::vec::Vec). The return value has fine-grained tracking: instead of only the
/// whole vec being updated or not updated, each individual element is also tracked. For more information on how that works see
/// [tracked::Vec].
///
/// Note that `vec` returns a `Tracked<tracked::Vec>`, which is marked as updated if
/// any of its children are updated. However, an individual element's update status overrides this where appropriate.
///
/// ## Example
/// ```rust
/// use avalanche::{component, tracked, View, vec};
/// use avalanche_web::components::{Div, H2, Button, Text};
///
/// #[component]
/// fn DynamicChildren() -> View {
///     let (data, update_data) = vec(self, || vec!["child 1"]);
///     let children = tracked!(data)
///         .iter()
///         .enumerate()
///         .map(|(n, text)| Text!(key: n.to_string(), *tracked!(text))).collect::<Vec<_>>();
///
///     Div!([
///         Button!(
///             on_click: move |_| update_data.update(|data| data.push("another child")),
///             child: Text!("+")
///         ),
///         Div!(tracked!(children))
///     ])
/// }
/// ```
#[track_caller]
pub fn vec<'a, T: 'static>(
    ctx: HookContext<'a>,
    f: fn() -> Vec<T>,
) -> (Tracked<&'a tracked::Vec<T>>, VecSetter<T>) {
    let (state, location) = internal_state(ctx, || tracked::Vec::new(f(), ctx.gen));
    let state = state.downcast_ref::<State<tracked::Vec<T>>>().unwrap();
    state.val.curr_gen.set(ctx.gen);
    let updated = state.gen.updated(ctx.gen);
    let state_ref = &state.val;
    let tracked_state_ref = Tracked::new(state_ref, updated);
    let updater = VecSetter::new(ctx.component_pos, ctx.scheduler.clone(), location);

    (tracked_state_ref, updater)
}

/// Provides a setter for a piece of state managed by [vec](vec()).
pub struct VecSetter<T: 'static> {
    setter: StateSetter<tracked::Vec<T>>,
}

impl<T> VecSetter<T> {
    fn new(
        component_pos: ComponentPos,
        scheduler: Shared<dyn Scheduler>,
        location: Location<'static>,
    ) -> Self {
        Self {
            setter: StateSetter::new(component_pos, scheduler, location),
        }
    }

    /// Takes a function that modifies the vec associated with the setter and
    /// triggers a rerender of its associated component.
    ///
    /// The update is not performed immediately; its effect will only be accessible
    /// on its component's rerender. Note that `update` always triggers a rerender, and the state value
    /// is marked as updated, even if the given function performs no mutations.
    pub fn update<F: FnOnce(&mut tracked::Vec<T>) + 'static>(&self, f: F) {
        self.setter.update_with_gen(|val, gen| {
            val.curr_gen.set(gen);
            f(val);
        });
    }

    /// Sets the vec to the given value, marking all elements as updated.
    ///
    /// The update is not performed immediately; its effect will only be accessible
    /// on its component's rerender. Note that `set` always triggers a rerender, and the state value
    /// is marked as updated, even if the new state is equal to the old.
    pub fn set(&self, val: Vec<T>) {
        self.update(|vec| *vec = tracked::Vec::new(val, vec.curr_gen.get()))
    }
}

impl<T> Clone for VecSetter<T> {
    fn clone(&self) -> Self {
        Self {
            setter: self.setter.clone(),
        }
    }
}
