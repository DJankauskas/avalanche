use std::{any::Any, marker::PhantomData, panic::Location};

use crate::{
    renderer::Scheduler,
    shared::Shared,
    tracked,
    tree::NodeId,
    vdom::{
        update_vnode,
        wrappers::{ComponentStateAccess, SharedBox},
        VDom, VNode,
    },
    ComponentPos, Tracked,
};

#[derive(Copy, Clone, PartialEq, Debug)]
pub(crate) struct Gen {
    pub(crate) gen: u32,
}

// TODO: support wrap edge cases
impl Gen {
    pub(crate) fn new(mut self) -> Self {
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

#[derive(Clone, Copy)]
pub struct Context<'a> {
    pub(crate) gen: Gen,
    pub(crate) state: &'a Shared<ComponentStateAccess<'a>>,
    pub(crate) component_pos: ComponentPos<'a>,
    pub(crate) scheduler: &'a Shared<dyn Scheduler>,
}

struct State<T: 'static> {
    val: T,
    gen: Gen,
}

pub type Ref<'a, T> = std::cell::Ref<'a, T>;

#[track_caller]
fn internal_state<'a, T: 'static>(
    ctx: Context<'a>,
    f: impl FnOnce() -> T,
) -> (&'a dyn Any, Location<'static>) {
    let location = Location::caller();
    let ctx_gen = ctx.gen;
    let state_ref = ctx.state.exec_mut(|state| {
        state.get_or_insert_with(*location, move || {
            SharedBox::new(Box::new(State {
                val: f(),
                // EDITION 2021: use ctx.gen
                gen: ctx_gen.new(),
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
/// The return value contains atracked reference to the current state,
/// and the setter [StateSetter<T>](StateSetter). `&T`'s lifetime is only valid within the component's render
/// function, but [StateSetter<T>](StateSetter) may be freely moved and cloned.
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
///             Text!(tracked!(count))
///         ]
///     )
/// }
/// ```
/// *Adapted from the `avalanche_web`
/// [counter example.](https://github.com/DJankauskas/avalanche/blob/38ec4ccb83f93550c7d444351fa395708505d053/avalanche-web/examples/counter/src/lib.rs)*
#[track_caller]
pub fn state<'a, T: 'static, F: FnOnce() -> T>(
    ctx: Context<'a>,
    f: F,
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
    vnode: NodeId<VNode>,
    scheduler: Shared<dyn Scheduler>,
    location: Location<'static>,
    phantom: PhantomData<T>,
}

impl<T: 'static> Clone for StateSetter<T> {
    fn clone(&self) -> Self {
        Self {
            vdom: self.vdom.clone(),
            vnode: self.vnode,
            scheduler: self.scheduler.clone(),
            location: self.location.clone(),
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
            vnode: component_pos.node_id.id,
            scheduler,
            location,
            phantom: PhantomData,
        }
    }

    /// Takes a function that modifies the state associated with [StateSetter] and
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
        let vnode_copy = self.vnode;
        let scheduler_clone = self.scheduler.clone();
        let location_copy = self.location;

        self.scheduler.exec_mut(move |scheduler| {
            scheduler.schedule_on_ui_thread(Box::new(move || {
                vdom_clone.exec_mut(|vdom| {
                    let vdom_gen = vdom.gen;
                    let vnode = vnode_copy.get_mut(&mut vdom.tree);
                    vnode.dirty = true;
                    let shared_box = vnode
                        .state
                        .get_mut(&location_copy)
                        .expect("state referenced by correct location");
                    let any_mut = shared_box.get_mut();
                    let state = any_mut
                        .downcast_mut::<State<T>>()
                        .expect("state with setter's type");
                    f(&mut state.val, vdom_gen);
                    state.gen = vdom_gen.new();
                    std::mem::drop(any_mut);
                    update_vnode(
                        None,
                        vnode_copy,
                        &mut vdom.tree,
                        &mut vdom.renderer,
                        &vdom_clone_2,
                        &scheduler_clone,
                        vdom_gen,
                    );
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

#[track_caller]
pub fn vec<'a, T: 'static, F: FnOnce() -> Vec<T>>(
    ctx: Context<'a>,
    f: F,
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

    pub fn update<F: FnOnce(&mut tracked::Vec<T>) + 'static>(&self, f: F) {
        self.setter.update_with_gen(|val, gen| {
            val.curr_gen.set(gen);
            f(val);
        });
    }

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