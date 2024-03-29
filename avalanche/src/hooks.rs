use std::{cell::Cell, marker::PhantomData, panic::Location, fmt::{Display, Write}};

use crate::{
    renderer::{NativeEvent, Scheduler},
    shared::{Shared, WeakShared},
    tracked::{Gen, InternalGen},
    alloc::{Bump, String as BumpString},
    vdom::{
        state::{ComponentStateAccess, SharedBox},
        data::{CellBumpVec, ComponentId, VDom}
    },
    ComponentPos, Tracked, View,
};

/// Shared context between HookContext and RenderContext.
/// The data here remains unchanged during rendering the VDom.
/// It is stored here to prevent needing recopying and stack space
/// during every new render.
pub(crate) struct SharedContext<'a, 'bump: 'a> {
    pub(crate) scheduler: &'a Shared<dyn Scheduler>,
    pub(crate) key: &'a Cell<Option<&'bump str>>,
    pub(crate) current_native_event: &'a Cell<Option<(NativeEvent, ComponentId)>>,
    /// components that need to be removed from the vdom at the end of a UI update iteration
    pub(crate) components_to_remove: &'a CellBumpVec<'bump, ComponentId>,
}

/// Provides a hook with component-specific state.
///
/// Accessed by passing `self` as the first parameter in a hook call.
#[derive(Copy, Clone)]
pub struct HookContext<'a, 'bump: 'a> {
    pub gen: Gen<'a>,
    pub(crate) state: &'a Shared<ComponentStateAccess<'a>>,
    pub(crate) component_pos: ComponentPos<'a>,
    pub(crate) shared: &'a SharedContext<'a, 'bump>,
    pub(crate) bump: &'bump Bump,
}

/// Provides a component with component-specific state.
#[derive(Copy, Clone)]
pub struct RenderContext<'a, 'bump: 'a> {
    pub(crate) vdom: &'a Shared<&'a mut VDom>,
    /// VNode of parent.
    pub(crate) body_parent_id: ComponentId,
    pub(crate) component_pos: ComponentPos<'a>,
    pub bump: &'bump Bump,
    pub(crate) shared: &'a SharedContext<'a, 'bump>,
}

/// Stores some state and its setter for `internal_state`.
struct InternalState<T: 'static, S: 'static> {
    val: T,
    gen: InternalGen,
    setter: S,
}

/// Provides common state storage and access for other state hooks.
#[track_caller]
fn internal_state<'a, T: 'static, S: 'static>(
    ctx: HookContext<'a, '_>,
    f: impl FnOnce() -> T,
    setter: S,
) -> (&'a InternalState<T, S>, Location<'static>) {
    let location = Location::caller();
    let state_ref = ctx.state.exec_mut(|state| {
        state.get_or_insert_with(*location, move || {
            SharedBox::new(Box::new(InternalState {
                val: f(),
                gen: ctx.gen.gen,
                setter,
            }))
        })
    });
    (
        state_ref
            .downcast_ref::<InternalState<T, S>>()
            .expect("downcast to internal state"),
        *location,
    )
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
///     Div(self, [
///         H2(self, Text(self, "Counter!")),
///         Button(
///             self,
///             on_click = move |_| set_count.update(|count| *count += 1),
///             Text(self, "+")
///         ),
///         Text(self, tracked!(count).to_string())
///     ])
/// }
/// ```
/// *Adapted from the `avalanche_web`
/// [counter example.](https://github.com/DJankauskas/avalanche/blob/38ec4ccb83f93550c7d444351fa395708505d053/avalanche-web/examples/counter/src/lib.rs)*
#[track_caller]
pub fn state<'a, T: 'static>(
    ctx: HookContext<'a, '_>,
    f: fn() -> T,
) -> (Tracked<&'a T>, &'a StateSetter<T>) {
    let location = Location::caller();
    let setter = StateSetter {
        internal_setter: InternalStateSetter::new(
            ctx.component_pos,
            ctx.shared.scheduler.clone(),
            *location,
        ),
    };
    let (state, _) = internal_state(ctx, f, setter);
    let state_ref = &state.val;
    let tracked_state_ref = Tracked::new(state_ref, state.gen.into());

    (tracked_state_ref, &state.setter)
}

/// Internal state setter implementation for different hooks' setters.
struct InternalStateSetter<T: 'static, S: 'static> {
    vdom: WeakShared<VDom>,
    component_id: ComponentId,
    scheduler: Shared<dyn Scheduler>,
    location: Location<'static>,
    phantom: PhantomData<(T, S)>,
}

impl<T: 'static, S: 'static> Clone for InternalStateSetter<T, S> {
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

impl<T: 'static, S: 'static> InternalStateSetter<T, S> {
    fn new(
        component_pos: ComponentPos,
        scheduler: Shared<dyn Scheduler>,
        location: Location<'static>,
    ) -> Self {
        Self {
            vdom: component_pos.vdom.downgrade(),
            component_id: component_pos.component_id,
            scheduler,
            location,
            phantom: PhantomData,
        }
    }

    /// Same as `update`, but also provides the `Gen` the root is on before the state update completes
    fn update_with_gen<F: FnOnce(&mut T, Gen) + 'static>(&self, f: F) {
        let vdom_clone = match self.vdom.upgrade() {
            Some(vdom) => vdom,
            None => {
                // TODO: warn for leaking setter on destroyed tree
                return;
            }
        };
        let vdom_clone_2 = vdom_clone.clone();
        let scheduler_clone = self.scheduler.clone();
        let location_copy = self.location;
        let component_id_copy = self.component_id;

        self.scheduler.exec_mut(move |scheduler| {
            scheduler.schedule_on_ui_thread(Box::new(move || {
                vdom_clone.exec_mut(|vdom| {
                    let vdom_gen = vdom.gen;
                    let vnode = match vdom.children.get_mut(&component_id_copy) {
                        Some(vnode) => vnode,
                        None => {
                            // TODO: emit warning
                            return;
                        }
                    };
                    let shared_box = vnode
                        .state
                        .get_mut(&location_copy)
                        .expect("state at hook location");
                    let any_mut = shared_box.get_mut();
                    let state = any_mut
                        .downcast_mut::<InternalState<T, S>>()
                        .expect("state with setter's type");
                    f(&mut state.val, vdom_gen.into());
                    state.gen = vdom_gen;

                    vdom.mark_node_dirty(component_id_copy);
                    (vdom.update_vdom)(vdom, &vdom_clone_2, &scheduler_clone, None);
                })
            }));
        });
    }

    /// Internal implementation of `StateSetter`'s set.
    pub fn set(&self, val: T) {
        self.update_with_gen(move |state, _| *state = val);
    }
}

/// Provides a setter for a piece of state managed by [state].
pub struct StateSetter<T: 'static> {
    internal_setter: InternalStateSetter<T, Self>,
}

impl<T> StateSetter<T> {
    /// Takes a function that modifies the state associated with the setter and
    /// triggers a rerender of its associated component.
    ///
    /// The update is not performed immediately; its effect will only be accessible
    /// on its component's rerender. Note that `update` always triggers a rerender, and the state value
    /// is marked as updated, even if the given function performs no mutations.
    #[inline]
    pub fn update<F: FnOnce(&mut T) + 'static>(&self, f: F) {
        self.internal_setter.update_with_gen(|val, _| f(val))
    }
    ///
    /// Sets the state to the given value.
    ///
    /// The update is not performed immediately; its effect will only be accessible
    /// on its component's rerender. Note that `set` always triggers a rerender, and the state value
    /// is marked as updated, even if the new state is equal to the old.
    #[inline]
    pub fn set(&self, val: T) {
        self.internal_setter.set(val);
    }
}

impl<T> Clone for StateSetter<T> {
    fn clone(&self) -> Self {
        Self {
            internal_setter: self.internal_setter.clone(),
        }
    }
}

/// Like [state], but enables fine-grained reactivity by allowing the storage of
/// nested [Tracked] values that can be created and updated by the provided [Gen] values.
///
/// Takes in a function `F` that accepts a [Gen] for initalization of tracked values. Like [state], returns a tracked reference to
/// `T` and a [StoreSetter], which is like a [StateSetter] except the `update` function's callback is also provided a `Gen` for
/// [Tracked] updates.
///
/// ## Example
/// ```rust
/// use avalanche::{component, tracked, Tracked, View, store, keyed};
/// use avalanche_web::components::{Div, H2, Button, Text};
///
/// #[component]
/// fn DynamicChildren() -> View {
///     let (data, update_data) = store(self, |gen| vec![Tracked::new("child 1", gen)]);
///     let children = tracked!(data)
///         .iter()
///         .enumerate()
///         .map(|(n, text)| keyed(self, n, || Text(
///                             self,
///                             tracked!(text)
///                         ))
///         );
///
///     Div(self, [
///         Button(
///             self,
///             on_click = move |_| update_data.update(|data, gen| data.push(Tracked::new("another child", gen))),
///             Text(self, "+")
///         ),
///         Div(self, tracked!(children))
///     ])
/// }
/// ```
#[track_caller]
pub fn store<'a, T: 'static>(
    ctx: HookContext<'a, '_>,
    f: fn(Gen) -> T,
) -> (Tracked<&'a T>, &'a StoreSetter<T>) {
    let setter = StoreSetter {
        setter: InternalStateSetter::new(
            ctx.component_pos,
            ctx.shared.scheduler.clone(),
            *Location::caller(),
        ),
    };
    let (state, _) = internal_state(ctx, move || f(ctx.gen), setter);
    let state_ref = &state.val;
    let tracked_state_ref = Tracked::new(state_ref, state.gen.into());

    (tracked_state_ref, &state.setter)
}

/// Provides a setter for a piece of state managed by [store].
pub struct StoreSetter<T: 'static> {
    setter: InternalStateSetter<T, Self>,
}

impl<T> StoreSetter<T> {
    /// Analogous to [StateSetter]'s `set` method.
    #[inline]
    pub fn set(&self, val: T) {
        self.setter.set(val)
    }

    /// Like [StateSetter]'s `update` method, but also passes a second `Gen` parameter for
    /// passing to [Tracked] methods.
    #[inline]
    pub fn update<F: FnOnce(&mut T, Gen) + 'static>(&self, f: F) {
        self.setter.update_with_gen(f);
    }
}

impl<T> Clone for StoreSetter<T> {
    fn clone(&self) -> Self {
        Self {
            setter: self.setter.clone(),
        }
    }
}

/// Disambiguates dynamically created components by providing them keys.
///
/// For performance and correctness reasons, components must have unique identities.
/// Components by default have unique identities based on their location in source code,
/// but if they are called more than once in the same location, they will be treated as the same component.
/// This is a problem if you want to have multiple instances of the same component in the same view.
/// To solve this, you can use the `keyed` hook to provide a unique key for each instance of the component.
pub fn keyed<K: Display>(ctx: HookContext, key: K, render: impl FnOnce() -> View) -> View {
    let mut bump_key = BumpString::new_in(ctx.bump);
    write!(bump_key, "{key}").expect("a Display implementation returned an error unexpectedly");
    let key = ctx.shared.key.replace(Some(bump_key.into_bump_str()));
    let view = render();
    ctx.shared.key.replace(key);
    view
}
