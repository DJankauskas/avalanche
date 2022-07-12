#[doc(inline)]
pub use crate::vdom::Root;
use crate::{
    any_ref::DynRef,
    shared::{Shared, WeakShared},
    tracked::Gen,
    vdom::{mark_node_dirty, ComponentId, VDom},
};
use std::any::Any;

/// An opaque handle whose underlying type is determined by the current `Renderer`.
pub type NativeHandle = Box<dyn Any>;

/// The interface through which `avalanche` updates the native UI as described by changes to components.
/// This allows `avalanche` to be platform-agnostic.
pub trait Renderer {
    /// Given a component and its native type, generate its native representation and return
    /// an opaque `NativeHandle` to it.
    fn create_component(
        &mut self,
        native_type: &NativeType,
        component: DynRef,
        dispatch_native_event: DispatchNativeEvent,
    ) -> NativeHandle;

    /// Appends the component with handle `child_handle` into the component with
    /// handle `parent_handle`'s children.
    fn append_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        child_type: &NativeType,
        child_handle: &NativeHandle,
    );

    /// Inserts the component with handle `child_handle` into the component with
    /// handle `parent_handle`'s children at the given `index`, shifting all
    /// children after it to the right.
    /// # Panics
    /// Panics if `index > len`, where `len` is the number of children the parent has.
    fn insert_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        index: usize,
        child_type: &NativeType,
        child_handle: &NativeHandle,
    );

    /// Replaces the component at position `index` with the native component
    /// with handle `child_handle`.
    /// The implementation should implement replacing a component with itself as a noop.
    /// # Panics
    /// Panics if `index > len - 1`, where `len` is the number of children the parent has.
    fn replace_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        index: usize,
        child_type: &NativeType,
        child_handle: &NativeHandle,
    );

    /// Swaps the children at indices `a` and `b`.
    /// # Panics
    /// Panics if `a` or `b` are less than `len`, where `len` is the number of children the parent has.
    fn swap_children(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        a: usize,
        b: usize,
    );

    /// Truncates the number of the children to the length given, removing any children
    /// greater in count than `len`.
    fn truncate_children(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        len: usize,
    );

    /// Updates the `component`'s corresponding native handle so that the representation
    /// and result match. The method may be provided an event that is dispatched
    /// to the component via a `Root` method.
    fn update_component(
        &mut self,
        native_type: &NativeType,
        native_handle: &mut NativeHandle,
        component: DynRef,
        curr_gen: Gen,
        event: Option<NativeEvent>,
    );

    /// Logs the given string to a platform-appropriate destination.
    /// This method is a placeholder, and may either be elaborated or replaced with
    /// the `log` crate
    fn log(&self, _string: &str) {}
}

/// An interface to schedule a function on a platform's ui thread.
pub trait Scheduler {
    /// Schedule the given function to be run on the ui thread in the future.
    fn schedule_on_ui_thread(&mut self, f: Box<dyn FnOnce()>);
}

/// Describes the native element type a given [Component](crate::Component) corresponds to.
/// The meaning of its fields is up to a particular renderer implementation to define.
#[derive(Copy, Clone)]
pub struct NativeType {
    pub handler: &'static str,
    pub name: &'static str,
}

/// The event data and type of an event dispatched to a native component.
pub struct NativeEvent {
    /// Data of the event being dispatched to a component.
    pub event: Box<dyn Any>,
    /// Name of the event being dispatched.
    pub name: &'static str,
}

#[derive(Clone)]
/// Allow native components to set up dispatching events to themselves.
pub struct DispatchNativeEvent {
    /// The component to which the native event should be dispatched.
    pub(crate) component_id: ComponentId,
    /// The ui tree to which the event should be dispatched.
    pub(crate) vdom: WeakShared<VDom>,
    /// The scheduler needed for hook rerendering.
    pub(crate) scheduler: Shared<dyn Scheduler>,
}

impl DispatchNativeEvent {
    /// Triggers a rerender of the component tree within this call,
    /// passing the event to the proper native component.
    pub fn dispatch(&self, native_event: NativeEvent) {
        let self_clone = self.clone();
        let vdom = match self.vdom.upgrade() {
            Some(vdom) => vdom,
            None => {
                // TODO warn of operation on deleted tree
                return;
            }
        };
        let vdom_clone = vdom.clone();
        let exec_event = move || {
            let vdom_clone2 = vdom_clone.clone();
            vdom_clone.exec_mut(move |vdom| {
                mark_node_dirty(vdom, self_clone.component_id);
                (vdom.update_vdom)(
                    vdom,
                    &vdom_clone2,
                    &self_clone.scheduler,
                    Some((native_event, self_clone.component_id)),
                );
            })
        };
        if vdom.borrowed() {
            self.scheduler
                .exec_mut(|scheduler| scheduler.schedule_on_ui_thread(Box::new(exec_event)));
        } else {
            exec_event();
        }
    }
}
