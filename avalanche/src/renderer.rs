use std::any::Any;

use crate::hooks::Context;
use crate::{Component, View};

/// An opaque handle whose underlying type is determined by the current `Renderer`.
pub type NativeHandle = Box<dyn Any>;

/// The interface through which `avalanche` updates the native UI as described by changes to components.
/// This allows `avalanche` to be platform-agnostic.
pub trait Renderer {
    /// Given a component and its native type, generate its native representation and return
    /// an opaque `NativeHandle` to it.
    fn create_component(&mut self, native_type: &NativeType, component: &View) -> NativeHandle;

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

    /// Moves the child at position `old` to the position `new`.
    /// # Panics
    /// Panics if `old < len` or `new < len` where `len` is the number of children
    fn move_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        old: usize,
        new: usize,
    );

    /// Removes the component with the given `index` from the component
    /// with handle `parent_handle`.
    fn remove_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        index: usize,
    );

    /// Updates the `component`'s corresponding native handle so that the representation
    /// and result match.
    fn update_component(
        &mut self,
        native_type: &NativeType,
        native_handle: &mut NativeHandle,
        component: &View,
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

/// Describes the native element type a given [`Component`] corresponds to.
/// The meaning of its fields is up to a particular renderer implementation to define.
#[derive(Copy, Clone)]
pub struct NativeType {
    pub handler: &'static str,
    pub name: &'static str,
}

/// A component for native children to render avalanche components.
#[derive(Clone, Default)]
pub struct HasChildrenMarker {
    pub children: Vec<View>,
}

impl Component for HasChildrenMarker {
    // TODO: make ! when never stabilizes
    type Builder = ();
    fn render(&self, _: Context) -> View {
        unreachable!()
    }
    fn updated(&self) -> bool {
        unimplemented!()
    }
}

#[doc(inline)]
pub use crate::vdom::Root;
