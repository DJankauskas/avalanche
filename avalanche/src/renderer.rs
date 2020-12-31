use std::any::Any;

use crate::{Component, View};
use crate::vdom::{VNode};
use crate::{InternalContext};

///An opaque handle whose underlying type is determined by the current `Renderer`.
pub type NativeHandle = Box<dyn Any>;

pub trait Renderer {
    fn create_component(
        &mut self, 
        native_type: &NativeType, 
        component: &View,
    ) -> NativeHandle;

    /// Appends the component with handle `child_handle` into the component with
    /// handle `parent_handle`'s children.
    fn append_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle, 
        child_type: &NativeType,
        child_handle: &NativeHandle
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
        child_handle: &NativeHandle
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
        child_handle: &NativeHandle
    );

    /// Swaps the children at indices `a` and `b`.
    /// # Panics
    /// Panics if `a` or `b` are less than `len`, where `len` is the number of children the parent has.
    fn swap_children(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        a: usize,
        b: usize
    );

    /// Moves the child at position `old` to the position `new`.
    /// # Panics
    /// Panics if `old < len` or `new < len` where `len` is the number of children
    fn move_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        old: usize,
        new: usize
    );

    /// Removes the component with the given `index` from the component
    /// with handle `parent_handle`.
    fn remove_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        index: usize
    );
    
    fn update_component(
        &mut self, 
        native_type: &NativeType, 
        native_handle: &mut NativeHandle, 
        component: &View,
    );

    fn remove_component(&mut self, vnode: &mut VNode);

    fn schedule_on_ui_thread(&mut self, f: Box<dyn FnOnce()>);

    //TODO: remove or elaborate?
    fn log(&self, string: &str);
}

pub struct NativeType {
    pub handler: &'static str,
    pub name: &'static str,
}

#[derive(Clone, Default)]
pub struct HasChildrenMarker {
    pub children: Vec<View>
}

impl Component for HasChildrenMarker {
    //TODO: make ! when never stabilizes
    type Builder = ();
    fn render(&self, _: InternalContext) -> View {
        unreachable!()
    }
    fn updated(&self) -> bool {
        todo!()
    }
}