use crate::{renderer::Renderer, shared::Shared};

use super::native_mock::{Node, Root};

/// Enables avalanche to manipulate `Node`s.
pub(super) struct TestRenderer {
    pub root: Shared<Root>,
}

impl TestRenderer {
    pub fn new(root: Shared<Root>) -> Self {
        Self { root }
    }
}

impl Renderer for TestRenderer {
    fn append_child(
        &mut self,
        _parent_type: &crate::renderer::NativeType,
        parent_handle: &crate::renderer::NativeHandle,
        _child_type: &crate::renderer::NativeType,
        child_handle: &crate::renderer::NativeHandle,
    ) {
        let parent_handle = parent_handle.downcast_ref::<Node>().unwrap();
        let child_handle = child_handle.downcast_ref::<Node>().unwrap();
        parent_handle.insert_child(child_handle.clone(), parent_handle.children_len());
    }

    fn insert_child(
        &mut self,
        _parent_type: &crate::renderer::NativeType,
        parent_handle: &crate::renderer::NativeHandle,
        index: usize,
        _child_type: &crate::renderer::NativeType,
        child_handle: &crate::renderer::NativeHandle,
    ) {
        let parent_handle = parent_handle.downcast_ref::<Node>().unwrap();
        let child_handle = child_handle.downcast_ref::<Node>().unwrap();
        parent_handle.insert_child(child_handle.clone(), index);
    }

    fn replace_child(
        &mut self,
        _parent_type: &crate::renderer::NativeType,
        _parent_handle: &crate::renderer::NativeHandle,
        _index: usize,
        _child_type: &crate::renderer::NativeType,
        _child_handle: &crate::renderer::NativeHandle,
    ) {
        // This method is currently unused by avalanche.
        unimplemented!()
    }

    fn swap_children(
        &mut self,
        _parent_type: &crate::renderer::NativeType,
        parent_handle: &crate::renderer::NativeHandle,
        a: usize,
        b: usize,
    ) {
        let parent_handle = parent_handle.downcast_ref::<Node>().unwrap();
        parent_handle.swap_children(a, b);
    }

    fn truncate_children(
        &mut self,
        _parent_type: &crate::renderer::NativeType,
        parent_handle: &crate::renderer::NativeHandle,
        len: usize,
    ) {
        let parent_handle = parent_handle.downcast_ref::<Node>().unwrap();
        while parent_handle.children_len() > len {
            parent_handle.remove_child(len);
        }
    }
}
