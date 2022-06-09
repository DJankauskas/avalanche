use std::rc::Rc;

use crate::{
    renderer::{NativeEvent, Renderer},
    shared::Shared,
};

use super::native_mock::{Native, Node, Root};

/// Enables avalanche to manipulate `Node`s.
pub(super) struct TestRenderer {
    root: Shared<Root>,
}

impl TestRenderer {
    pub fn new(root: Shared<Root>) -> Self {
        Self { root }
    }
}

impl Renderer for TestRenderer {
    fn create_component(
        &mut self,
        _native_type: &crate::renderer::NativeType,
        component: crate::any_ref::DynRef,
        dispatch_native_event: crate::renderer::DispatchNativeEvent,
    ) -> crate::renderer::NativeHandle {
        let component = component.downcast_ref::<Native>().unwrap();
        let node = self.root.exec_mut(|root| root.create_node(component.name));
        node.set_value(component.value.to_string());
        if component.on_click.is_some() {
            node.set_on_click(Rc::new(move || {
                dispatch_native_event.dispatch(NativeEvent {
                    event: Box::new(()),
                    name: "click",
                })
            }));
        };
        Box::new(node)
    }

    fn append_child(
        &mut self,
        _parent_type: &crate::renderer::NativeType,
        parent_handle: &mut crate::renderer::NativeHandle,
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
        parent_handle: &mut crate::renderer::NativeHandle,
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
        _parent_handle: &mut crate::renderer::NativeHandle,
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
        parent_handle: &mut crate::renderer::NativeHandle,
        a: usize,
        b: usize,
    ) {
        let parent_handle = parent_handle.downcast_ref::<Node>().unwrap();
        parent_handle.swap_children(a, b);
    }

    fn truncate_children(
        &mut self,
        _parent_type: &crate::renderer::NativeType,
        parent_handle: &mut crate::renderer::NativeHandle,
        len: usize,
    ) {
        let parent_handle = parent_handle.downcast_ref::<Node>().unwrap();
        while parent_handle.children_len() > len {
            parent_handle.remove_child(len);
        }
    }

    fn update_component(
        &mut self,
        _native_type: &crate::renderer::NativeType,
        native_handle: &mut crate::renderer::NativeHandle,
        component: crate::any_ref::DynRef,
        event: Option<crate::renderer::NativeEvent>,
    ) {
        let handle = native_handle.downcast_ref::<Node>().unwrap();
        let component = component.downcast_ref::<Native>().unwrap();
        if let (Some(_), Some(on_click)) = (event, &component.on_click) {
            on_click();
        }
        if component.name_updated() {
            panic!("Name must be static");
        }
        if component.value_updated() {
            handle.set_value(component.value.to_string());
        }
    }
}
