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
        children: &mut dyn Iterator<Item=Child>
    ) -> Option<NativeHandle>;
    
    //TODO: make native_handle take &mut?
    fn update_component(
        &mut self, 
        native_type: &NativeType, 
        native_handle: &mut NativeHandle, 
        component: &View,
        children: &mut dyn Iterator<Item=Child>
    );

    fn remove_component(&mut self, vnode: &mut VNode);

    fn schedule_on_ui_thread(&mut self, f: Box<dyn FnOnce()>);
}
#[non_exhaustive]
pub struct Child<'a> {
    pub component: &'a View,
    pub native_handle: Option<&'a NativeHandle> 
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