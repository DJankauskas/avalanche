use crate::View;
use crate::renderer::{Renderer, NativeHandle, NativeType, HasChildrenMarker};

use crate::{InternalContext, shared::Shared};
use std::any::Any;

pub struct VDom {
    pub root: Option<Shared<VNode>>,
    pub renderer: Box<dyn Renderer>,
}

pub struct VNode {
    pub component: View,
    pub(crate) vdom: Shared<VDom>,
    pub children: Vec<Shared<VNode>>,
    pub native_handle: Option<NativeHandle>,
    pub native_type: Option<NativeType>,
    pub(crate) state: Option<Box<dyn Any>>,
    pub(crate) dirty: bool
}

pub fn generate_vdom(component: View, renderer: Box<dyn Renderer>) -> Shared<VDom> {
    let vdom = VDom {
        root: None,
        renderer
    };
    let mut vdom = Shared::new(vdom);
    let vdom_clone = vdom.clone();
    vdom.exec_mut(|vdom| {
        let vnode = generate_vnode(component, vdom_clone, &mut vdom.renderer);
        vdom.root = Some(vnode);
    });
    vdom
}

//Traverses hierarchy until node with NativeHandle is found.
//Returns None if end of tree has no handle
pub fn node_with_native_handle(mut vnode: Shared<VNode>) -> Option<Shared<VNode>> {
    loop {
        if vnode.exec(|vnode| vnode.native_handle.is_some()) {
            return Some(vnode);
        }
        else if vnode.exec(|vnode| vnode.children.is_empty()) {
            return None;
        }
        vnode = vnode.exec(|vnode| {
            if vnode.children.len() > 1 {
                panic!("Expected non-native Oak component to have 1 child.");
            };
            vnode.children[0].clone()
        });
    };
}

pub fn generate_vnode(component: View, vdom: Shared<VDom>, renderer: &mut Box<dyn Renderer>) -> Shared<VNode> {
    if component.is::<()>() {
        let vnode = VNode {
            component,
            vdom,
            children: Vec::new().into(),
            native_handle: None,
            native_type: None,
            state: None,
            dirty: false
        };
        return Shared::new(vnode);
    };

    let vnode = VNode {
        component,
        vdom: vdom.clone(),
        children: Vec::new(),
        native_handle: None,
        native_type: None,
        state: None,
        dirty: false
    };
    let mut vnode = Shared::new(vnode);
    let vnode_clone = vnode.clone();

    vnode.exec_mut(move |vnode| {
        vnode.state = Some(vnode.component.init_state());

        let context = InternalContext {
            state: vnode.state.as_mut().unwrap(),
            vnode: vnode_clone
        };
        let child = vnode.component.render(context);

        vnode.children = match child.downcast_ref::<HasChildrenMarker>() {
            Some(marker) => {
                marker.children.iter().map(|c| generate_vnode(c.clone(), vdom.clone(), renderer)).collect()
            }
            None => {
                let vnode = generate_vnode(child, vdom.clone(), renderer);
                vec![vnode]
            }
        };

        let native_type = vnode.component.native_type();
        vnode.native_type = native_type;
        
        vnode.native_handle = renderer.create_component(vnode);
    });

    vnode
}

pub fn update_vnode(mut vnode: Shared<VNode>, mut new_component: Option<View>, renderer: &mut Box<dyn Renderer>) {
    let props_updated = match new_component {
        Some(ref other) => other.updates() != 0,
        None => false
    };
    let state_updated = vnode.exec(|vnode| vnode.dirty);

    //if neither props nor state have changed
    //we do not need to rerender
    if !(props_updated || state_updated) {
        return;
    }

    let vnode_clone = vnode.clone();

    vnode.exec_mut(move |vnode| {
        let old_component = match new_component {
            Some(ref mut comp) => {
                std::mem::swap(&mut vnode.component, comp);
                Some(&*comp)
            }
            None => None
        };

        let context = InternalContext {
            state: vnode.state.as_mut().unwrap(),
            vnode: vnode_clone
        };

        let child = vnode.component.render(context);

        let mut children = match child.downcast_ref::<HasChildrenMarker>() {
            Some(marker) => marker.children.clone(),
            None => vec![child],
        };

        //TODO: rewrite this code using a key mechanism when devised
        //potentially have a key() method returning something hashable
        //then put the old components in a HashMap using the key() values as map keys
        //use the keys of the new children to match
        //the implementation below is naive and is only efficient 
        //for single children and concatenation

        //if the number of children has decreased, clear the extra children
        if vnode.children.len() > children.len() {
            for child in vnode.children.drain((vnode.children.len() - children.len())..) {
                destroy_vnode(child, renderer);
            }
        }

        let new_children = children.split_off(vnode.children.len());

        //updates every old node with new contents
        for (old, new) in vnode.children.iter().zip(children.into_iter()) {
            update_vnode(old.clone(), Some(new), renderer);
        }

        for child in new_children.into_iter() {
            let new_vnode = generate_vnode(child, vnode.vdom.clone(), renderer);
            vnode.children.push(new_vnode);
        }

        //if there is no props update, and thus no old_component
        //the component should not require native updating
        match old_component {
            Some(old_component) => {
                //The native_action can only be updated if the type of component
                //has remained the same
                if old_component.type_id() == vnode.component.type_id() {
                    vnode.native_type = vnode.component.native_type();
                    match &vnode.native_type {
                        Some(native_action) => {
                            renderer.update_component(
                                native_action, 
                                vnode.native_handle.as_mut().unwrap(), 
                                old_component, 
                                &vnode.component,
                                &vnode.children
                            );
                        },
                        None => {},
                    }
                }
            }
            None => {}
        }   
    });
}

pub fn destroy_vnode(mut vnode: Shared<VNode>, renderer: &mut Box<dyn Renderer>) {
    vnode.exec_mut(|vnode| {
        //TODO: remove check for is_some()?
        if vnode.native_handle.is_some() {
            renderer.remove_component(vnode);
        }
    });

    //TODO: call recursively on children?
    //perhaps a new component method revealing whether recursive detruction needed
    //currently not needed for web platform
}
