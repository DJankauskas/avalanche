use crate::View;
use crate::{
    renderer::Child,
    tree::{NodeId, Tree},
};
use crate::{
    renderer::{HasChildrenMarker, NativeHandle, NativeType, Renderer},
    ComponentPos,
};

use crate::{shared::Shared, InternalContext};
use std::{any::Any, collections::HashMap, hash::Hash};

pub struct VDom {
    pub(crate) tree: Tree<VNode>,
    pub renderer: Box<dyn Renderer>,
}

//TODO: make pub(crate)
pub struct VNode {
    pub component: View,
    pub native_handle: Option<NativeHandle>,
    pub native_type: Option<NativeType>,
    pub(crate) state: Option<Box<dyn Any>>,
    pub(crate) dirty: bool,
}

impl VNode {
    ///default VNode initialized with a component
    ///this VNode should be filled in with [`generate_vnode`]
    fn component(component: View) -> Self {
        Self {
            component,
            native_handle: None,
            native_type: None,
            state: None,
            dirty: false,
        }
    }
}

pub struct Root {
    vdom: Shared<VDom>,
}

impl Root {
    pub fn native_handle<F: FnOnce(Option<&NativeHandle>)>(&self, f: F) {
        &self
            .vdom
            .exec(|vdom| f(node_with_native_handle(vdom.tree.root(), &vdom.tree)));
    }
}

pub fn generate_root(component: View, renderer: Box<dyn Renderer>) -> Root {
    let vdom = VDom {
        tree: Tree::new(VNode {
            component,
            native_handle: None,
            native_type: None,
            state: None,
            dirty: false,
        }),
        renderer,
    };
    let root_node = vdom.tree.root();
    let vdom = Shared::new(vdom);
    let vdom_clone = vdom.clone();
    vdom.exec_mut(|vdom| {
        generate_vnode(root_node, &mut vdom.tree, &mut vdom.renderer, vdom_clone);
    });

    Root { vdom }
}

///Traverses hierarchy until node with NativeHandle is found.
///Returns None if end of tree has no handle
fn node_with_native_handle(mut vnode: NodeId<VNode>, tree: &Tree<VNode>) -> Option<&NativeHandle> {
    loop {
        if vnode.get(tree).native_handle.is_some() {
            return vnode.get(tree).native_handle.as_ref();
        } else if vnode.iter(tree).len() == 0 {
            return None;
        }
        vnode = if vnode.iter(tree).len() > 1 {
            panic!("Expected non-native Oak component to have 1 child.");
        } else {
            vnode.iter(tree).nth(0).unwrap()
        };
    }
}

pub(crate) fn generate_vnode(
    node: NodeId<VNode>,
    tree: &mut Tree<VNode>,
    renderer: &mut Box<dyn Renderer>,
    vdom: Shared<VDom>,
) {
    let vnode = node.get_mut(tree);

    if vnode.component.is::<()>() {
        return;
    };

    vnode.state = Some(vnode.component.init_state());

    let context = InternalContext {
        state: vnode.state.as_mut().unwrap(),
        component_pos: ComponentPos {
            vnode: node,
            vdom: vdom.clone(),
        },
    };
    let child = vnode.component.render(context);

    match child.downcast_ref::<HasChildrenMarker>() {
        Some(marker) => {
            for child in marker.children.iter() {
                let child = node.push(VNode::component(child.clone()), tree);
                generate_vnode(child, tree, renderer, vdom.clone());
            }
        }
        None => {
            let child = node.push(VNode::component(child.clone()), tree);
            generate_vnode(child, tree, renderer, vdom.clone());
        }
    };

    let vnode = node.get_mut(tree);
    let native_type = vnode.component.native_type();
    vnode.native_type = native_type;
    // convert to immutable borrow
    let vnode = node.get(tree);

    if let Some(native_type) = &vnode.native_type {
        let mut children = node.iter(tree).map(|c| Child {
            component: &c.get(tree).component,
            native_handle: node_with_native_handle(c, tree),
        });
        let native_handle =
            renderer.create_component(&native_type, &vnode.component, &mut children);
        std::mem::drop(children);
        node.get_mut(tree).native_handle = native_handle;
    }
}

#[derive(PartialEq, Eq, Hash)]
struct ChildId {
    key: Option<String>,
    location: Option<(u32, u32)>,
}

impl ChildId {
    fn from_view(view: &View) -> Self {
        Self {
            key: view.key().map(ToOwned::to_owned),
            location: view.location(),
        }
    }
}

fn component_identity_cmp(a: &View, b: &View) -> bool {
    a.key() == b.key() && a.location() == b.location()
}

// /// Allow hashing and equality within a [`HashSet`] with both [`View`]
// /// and [`VNode`]. Facilitates key- and location-based lookup in [`update_vnode`].
// enum ByChildId<'a> {
//     View(&'a View),
//     VNode(VNode)
// }

// impl<'a> ByChildId<'a> {
//     fn view(&self) -> &View {
//         match &self {
//             ByChildId::View(view) => view,
//             ByChildId::VNode(vnode) => &vnode.component
//         }
//     }
// }

// impl<'a> Hash for ByChildId<'a> {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         ChildId::from_view(self.view()).hash(state);
//     }
// }

// impl<'a> PartialEq for ByChildId<'a> {
//     fn eq(&self, other: &Self) -> bool {
//         ChildId::from_view(self.view()) == ChildId::from_view(other.view())
//     }
// }

// impl<'a> Eq for ByChildId<'a> {}

/// Precondition: `new_component` is of the same type as the current component
pub(crate) fn update_vnode(
    mut new_component: Option<View>,
    node: NodeId<VNode>,
    tree: &mut Tree<VNode>,
    renderer: &mut Box<dyn Renderer>,
    vdom: Shared<VDom>,
) {
    let props_updated = match new_component {
        Some(ref new) => new.updated(),
        None => false,
    };
    let vnode = node.get_mut(tree);
    let state_updated = vnode.dirty;

    //if neither props nor state have changed
    //we do not need to rerender
    if !(props_updated || state_updated) {
        return;
    }

    let old_component = match new_component {
        Some(ref mut comp) => {
            std::mem::swap(&mut vnode.component, comp);
            Some(&*comp)
        }
        None => None,
    };

    let context = InternalContext {
        state: vnode.state.as_mut().unwrap(),
        component_pos: ComponentPos {
            vnode: node,
            vdom: vdom.clone(),
        },
    };

    let child = vnode.component.render(context);

    let children = match child.downcast_ref::<HasChildrenMarker>() {
        Some(marker) => marker.children.clone(),
        None => vec![child],
    };

    let mut vnode_children_iter = node.iter_mut(tree).enumerate();
    let mut new_children_iter = children.into_iter();

    loop {
        let vnode_child = vnode_children_iter.next();
        let new_child = new_children_iter.next();
        let mut new_child = match new_child {
            Some(new_child) => new_child,
            None => {
                // remove remaining vnodes
                // as they do not correspond to any new children
                for (_, vnode_child) in vnode_children_iter.rev() {
                    destroy_vnode(vnode_child.remove(tree), renderer);
                }
                if let Some((_, vnode_child)) = vnode_child {
                    destroy_vnode(vnode_child.remove(tree), renderer);
                }
                break;
            }
        };

        let (mut vnode_i, vnode_child) = match vnode_child {
            Some(vnode_child) => vnode_child,
            None => {
                // as no more vnodes to match, generate and append all new children
                // to the end of the node we are updating
                let new_vnode = VNode::component(new_child);
                let new_id = node.push(new_vnode, tree);
                generate_vnode(new_id, tree, renderer, vdom.clone());
                for new_child in new_children_iter {
                    let new_vnode = VNode::component(new_child);
                    let new_id = node.push(new_vnode, tree);
                    generate_vnode(new_id, tree, renderer, vdom.clone());
                }
                break;
            }
        };

        if component_identity_cmp(&new_child, &vnode_child.get(tree).component) {
            update_vnode(
                Some(new_child),
                node.child(vnode_i, tree),
                tree,
                renderer,
                vdom.clone(),
            );
            continue;
        } else {
            // do fancy HashSet stuff
            let len = vnode_children_iter.len();
            let mut hashset = HashMap::with_capacity(len);
            hashset.insert(
                ChildId::from_view(&vnode_child.get(tree).component),
                vnode_child,
            );
            for (_, node) in vnode_children_iter {
                hashset.insert(ChildId::from_view(&node.get(tree).component), node);
            }
            if len > hashset.len() {
                // TODO: issue key warning in debug mode
            }

            loop {
                if let Some(child_node) = hashset.get(&ChildId::from_view(&new_child)) {
                    update_vnode(Some(new_child), *child_node, tree, renderer, vdom.clone());
                    // TODO: swap into correct position
                    todo!();
                // vnode_i += 1;
                } else {
                    let vnode = VNode::component(new_child);
                    let new_vnode = node.insert(vnode_i, vnode, tree);
                    generate_vnode(new_vnode, tree, renderer, vdom.clone());
                }

                if let Some(child) = new_children_iter.next() {
                    new_child = child;
                } else {
                    // remove now-unused vnodes
                    for i in (vnode_i..node.len(tree)).rev() {
                        destroy_vnode(node.remove_child(i, tree), renderer);
                    }
                    break;
                }
            }
            break;
        }
    }

    if let Some(old_component) = old_component {
        // The native_action can only be updated if the type of component
        // has remained the same
        let mut parent_children = node.parent_children_iter(tree);
        let (vnode, children_iter, tree) = parent_children.get();
        if old_component.type_id() == vnode.component.type_id() {
            vnode.native_type = vnode.component.native_type();
            match &vnode.native_type {
                Some(native_action) => {
                    let mut children_iter = children_iter.map(|c| Child {
                        component: &c.get(tree).component,
                        native_handle: node_with_native_handle(c, tree),
                    });
                    
                    renderer.update_component(
                        native_action,
                        vnode.native_handle.as_mut().unwrap(),
                        &vnode.component,
                        &mut children_iter,
                    );
                }
                None => {}
            }
        }
    }

    node.get_mut(tree).dirty = false;
}

// pub(crate) fn update_vnode(
//     mut new_component: Option<View>,
//     node: NodeId<VNode>,
//     tree: &mut Tree<VNode>,
//     renderer: &mut Box<dyn Renderer>,
//     vdom: Shared<VDom>,
// ) {
//     let props_updated = match new_component {
//         Some(ref new) => new.updated(),
//         None => false,
//     };
//     let vnode = node.get_mut(tree);
//     let state_updated = vnode.dirty;

//     //if neither props nor state have changed
//     //we do not need to rerender
//     if !(props_updated || state_updated) {
//         return;
//     }

//     let old_component = match new_component {
//         Some(ref mut comp) => {
//             std::mem::swap(&mut vnode.component, comp);
//             Some(&*comp)
//         }
//         None => None,
//     };

//     let context = InternalContext {
//         state: vnode.state.as_mut().unwrap(),
//         component_pos: ComponentPos {
//             vnode: node,
//             vdom: vdom.clone(),
//         },
//     };

//     if vnode.component.rc.is::<()>() {
//         // TODO: clean up
//         return;
//     }

//     let child = vnode.component.render(context);

//     let mut children = match child.downcast_ref::<HasChildrenMarker>() {
//         Some(marker) => marker.children.clone(),
//         None => vec![child],
//     };

//     //TODO: rewrite this code using a key mechanism when devised
//     //potentially have a key() method returning something hashable
//     //then put the old components in a HashMap using the key() values as map keys
//     //use the keys of the new children to match
//     //the implementation below is naive and is only efficient
//     //for single children and concatenation

//     //if the number of children has decreased, clear the extra children
//     let node_children_len = node.iter(tree).len();
//     if node_children_len > children.len() {
//         for i in (node_children_len - children.len())..children.len() {
//             destroy_vnode(node.remove_child(i, tree), renderer);
//         }
//     }

//     let new_children = children.split_off(node.iter(tree).len());

//     //updates every old node with new contents
//     for new in children.into_iter() {
//         //update_vnode(old.clone(), Some(new), renderer);
//         update_vnode(Some(new.clone()), node, tree, renderer, vdom.clone());
//     }

//     for child in new_children.into_iter() {
//         let child_node = node.push(VNode::component(child), tree);
//         generate_vnode(child_node, tree, renderer, vdom.clone());
//     }

//     //if there is no props update, and thus no old_component
//     //the component should not require native updating
//     match old_component {
//         Some(old_component) => {
//             //The native_action can only be updated if the type of component
//             //has remained the same
//             let mut parent_children = node.parent_children_iter(tree);
//             let (vnode, tree) = parent_children.get();
//             if old_component.type_id() == vnode.component.type_id() {
//                 vnode.native_type = vnode.component.native_type();
//                 match &vnode.native_type {
//                     Some(native_action) => {
//                         let children: Vec<_> = node.iter(tree).collect();
//                         let mut children_iter = children.iter().map(|c| Child {
//                             component: &c.get(tree).component,
//                             native_handle: node_with_native_handle(*c, tree),
//                         });
//                         renderer.update_component(
//                             native_action,
//                             vnode.native_handle.as_mut().unwrap(),
//                             &vnode.component,
//                             &mut children_iter,
//                         );
//                     }
//                     None => {}
//                 }
//             }
//         }
//         None => {}
//     }
// }

pub(crate) fn destroy_vnode(mut vnode: VNode, renderer: &mut Box<dyn Renderer>) {
    if vnode.native_handle.is_some() {
        renderer.remove_component(&mut vnode);
    };

    //TODO: call recursively on children?
    //perhaps a new component method revealing whether recursive detruction needed
    //currently not needed for web platform
}
