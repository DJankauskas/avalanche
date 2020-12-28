use crate::tree::{NodeId, Tree};
use crate::View;
use crate::{
    renderer::{HasChildrenMarker, NativeHandle, NativeType, Renderer},
    ComponentPos,
};

use crate::{shared::Shared, InternalContext};
use std::{any::Any, collections::HashMap, hash::Hash, ops::Deref, ops::DerefMut};

const DYNAMIC_CHILDREN_ERR: &'static str = "Dynamic components must be provided keys.";

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
        &self.vdom.exec(|vdom| {
            let node = child_with_native_handle(vdom.tree.root(), &vdom.tree);
            let native_handle = node.map(|node| node.get(&vdom.tree).native_handle.as_ref());
            f(native_handle.flatten());
        });
    }
}

pub fn generate_root(component: View, renderer: Box<dyn Renderer>) -> Root {
    let vdom = VDom {
        tree: Tree::new(VNode::component(component)),
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
fn child_with_native_handle(mut vnode: NodeId<VNode>, tree: &Tree<VNode>) -> Option<NodeId<VNode>> {
    loop {
        if vnode.get(tree).native_handle.is_some() {
            return Some(vnode);
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

fn parent_with_native_handle(
    mut vnode: NodeId<VNode>,
    tree: &Tree<VNode>,
) -> Option<NodeId<VNode>> {
    loop {
        vnode = vnode.parent(tree)?;
        let vnode_ref = vnode.get(tree);
        if vnode_ref.native_handle.is_some() {
            return Some(vnode);
        }
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

    let vnode = node.get_mut(tree);
    let native_type = vnode.component.native_type();
    vnode.native_type = native_type;
    // convert to immutable borrow
    let vnode = node.get(tree);

    let is_native = match &vnode.native_type {
        Some(native_type) => {
            let native_handle = renderer.create_component(&native_type, &vnode.component);
            node.get_mut(tree).native_handle = Some(native_handle);
            true
        }
        None => false,
    };

    match child.downcast_ref::<HasChildrenMarker>() {
        Some(marker) => {
            for child in marker.children.iter() {
                let child = node.push(VNode::component(child.clone()), tree);
                generate_vnode(child, tree, renderer, vdom.clone());
                if is_native {
                    native_append_child(node, child, tree, renderer);
                }
            }
        }
        None => {
            let child = node.push(VNode::component(child.clone()), tree);
            generate_vnode(child, tree, renderer, vdom.clone());
            if is_native {
                native_append_child(node, child, tree, renderer);
            }
        }
    };
}

/// if a child has a native element,
/// appends it to the children of the parent
/// `parent` must have a `native_handle` and `native_type`
fn native_append_child(
    parent: NodeId<VNode>,
    child: NodeId<VNode>,
    tree: &mut Tree<VNode>,
    renderer: &mut Box<dyn Renderer>,
) {
    if let Some(native_child) = child_with_native_handle(child, tree) {
        let (parent_mut, child_mut) = tree.get_mut_pair(parent, native_child);
        renderer.append_child(
            parent_mut
                .native_type
                .as_ref()
                .expect("parent is a native component"),
            parent_mut
                .native_handle
                .as_mut()
                .expect("parent is a native component"),
            child_mut.native_type.as_ref().unwrap(),
            child_mut.native_handle.as_ref().unwrap(),
        );
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
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

#[derive(Copy, Clone, Debug)]
enum NativePos {
    /// The vnode's corresponding native component position
    Pos(usize),
    /// The vnode has no native component, so the index of the next
    /// Note: it may be one past the upper bound
    Next(usize),
}

impl Deref for NativePos {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        match self {
            NativePos::Pos(pos) => pos,
            NativePos::Next(next) => next,
        }
    }
}

impl DerefMut for NativePos {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            NativePos::Pos(pos) => pos,
            NativePos::Next(next) => next,
        }
    }
}

impl NativePos {
    fn increment(&mut self) {
        **self += 1
    }
    fn decrement(&mut self) {
        **self -= 1
    }
}

// TODO: clarify: can new_component be a different type than the old component?
// right now, assumption is no
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

    debug_assert_eq!(vnode.native_handle.is_some(), vnode.native_type.is_some());
    let is_native = vnode.native_handle.is_some();

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

    let vnode_children_iter = node.iter(tree).enumerate();
    let vnode_children_len = vnode_children_iter.len();
    let mut in_place_components = HashMap::with_capacity(vnode_children_len);

    let mut curr_native_idx = 0usize;
    let mut native_indices = Vec::with_capacity(vnode_children_len);

    for (idx, old_vnode) in vnode_children_iter {
        let old_value = in_place_components
            .insert(ChildId::from_view(&old_vnode.get(tree).component), {
                (old_vnode, idx)
            });
        assert!(old_value.is_none(), DYNAMIC_CHILDREN_ERR);
        native_indices.push(child_with_native_handle(old_vnode, tree).map(|_| {
            let idx = curr_native_idx;
            curr_native_idx += 1;
            idx
        }));
    }

    if in_place_components.len() != vnode_children_len {
        panic!(DYNAMIC_CHILDREN_ERR);
    }

    let children_ids: Vec<_> = children
        .iter()
        .map(|view| ChildId::from_view(view))
        .collect();

    let mut children: Vec<_> = children.into_iter().map(|c| Some(c)).collect();

    for (child, id) in children.iter_mut().zip(children_ids.iter()) {
        if in_place_components.get(id).is_none() {
            let vnode = VNode::component(child.take().unwrap());
            let new_child = node.push(vnode, tree);
            generate_vnode(new_child, tree, renderer, vdom.clone());
            native_append_child(node, new_child, tree, renderer);
            let old_value = in_place_components.insert(id.clone(), (new_child, node.len(tree) - 1));
            assert!(old_value.is_none(), DYNAMIC_CHILDREN_ERR);
        }
    }

    for i in vnode_children_len..node.len(tree) {
        native_indices.push(
            child_with_native_handle(node.child(i, tree), tree).map(|_| {
                let idx = curr_native_idx;
                curr_native_idx += 1;
                idx
            }),
        );
    }

    renderer.log(&format!("{:#?}", native_indices));

    for (i, id) in children_ids.iter().enumerate() {
        let old_node = node.child(i, tree).get(tree);
        let old_node_id = ChildId::from_view(&old_node.component);
        if old_node_id != *id {
            let swap_node_ref = in_place_components.get(id).unwrap();
            let swap_node = swap_node_ref.0;
            let swap_pos = swap_node_ref.1;
            node.swap_children(i, swap_pos, tree);
            native_indices.swap(i, swap_pos);
            let old_node_mut = in_place_components.get_mut(&old_node_id).unwrap();
            old_node_mut.0 = swap_node;
            old_node_mut.1 = swap_pos;
        }
        // TODO: remove used-up elements from in_place_components?
        // or update .1 of the entry originally at swap_pos?
    }

    // prevent use of some inconsistent state
    std::mem::drop(in_place_components);

    if is_native {
        let native_indices: Vec<_> = native_indices.into_iter().filter_map(|i| i).collect();
        let mut native_indices_map = vec![usize::MAX; native_indices.len()];
        for (i, elem) in native_indices.iter().enumerate() {
            native_indices_map[*elem] = i;
        }
        renderer.log(&format!("{:#?}", native_indices));
        let node_mut = node.get_mut(tree);
        let node_type = node_mut.native_type.as_ref().unwrap();
        let node_handle = node_mut.native_handle.as_mut().unwrap();
        for i in 0..native_indices.len() {
            while i != native_indices_map[i] {
                let swap_pos = native_indices_map[i];
                renderer.swap_children(node_type, node_handle, i, swap_pos);
                native_indices_map.swap(i, swap_pos);
            }
        }

        for i in (children_ids.len()..node.len(tree)).rev() {
            if let Some(_) = child_with_native_handle(node.child(i, tree), tree) {
                let node_mut = node.get_mut(tree);
                let parent_type = node_mut.native_type.as_ref().unwrap();
                let parent_handle = node_mut.native_handle.as_mut().unwrap();
                renderer.remove_child(
                    parent_type,
                    parent_handle,
                    native_indices_map.pop().unwrap(),
                );
                curr_native_idx = curr_native_idx.saturating_sub(1);
            }
        }
    }

    
    for i in (children_ids.len()..node.len(tree)).rev() {
        node.remove_child(i, tree);
    }

    debug_assert_eq!(children.len(), node.len(tree));

    curr_native_idx = curr_native_idx.saturating_sub(1);
    for (child, child_vnode) in children.into_iter().zip(node.iter_mut(tree)).rev() {
        match child {
            Some(child) => {
                native_update_vnode(
                    is_native,
                    &mut curr_native_idx,
                    Some(child),
                    node,
                    child_vnode,
                    tree,
                    renderer,
                    vdom.clone(),
                );
            }
            None => {
                if is_native && child_with_native_handle(child_vnode, tree).is_some() {
                    curr_native_idx = curr_native_idx.saturating_sub(1);
                }
            }
        }
    }

    let vnode_mut = node.get_mut(tree);
    if let Some(old_component) = old_component {
        // The native_action can only be updated if the type of component
        // has remained the same
        if old_component.type_id() == vnode_mut.component.type_id() {
            vnode_mut.native_type = vnode_mut.component.native_type();
            if let Some(native_type) = &vnode_mut.native_type {
                renderer.update_component(
                    native_type,
                    vnode_mut.native_handle.as_mut().unwrap(),
                    &vnode_mut.component,
                );
            }
        }
    }

    node.get_mut(tree).dirty = false;
}

fn native_insert_child(
    parent: NodeId<VNode>,
    child: NodeId<VNode>,
    pos: usize,
    tree: &mut Tree<VNode>,
    renderer: &mut Box<dyn Renderer>,
) {
    if let Some(native_child) = child_with_native_handle(child, tree) {
        let (parent_mut, child_mut) = tree.get_mut_pair(parent, native_child);
        renderer.insert_child(
            parent_mut
                .native_type
                .as_ref()
                .expect("parent is a native component"),
            parent_mut
                .native_handle
                .as_mut()
                .expect("parent is a native component"),
            pos,
            child_mut.native_type.as_ref().unwrap(),
            child_mut.native_handle.as_ref().unwrap(),
        );
    }
}

fn native_update_vnode(
    is_native: bool,
    native_pos: &mut usize,
    new_component: Option<View>,
    parent: NodeId<VNode>,
    child: NodeId<VNode>,
    tree: &mut Tree<VNode>,
    renderer: &mut Box<dyn Renderer>,
    vdom: Shared<VDom>,
) {
    let old_native_child = if is_native {
        child_with_native_handle(child, tree)
    } else {
        None
    };
    update_vnode(new_component, child, tree, renderer, vdom);
    let new_native_child = if is_native {
        child_with_native_handle(child, tree)
    } else {
        None
    };

    match (old_native_child, new_native_child) {
        (Some(_), Some(new)) => {
            let (parent_mut, child_mut) = tree.get_mut_pair(parent, new);
            //TODO: should old component be explicitly destroyed?
            // TODO: should checking if this is necessary occur in the `replace_child`
            // method or through some other means?
            renderer.replace_child(
                parent_mut.native_type.as_ref().unwrap(),
                parent_mut.native_handle.as_mut().unwrap(),
                *native_pos,
                child_mut.native_type.as_ref().unwrap(),
                child_mut.native_handle.as_ref().unwrap(),
            );
            *native_pos = native_pos.saturating_sub(1);
        }
        // There was a native child, and now there isn't on rerender
        (Some(_), None) => {
            // TODO: this doesn't use `destroy_vnode`
            // refactor later to use either only `remove_component`
            // or `remove_child`
            let parent_mut = parent.get_mut(tree);
            renderer.remove_child(
                parent_mut.native_type.as_ref().unwrap(),
                parent_mut.native_handle.as_mut().unwrap(),
                *native_pos,
            );
            *native_pos = native_pos.saturating_sub(1);
        }
        // There was no native child, but now there is on rerender
        (None, Some(new)) => {
            native_insert_child(parent, new, *native_pos, tree, renderer);
        }
        // no native child before and on rerender
        (None, None) => {}
    }
}
