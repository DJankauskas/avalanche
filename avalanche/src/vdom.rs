use crate::tree::{NodeId, Tree};
use crate::View;
use crate::{
    renderer::{HasChildrenMarker, NativeHandle, NativeType, Renderer, Scheduler},
    ComponentPos,
};

use crate::{shared::Shared, InternalContext};
use std::{
    any::Any,
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
};

const DYNAMIC_CHILDREN_ERR: &'static str = "Dynamic components must be provided keys.";

pub struct VDom {
    pub(crate) tree: Tree<VNode>,
    pub renderer: Box<dyn Renderer>,
}

// TODO: possibly make pub(crate)
/// A "virtual node" in the UI hierarchy. Contains the node's component,
/// native information, and associated state.
#[doc(hidden)]
pub(crate) struct VNode {
    pub component: View,
    pub native_handle: Option<NativeHandle>,
    pub native_type: Option<NativeType>,
    pub(crate) state: Option<Box<dyn Any>>,
    pub(crate) dirty: bool,
}

impl VNode {
    /// default VNode initialized with a component
    /// this VNode should be filled in with [`generate_vnode`]
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

/// Contains the data structures necessary to support the avalanche vdom abstraction. This struct
/// should only be used by renderer implementation libraries.
///
/// # Usage
///
/// In order to render an avalanche `View`, a renderer library should accept a `View` from the user, then
/// use the `new` method to create a `Root` instance.
pub struct Root {
    _vdom: Shared<VDom>,
}

impl Root {
    /// Creates a new UI tree rooted at `native_parent`, with native handle `native_handle`. That handle will be used,
    /// and `renderer.create_component` will not be called for it, in order to allow rooting an avaqlanche tree upon
    /// an existing UI component created externally. Renders `child` as the child of `native_parent`; `native_parent`
    /// must also return only `child` from its `render` method within `HasChildMarker`.
    pub fn new<R: Renderer + 'static, S: Scheduler + 'static>(
        child: View,
        native_parent: View,
        native_handle: NativeHandle,
        renderer: R,
        scheduler: S,
    ) -> Self {
        let native_type = native_parent
            .native_type()
            .expect("native_parent has native_type");
        let native_parent_state = native_parent.init_state();
        let mut vnode = VNode::component(native_parent);
        vnode.native_type = Some(native_type);
        vnode.native_handle = Some(native_handle);
        vnode.state = Some(native_parent_state);
        let scheduler: Shared<dyn Scheduler> = Shared::new_dyn(Rc::new(RefCell::new(scheduler)));
        let vdom = VDom {
            tree: Tree::new(vnode),
            renderer: Box::new(renderer),
        };
        let vdom = Shared::new(vdom);
        let vdom_clone = vdom.clone();
        vdom.exec_mut(|vdom| {
            let root = vdom.tree.root();
            let child = root.push(VNode::component(child), &mut vdom.tree);
            generate_vnode(
                child,
                &mut vdom.tree,
                &mut vdom.renderer,
                &vdom_clone,
                &scheduler,
            );
            native_append_child(root, child, &mut vdom.tree, &mut vdom.renderer);
        });

        Root { _vdom: vdom }
    }
}

/// Traverses hierarchy until node with NativeHandle is found.
/// Returns None if end of tree has no handle.
/// # Panics
/// Panics if `node` is invalid or a node violates the invariant that it may only have multiple
/// children if it is a native component.
fn child_with_native_handle(mut node: NodeId<VNode>, tree: &Tree<VNode>) -> Option<NodeId<VNode>> {
    loop {
        if node.get(tree).native_handle.is_some() {
            return Some(node);
        } else if node.iter(tree).len() == 0 {
            return None;
        }
        node = if node.iter(tree).len() > 1 {
            panic!("Expected non-native Oak component to have 1 child.");
        } else {
            node.iter(tree).nth(0).unwrap()
        };
    }
}

/// Given a `node` with a new component, generates a virtual tree for its children,
/// and renders it.
pub(crate) fn generate_vnode(
    node: NodeId<VNode>,
    tree: &mut Tree<VNode>,
    renderer: &mut Box<dyn Renderer>,
    vdom: &Shared<VDom>,
    scheduler: &Shared<dyn Scheduler>,
) {
    let vnode = node.get_mut(tree);

    if vnode.component.is::<()>() {
        return;
    };

    vnode.state = Some(vnode.component.init_state());

    let context = InternalContext {
        state: vnode.state.as_mut().unwrap(),
        component_pos: ComponentPos {
            node_id: node.into(),
            vdom: vdom,
        },
        scheduler,
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
                generate_vnode(child, tree, renderer, vdom, scheduler);
                if is_native {
                    native_append_child(node, child, tree, renderer);
                }
            }
        }
        None => {
            let child = node.push(VNode::component(child.clone()), tree);
            generate_vnode(child, tree, renderer, vdom, scheduler);
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

/// Uniquely represents a component amongst its children.
/// Components with the same location rendered multiple times must be given unique keys.
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

/// Given a node within the VDom, goes up its hierarchy until finding the first parent with a
/// native handle, returning it if present, or `None` otherwise. Sets all nodes up to but
/// excluding the parent to `dirty` to propagate the need for an update
fn propogate_update_to_native_parent(
    mut node: NodeId<VNode>,
    tree: &mut Tree<VNode>,
) -> Option<NodeId<VNode>> {
    loop {
        node = node.parent(tree)?;
        let node = node.get_mut(tree);
        if node.native_handle.is_some() {
            break;
        }
        node.dirty = true;
    }
    Some(node)
}

// TODO: clarify: can new_component be a different type than the old component?
// right now, assumption is no
/// Updates the given `node` so that its children and corresponding native elements
/// match the current properties and state of its component.
pub(crate) fn update_vnode(
    new_component: Option<View>,
    node: NodeId<VNode>,
    tree: &mut Tree<VNode>,
    renderer: &mut Box<dyn Renderer>,
    vdom: &Shared<VDom>,
    scheduler: &Shared<dyn Scheduler>,
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

    // enables recursive behavior like propogate_update_to_native_parent to work properly
    vnode.dirty = true;

    let old_component = match new_component {
        Some(mut comp) => {
            std::mem::swap(&mut vnode.component, &mut comp);
            Some(comp)
        }
        None => None,
    };

    let context = InternalContext {
        state: vnode.state.as_mut().unwrap(),
        component_pos: ComponentPos {
            node_id: node.into(),
            vdom: vdom,
        },
        scheduler,
    };

    let child = vnode.component.render(context);

    let children = match child.downcast_ref::<HasChildrenMarker>() {
        Some(marker) => marker.children.clone(),
        None => vec![child],
    };

    // If the component is non-native, but its native_child potentially changes, this result must be
    // propagated up to its native parent, unless if the native parent is already being processed
    if !is_native {
        let old_child = &node.child(0, tree).get(tree).component;
        let new_child = &children[0];
        if old_child.location() != new_child.location() || old_child.key() != new_child.key() {
            let native_parent = propogate_update_to_native_parent(node, tree)
                .expect("native parent of a component with updated native identity");
            let native_parent_mut = native_parent.get_mut(tree);
            // If the native parent is not currently being updated, restart the update process there.
            if !native_parent_mut.dirty {
                native_parent_mut.dirty = true;
                if let Some(mut old_component) = old_component {
                    let vnode_mut = node.get_mut(tree);
                    std::mem::swap(&mut vnode_mut.component, &mut old_component);
                }
                update_vnode(None, native_parent, tree, renderer, vdom, scheduler);
                return;
            }
        }
    };

    let vnode_children_iter = node.iter(tree).enumerate();
    let vnode_children_len = vnode_children_iter.len();
    let mut in_place_components = HashMap::with_capacity(vnode_children_len);

    let mut curr_native_idx = 0usize;
    let mut native_indices = Vec::with_capacity(vnode_children_len);

    for (idx, old_vnode) in vnode_children_iter {
        in_place_components.insert(ChildId::from_view(&old_vnode.get(tree).component), {
            (old_vnode, idx)
        });
        native_indices.push(child_with_native_handle(old_vnode, tree).map(|_| {
            let idx = curr_native_idx;
            curr_native_idx += 1;
            idx
        }));
    }

    if in_place_components.len() != vnode_children_len {
        panic!("{}", DYNAMIC_CHILDREN_ERR);
    }

    let children_ids: Vec<_> = children
        .iter()
        .map(|view| ChildId::from_view(view))
        .collect();

    let check_duplicates: HashSet<_> = children_ids.iter().collect();
    if check_duplicates.len() != children_ids.len() {
        panic!("{}", DYNAMIC_CHILDREN_ERR);
    }

    let mut children: Vec<_> = children.into_iter().map(|c| Some(c)).collect();

    for (child, id) in children.iter_mut().zip(children_ids.iter()) {
        if in_place_components.get(id).is_none() {
            let vnode = VNode::component(child.take().unwrap());
            let new_child = node.push(vnode, tree);
            in_place_components.insert(id.clone(), (new_child, node.len(tree) - 1));
            generate_vnode(new_child, tree, renderer, vdom, scheduler);
            if is_native {
                native_append_child(node, new_child, tree, renderer);
            }
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
                    vdom,
                    scheduler,
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

/// Inserts `child`'s corresponding native element at position `pos`.
/// # Panics
/// Panics if `parent` does not have a native handle and native type, or if `pos` is greater than
/// the length of `parent`'s current native children.
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

/// Updates the given `child`, and if `is_native` is true, creates, updates, or removes its
/// corresponding native element as needed. `native_pos` should be the index of the native element of `child`,
// if it possesses one.
/// This function is intended to be called from the last child
/// to the first, in reverse order, as `native_pos` is decremented when a child that contains a native element is processed.
/// # Panics
/// Panics if `is_native` is incorrect, `native_pos` is out of bounds, or `parent` and `child` are not
/// valid parents and children within `tree`.
fn native_update_vnode(
    is_native: bool,
    native_pos: &mut usize,
    new_component: Option<View>,
    parent: NodeId<VNode>,
    child: NodeId<VNode>,
    tree: &mut Tree<VNode>,
    renderer: &mut Box<dyn Renderer>,
    vdom: &Shared<VDom>,
    scheduler: &Shared<dyn Scheduler>,
) {
    let old_native_child = if is_native {
        child_with_native_handle(child, tree)
    } else {
        None
    };
    update_vnode(new_component, child, tree, renderer, vdom, scheduler);

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
