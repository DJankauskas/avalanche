use std::cell::Cell;
use std::num::NonZeroU64;

use rustc_hash::FxHashMap;

use crate::{ChildId, View};
use crate::renderer::{NativeEvent, NativeHandle, Renderer, Scheduler};
use crate::shared::Shared;
use crate::tracked::InternalGen;
use crate::alloc::{Bump, Vec as BumpVec};

use super::ComponentState;

/// Holds all the component nodes for a given root, as well as state information
/// for allowing updates and the dataflow tracking system to function.
pub(crate) struct VDom {
    /// Stores all the nodes in the vdom, addressable by their `ComponentId`.
    pub(crate) children: FxHashMap<ComponentId, VNode>,
    /// Yields the `ComponentId` for the next created `VNode`, available by calling
    /// the `create_next` method.
    pub(crate) curr_component_id: ComponentId,
    /// The renderer used to convert avalanche representations of native components into
    /// actual native components.
    pub(crate) renderer: Box<dyn Renderer>,
    /// The current state update generation.
    pub(crate) gen: InternalGen,
    /// Updates the vdom by rendering the root component and all descendents that are dirty.
    pub(crate) update_vdom:
        fn(&mut VDom, &Shared<VDom>, &Shared<dyn Scheduler>, Option<(NativeEvent, ComponentId)>),
    /// Allows for efficient render-time allocations.
    pub(crate) bump: Bump,
}

impl VDom {
    /// Remove a vnode from `vdom`, along with all of its descendents.
    pub(super) fn remove_node(&mut self, mut to_remove: BumpVec<ComponentId>) {
        while let Some(node) = to_remove.pop() {
            if let Some(node) = self.children.remove(&node) {
                to_remove.extend(node.body_children.iter().map(|(_, child)| child.id));
            }
        }
    }

    /// Walks up the VDom upwards and marks the given node and its body parents dirty to allow for running the update algorithm
    pub(crate) fn mark_node_dirty(&mut self, mut component_id: ComponentId) {
        loop {
            let vnode = match self.children.get_mut(&component_id) {
                Some(vnode) => vnode,
                None => return,
            };
            vnode.dirty = true;
            component_id = match vnode.body_parent {
                Some(body_parent) => body_parent,
                None => break,
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
/// Represents a unique instance of a component within the lifetime
/// of a particular vdom tree.
pub(crate) struct ComponentId {
    pub id: NonZeroU64,
}

impl ComponentId {
    /// Returns the next instance of a `ComponentId`, modifying the given instance
    /// to allow generating new ids.
    pub fn create_next(&mut self) -> Self {
        let id = self.id;
        self.id = NonZeroU64::try_from(self.id.get() + 1).unwrap();
        ComponentId { id }
    }

    pub fn new() -> Self {
        Self {
            id: NonZeroU64::new(1).unwrap(),
        }
    }
}

/// Holds data specific to avalanche components representing native components.
pub(super) struct NativeComponent {
    pub native_handle: NativeHandle,
    pub native_parent: Option<ComponentId>,
    pub native_children: Vec<ComponentId>,
}

/// Data associated with a child rendered within the body of another function.
pub(super) struct BodyChild {
    /// The component id associated with a child id.
    pub id: ComponentId,
    /// Whether a component has been rendered over.
    pub used: bool,
}

/// State and other data associated with a non-empty component within the `VDom`.
pub(crate) struct VNode {
    /// The component in which the VNode's component was rendered in, if any.
    pub body_parent: Option<ComponentId>,
    /// The components rendered within the render function of the component, if any.
    pub(super) body_children: FxHashMap<ChildId, BodyChild>,
    /// The native information of the given component, if it is native.
    pub(super) native_component: Option<NativeComponent>,
    /// The hook state of the given component.
    pub(crate) state: ComponentState,
    /// Whether the VNode is in need of updating due to a change in state in
    /// itself or a descendent.
    pub(crate) dirty: bool,
    /// Memoized value of the component's `ComponentId` and its closest native descendent.
    pub(crate) view: View,
}

/// A crate allowing bump-allocated data accumulation with only an immutable reference.
pub(crate) struct CellBumpVec<'a, T>(Cell<BumpVec<'a, T>>);

impl<'a, T> CellBumpVec<'a, T> {
    pub fn new_in(bump: &'a Bump) -> Self {
        Self(Cell::new(BumpVec::new_in(bump)))
    }

    pub fn push(&self, elem: T, bump: &'a Bump) {
        let mut inner = self.0.replace(BumpVec::new_in(bump));
        inner.push(elem);
        self.0.set(inner);
    }

    pub fn into_inner(self) -> BumpVec<'a, T> {
        self.0.into_inner()
    }
}
