use std::marker::PhantomData;

pub struct Tree<T> {
    nodes: Vec<Option<Node<T>>>,
    ///from the end, the first open site
    ///if 0, signifies none open
    last_open: usize,
}

pub struct Node<T> {
    ///The index of the node's parent
    ///Must only be used in methods taking a &mut Tree
    parent: usize,
    children: Vec<usize>,
    data: T,
}

pub struct NodeId<T> {
    idx: usize,
    phantom: PhantomData<T>,
}

impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        Self {
            idx: self.idx,
            phantom: self.phantom,
        }
    }
}
impl<T> Copy for NodeId<T> {}

impl<T> NodeId<T> {
    fn idx(idx: usize) -> Self {
        NodeId {
            idx,
            phantom: PhantomData,
        }
    }

    pub fn get(self, tree: &Tree<T>) -> &T {
        &tree.nodes[self.idx].as_ref().expect("valid NodeId").data
    }

    pub fn get_mut(self, tree: &mut Tree<T>) -> &mut T {
        &mut tree.nodes[self.idx].as_mut().expect("valid NodeId").data
    }

    /// Returns the number of children nodes the receiver node has.
    /// # Panics
    /// Panics if `self` is not present in the given `tree`, due to it being
    /// removed or from another tree.
    pub fn len(self, tree: &Tree<T>) -> usize {
        tree.nodes[self.idx].as_ref().expect("valid self").children.len()
    }

    /// If the node has a parent, returns `Some(id)` where `id` is the parent [`NodeId`].
    /// Otherwise, returns `None`.
    /// # Panics
    /// Panics if `self` is not present in the given `tree`, due to it being
    /// removed or from another tree.
    fn parent(self, tree: &Tree<T>) -> Option<NodeId<T>> {
        let node_ref = tree.nodes[self.idx].as_ref().expect("valid self");
        if node_ref.parent != 0 {
            Some(NodeId::idx(node_ref.parent))
        } else {
            None
        }
    }

    /// Get the [`NodeId`] with index `child_idx`
    /// # Panics
    /// Panics if `child_idx` is out of bounds
    pub fn child(self, child_idx: usize, tree: &Tree<T>) -> Self {
        NodeId::idx(tree.nodes[self.idx].as_ref().expect("valid self").children[child_idx])
    }

    /// Helper function for [`push`] and [`insert`].
    /// Returns index of created node within `tree.nodes`
    fn gen_node(self, data: T, tree: &mut Tree<T>) -> Self {
        let node = Node {
            parent: self.idx,
            data,
            children: Vec::new(),
        };

        match tree.next_open_site() {
            Some(idx) => {
                tree.nodes[idx] = Some(node);
                NodeId::idx(idx)
            }
            None => {
                let id = NodeId::idx(tree.nodes.len());
                tree.nodes.push(Some(node));
                id
            }
        }
    }

    /// Inserts a child at position `index`, shifting all subtrees after it to the right
    pub fn insert(self, index: usize, data: T, tree: &mut Tree<T>) -> Self {
        let gen_id = self.gen_node(data, tree);

        tree.nodes[self.idx]
            .as_mut()
            .expect("valid NodeId")
            .children
            .insert(index, gen_id.idx);

        gen_id
    }

    pub fn push(self, data: T, tree: &mut Tree<T>) -> Self {
        let gen_id = self.gen_node(data, tree);

        tree.nodes[self.idx]
            .as_mut()
            .expect("valid NodeId")
            .children
            .push(gen_id.idx);

        gen_id
    }

    /// Removes the specified node from its parent.
    /// Panics if `self` is invalid or is the root node.
    pub fn remove(self, tree: &mut Tree<T>) -> T {
        NodeId::idx(tree.nodes[self.idx].as_ref().unwrap().parent).remove_child(self.idx, tree)
    }

    /// Removes the specified child by index
    pub fn remove_child(self, child: usize, tree: &mut Tree<T>) -> T {
        let idx = *tree.nodes[self.idx]
            .as_ref()
            .expect("valid self")
            .children
            .get(child)
            .expect("valid child index");

        tree.remove(NodeId::idx(idx))
    }

    /// Removes all children from the node
    pub fn clear(self, tree: &mut Tree<T>) {
        let len = tree.nodes[self.idx].as_ref().unwrap().children.len();
        for i in (0..len).rev() {
            self.remove_child(i, tree);
        }
    }

    pub fn iter<'a>(
        self,
        tree: &'a Tree<T>,
    ) -> impl DoubleEndedIterator<Item = Self> + ExactSizeIterator<Item = Self> + 'a {
        tree.nodes[self.idx]
            .as_ref()
            .expect("valid self")
            .children
            .iter()
            .map(|&x| NodeId::idx(x))
    }

    pub fn iter_mut(
        self,
        tree: &Tree<T>,
    ) -> impl DoubleEndedIterator<Item = Self> + ExactSizeIterator<Item = Self> {
        tree.nodes[self.idx]
            .as_ref()
            .expect("valid self")
            .children
            .clone()
            .into_iter()
            .map(|child| NodeId::idx(child))
    }

    pub fn parent_children_iter<'a>(self, tree: &'a mut Tree<T>) -> ParentChildrenIter<T> {
        ParentChildrenIter {
            removed: tree.nodes[self.idx].take(),
            removed_id: self,
            tree,
        }
    }
}

pub struct ParentChildrenIter<'a, T> {
    removed: Option<Node<T>>,
    removed_id: NodeId<T>,
    tree: &'a mut Tree<T>,
}

//TODO: optimize with use of ManuallyDropped (note: unsafe code)
impl<'a, T> ParentChildrenIter<'a, T> {
    pub fn get<'f>(
        &'f mut self,
    ) -> (
        &'f mut T,
        impl DoubleEndedIterator<Item = NodeId<T>> + ExactSizeIterator<Item = NodeId<T>> + 'f,
        &'f Tree<T>,
    ) {
        let mut_removed = self.removed.as_mut().unwrap();
        (
            &mut mut_removed.data,
            mut_removed.children.iter().map(|idx| NodeId::idx(*idx)),
            &self.tree,
        )
    }
}

impl<'a, T> Drop for ParentChildrenIter<'a, T> {
    fn drop(&mut self) {
        self.tree.nodes[self.removed_id.idx] = Some(self.removed.take().unwrap())
    }
}

impl<T> Tree<T> {
    pub fn new(data: T) -> Self {
        Self {
            nodes: vec![Some(Node {
                parent: 0,
                children: Vec::new(),
                data,
            })],
            last_open: 0,
        }
    }

    pub fn root(&self) -> NodeId<T> {
        NodeId::idx(0)
    }

    pub fn remove(&mut self, node_id: NodeId<T>) -> T {
        let node = self.nodes[node_id.idx].take().expect("valid child NodeId");
        if node_id.idx > self.last_open {
            self.last_open = node_id.idx;
        };
        //remove child nodes recursively
        for child_idx in node.children.iter() {
            self.remove(NodeId::idx(*child_idx));
        }
        let parent_children = &mut self.nodes[node.parent]
            .as_mut()
            .expect("valid self NodeId")
            .children;
        let children_idx = *parent_children.iter().find(|&&x| x == node_id.idx).unwrap();
        parent_children.remove(children_idx);
        node.data
    }

    ///find next open site, if available
    fn next_open_site(&mut self) -> Option<usize> {
        //0 is always full, so this indicates no open site
        if self.last_open == 0 {
            return None;
        }

        //construct an iterator that iterates from last_site to start of array
        let rev_iter = self
            .nodes
            .iter()
            .enumerate()
            .rev()
            .skip(self.nodes.len() - self.last_open - 1);

        for (i, x) in rev_iter {
            //if empty site found, we store its position
            //this may enable faster searching later
            if let None = x {
                self.last_open = i;
                return Some(i);
            }
        }

        //if we didn't find anything, we say there are no open sites
        self.last_open = 0;
        None
    }

    pub fn get_mut_pair(&mut self, a: NodeId<T>, b: NodeId<T>) -> (&mut T, &mut T) {
        if a.idx < b.idx {
            let (left, right) = self.nodes.split_at_mut(b.idx);
            (
                &mut left[a.idx].as_mut().expect("valid NodeId a").data,
                &mut right[0].as_mut().expect("valid NodeId b").data,
            )
        } else {
            let (left, right) = self.nodes.split_at_mut(a.idx);
            (
                &mut right[0].as_mut().expect("valid NodeId a").data,
                &mut left[b.idx].as_mut().expect("valid NodeId b").data,
            )
        }
    }
}
