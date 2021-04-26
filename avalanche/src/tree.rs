use std::marker::PhantomData;

pub struct Tree<T> {
    nodes: Vec<Option<Node<T>>>,
    /// from the end, the first open site
    /// if 0, signifies none open
    last_open: usize,
}

pub struct Node<T> {
    /// The index of the node's parent.
    /// Must only be used in methods taking a &mut Tree.
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
    /// Constructs a `NodeId` from a given `idx` to a node in the backing `Vec`.
    fn idx(idx: usize) -> Self {
        NodeId {
            idx,
            phantom: PhantomData,
        }
    }

    /// Gets a reference to the node's corresponding element; `tree` cannot be
    /// mutated during the lifetime of this borrow.
    #[must_use]
    pub fn get(self, tree: &Tree<T>) -> &T {
        &tree.nodes[self.idx].as_ref().expect("valid NodeId").data
    }

    /// Gets a mutable reference to the node's corresponding element; `tree` 
    /// is borrowed for the lifetime of this borrow.
    #[must_use]
    pub fn get_mut(self, tree: &mut Tree<T>) -> &mut T {
        &mut tree.nodes[self.idx].as_mut().expect("valid NodeId").data
    }

    /// Returns the number of children nodes the receiver node has.
    /// # Panics
    /// Panics if `self` is not present in the given `tree`, due to it being
    /// removed or from another tree.
    pub fn len(self, tree: &Tree<T>) -> usize {
        tree.nodes[self.idx]
            .as_ref()
            .expect("valid self")
            .children
            .len()
    }

    /// If the node has a parent, returns `Some(id)` where `id` is the parent [`NodeId`].
    /// Otherwise, returns `None`.
    /// # Panics
    /// Panics if `self` is not present in the given `tree`, due to it being
    /// removed or from another tree.
    pub fn parent(self, tree: &Tree<T>) -> Option<NodeId<T>> {
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
        NodeId::idx(
            tree.nodes
                .get(self.idx)
                .expect("valid self")
                .as_ref()
                .expect("valid self")
                .children
                .get(child_idx)
                .copied()
                .expect("in bounds child_idx"),
        )
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
                debug_assert!(tree.nodes[idx].is_none(), "overwriting occupied cell");
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
    /// # Panics
    /// Panics if `index` is greater than the tree's `len`.
    pub fn insert(self, index: usize, data: T, tree: &mut Tree<T>) -> Self {
        let gen_id = self.gen_node(data, tree);

        tree.nodes[self.idx]
            .as_mut()
            .expect("valid NodeId")
            .children
            .insert(index, gen_id.idx);

        gen_id
    }

    /// Appends the given element to the back of the children of the given node.
    pub fn push(self, data: T, tree: &mut Tree<T>) -> Self {
        let gen_id = self.gen_node(data, tree);

        tree.nodes[self.idx]
            .as_mut()
            .expect("valid NodeId")
            .children
            .push(gen_id.idx);

        gen_id
    }

    /// Moves the child at index `old` to the index `new`.
    /// # Panics
    /// Panics if `old` or `new` are out of bounds.
    pub fn move_child(self, old: usize, new: usize, tree: &mut Tree<T>) {
        let children = &mut tree.nodes[self.idx]
            .as_mut()
            .expect("valid NodeId")
            .children;
        let id = children.remove(old);
        children.insert(new, id);
    }

    /// Removes the specified child by index
    /// # Panics
    /// Panics if `index` is out of bounds.
    pub fn remove_child(self, child: usize, tree: &mut Tree<T>) -> T {
        // TODO: check for child idx validity
        let child_node_idx = tree.nodes[self.idx].as_mut().expect("valid self id").children.remove(child);
        let child_node = tree.nodes[child_node_idx].as_mut().unwrap();
        //remove child nodes recursively
        for child_idx in (0..child_node.children.len()).rev() {
            NodeId::idx(child_node_idx).remove_child(child_idx, tree);
        }
        let child_node = tree.nodes[child_node_idx].take().unwrap();
        if child_node_idx > tree.last_open {
            tree.last_open = child_node_idx;
        };

        child_node.data
    }

    /// Swaps the children of the given node at positions `a` and `b`.
    /// # Panics
    /// Panics if `a` or `b` are out of bounds.
    pub fn swap_children(self, a: usize, b: usize, tree: &mut Tree<T>) {
        tree.nodes[self.idx]
            .as_mut()
            .expect("valid NodeId")
            .children
            .swap(a, b);
    }

    /// Removes all children from the node
    pub fn clear(self, tree: &mut Tree<T>) {
        let len = tree.nodes[self.idx].as_ref().unwrap().children.len();
        for i in (0..len).rev() {
            self.remove_child(i, tree);
        }
    }

    /// Returns an iterator to the given node's children's ids.
    /// The tree is borrowed immutably for the duration of the iterator's lifetime.
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

    /// Returns an iterator to the given node's children's ids, without borrowing `tree`
    /// for the duration of the iterator's lifetime. This method allocates memory proportional
    /// to the number of children.
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
}

impl<T> Tree<T> {
    /// Constructs a new `Tree<T>`, with a root node containing the element `data`.
    pub fn new(data: T) -> Self {
        Self {
            nodes: vec![None, Some(Node {
                parent: 0,
                children: Vec::new(),
                data,
            })],
            last_open: 0,
        }
    }

    /// Returns the `NodeId` of the tree's root node. This will always succeed.
    pub fn root(&self) -> NodeId<T> {
        NodeId::idx(1)
    }

    /// Removes the node corresponding to `node_id` from `tree`.
    /// # Panics
    /// Panics if `node_id` corresponds to the root node, which cannot be removed.
    pub fn remove(&mut self, node_id: NodeId<T>) -> T {
        assert_ne!(node_id.idx, 0, "cannot remove root node");
        let parent_idx = self.nodes[node_id.idx].as_ref().expect("valid node_id").parent;
        let parent_children = &mut self.nodes[parent_idx].as_mut().unwrap().children;
        let children_idx = *parent_children.iter().find(|&&x| x == node_id.idx).unwrap();

        node_id.remove_child(children_idx, self)
    }

    /// Find next open site, if available
    fn next_open_site(&mut self) -> Option<usize> {
        // 0 is always full, so this indicates no open site
        if self.last_open == 0 {
            return None;
        }

        // construct an iterator that iterates from last_site to start of array
        let rev_iter = self
            .nodes
            .iter()
            .skip(1)
            .enumerate()
            .rev()
            .skip(self.nodes.len() - self.last_open - 1);

        for (i, x) in rev_iter {
            //if empty site found, we store its position
            //this may enable faster searching later
            if let None = x {
                self.last_open = i;
                return Some(i)
            }
        }

        None
    }

    /// Given two `NodeId`s, returns mutable references to each of their respective elements.
    /// # Panics
    /// Panics if either `a` or `b` are invalid nodes within the given tree.
    #[must_use]
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
