use std::marker::PhantomData;

pub struct Tree<T> {
    nodes: Vec<Option<Node<T>>>,
    ///from the end, the first open site
    ///if 0, signifies none open
    last_open: usize,
}

pub struct Node<T> {
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
            phantom: self.phantom
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

    pub fn append(self, data: T, tree: &mut Tree<T>) -> Self {
        let node = Node {
            parent: self.idx,
            data,
            children: Vec::new(),
        };

        let id = match tree.next_open_site() {
            Some(idx) => {
                tree.nodes[idx] = Some(node);
                NodeId::idx(idx)
            }
            None => {
                let id = NodeId::idx(tree.nodes.len());
                tree.nodes.push(Some(node));
                id
            }
        };

        tree.nodes[self.idx]
            .as_mut()
            .expect("valid NodeId")
            .children
            .push(id.idx);
        id
    }

    ///Removes the specified child by index
    pub fn remove(self, child: usize, tree: &mut Tree<T>) -> T {
        let idx = tree.nodes[self.idx]
            .as_ref()
            .expect("valid self")
            .children
            .get(child)
            .expect("valid child index");

        tree.remove(NodeId::idx(*idx))
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
}
