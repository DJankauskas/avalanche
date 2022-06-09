use std::{collections::HashMap, rc::Rc};

use crate::{impl_any_ref, renderer::NativeType, shared::Shared, Component, View};

use super::native_repr::Repr;

/// Internal node state.
struct NodeInner {
    parent: Option<Node>,
    children: Vec<Node>,
    name: String,
    value: String,
    on_click: Option<Rc<dyn Fn()>>,
}

/// A very simple and limited emulation of a retained UI node.
/// Note that methods are not meant to be efficient or well-implemented,
/// just simple and quick to test.
#[derive(Clone)]
pub(super) struct Node(Shared<NodeInner>);

/// Root of the mocked components, used to trigger events.
pub(super) struct Root {
    nodes: HashMap<String, Node>,
}

impl Root {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
        }
    }

    /// Creates a new node associated with the root. `name`
    /// must be unique within the root.
    pub fn create_node(&mut self, name: &str) -> Node {
        let node = Node(Shared::new(NodeInner {
            parent: None,
            children: Vec::new(),
            name: name.to_string(),
            value: String::new(),
            on_click: None,
        }));
        // TODO: debug code included, potentially remove
        if self.nodes.insert(name.to_string(), node.clone()).is_some() {
            for key in self.nodes.keys() {
                println!("Key {} with len {}", key, key.len());
            }
            panic!(
                "Expected created nodes to have unique names, reused {}",
                name
            );
        };
        node
    }

    /// Gets the node associated with the given name in the root.
    pub fn get_node(&self, name: &str) -> Node {
        self.nodes[name].clone()
    }
}

impl Node {
    /// Sets the value of the component.
    pub fn set_value(&self, value: String) {
        self.0.exec_mut(|node| node.value = value);
    }

    /// Sets the `on_click` hander.
    pub fn set_on_click(&self, on_click: Rc<dyn Fn()>) {
        self.0.exec_mut(|node| node.on_click = Some(on_click));
    }

    /// Inserts the child node at the position, does not handle complex cases
    /// like an ancestor becoming a descendent or a node becoming its own parent. Avoid
    /// testing these cases.
    pub fn insert_child(&self, child: Node, pos: usize) {
        let old_len = self.children_len();
        remove_from_parent(child.clone());
        child.0.exec_mut(|child| child.parent = Some(self.clone()));
        self.0.exec_mut(|parent| parent.children.insert(pos, child));
        assert_eq!(old_len + 1, self.children_len());
    }

    /// Swaps the two children with the given indices within the component.
    pub fn swap_children(&self, a: usize, b: usize) {
        self.0.exec_mut(move |node| node.children.swap(a, b));
    }

    /// Returns the number of children.
    pub fn children_len(&self) -> usize {
        self.0.exec(|node| node.children.len())
    }

    /// Removes the child with the given position from the component.
    pub fn remove_child(&self, pos: usize) {
        let old_len = self.children_len();
        let to_remove = self.0.exec(|node| node.children[pos].clone());
        remove_from_parent(to_remove);
        assert_eq!(old_len - 1, self.children_len());
    }

    /// Creates a comparable and debuggable representation of the component.
    pub fn to_repr(&self) -> Repr {
        self.0.exec(|node| Repr {
            children: node.children.iter().map(Node::to_repr).collect(),
            name: node.name.clone(),
            value: node.value.clone(),
            has_on_click: node.on_click.is_some(),
        })
    }

    /// Executes the node's click handler.
    pub fn click(&self) {
        // Create a click clone outside the component borrow so that if the handler borrows the node,
        // we don't get a runtime panic.
        let on_click = self
            .0
            .exec(|node| (node.on_click.as_ref().expect("provided handler")).clone());
        on_click();
    }
}

/// A helper function that removes the given node from its parent, if it has one.
fn remove_from_parent(node: Node) {
    node.0.exec_mut(|node_inner| {
        if let Some(parent) = &mut node_inner.parent {
            parent.0.exec_mut(|parent| {
                parent.children.retain(|child| !child.0.ptr_eq(&node.0));
            });
            node_inner.parent = None;
        }
    })
}

/// An avalanche component that allows the creation of `Node`s. This struct serves as its own builder.
pub(super) struct Native<'a> {
    pub name: &'a str,
    pub value: &'a str,
    pub on_click: Option<Box<dyn Fn() + 'a>>,
    pub children: Vec<View>,
    key: Option<String>,
    location: (u32, u32),
    updated: u8,
}

impl<'a> Native<'a> {
    pub fn new() -> Self {
        Self {
            name: "",
            value: "",
            on_click: None,
            children: Vec::new(),
            key: None,
            location: (0, 0),
            updated: 0,
        }
    }

    pub fn name(mut self, name: &'a str, updated: bool) -> Self {
        self.name = name;
        if updated {
            self.updated = self.updated | 1;
        }
        self
    }

    pub fn value(mut self, value: &'a str, updated: bool) -> Self {
        self.value = value;
        if updated {
            self.updated = self.updated | 2;
        }
        self
    }

    pub fn on_click(mut self, on_click: impl Fn() + 'a, updated: bool) -> Self {
        self.on_click = Some(Box::new(on_click));
        if updated {
            self.updated = self.updated | 4;
        }
        self
    }

    pub fn children(mut self, children: Vec<View>, updated: bool) -> Self {
        self.children = children;
        if updated {
            self.updated = self.updated | 8;
        }
        self
    }

    pub fn __last(self, children: Vec<View>, updated: bool) -> Self {
        self.children(children, updated)
    }

    pub fn key(mut self, key: String, _updated: bool) -> Self {
        self.key = Some(key);
        self
    }

    pub fn name_updated(&self) -> bool {
        (self.updated & 1) != 0
    }

    pub fn value_updated(&self) -> bool {
        (self.updated & 2) != 0
    }

    pub fn build(mut self, location: (u32, u32)) -> Self {
        self.location = location;
        self
    }
}

impl_any_ref!(Native<'a>);

impl<'a> Component<'a> for Native<'a> {
    type Builder = Self;

    fn render(self, _: crate::RenderContext, _: crate::HookContext) -> View {
        unimplemented!()
    }

    fn updated(&self) -> bool {
        self.updated != 0
    }

    fn children(self) -> Vec<View> {
        self.children
    }

    fn native_type(&self) -> Option<NativeType> {
        Some(NativeType {
            handler: "test",
            name: "test",
        })
    }

    fn location(&self) -> Option<(u32, u32)> {
        Some(self.location)
    }

    fn key(&self) -> Option<String> {
        self.key.clone()
    }
}
