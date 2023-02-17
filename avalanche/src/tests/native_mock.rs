use std::{collections::HashMap, rc::Rc};

use crate::{
    renderer::{NativeType, NativeEvent, Renderer},
    shared::{Shared, WeakShared},
    tracked::Gen,
    Component, View,
};

use super::{native_repr::Repr, renderer::TestRenderer};

/// Internal node state.
struct NodeInner {
    parent: Option<WeakShared<NodeInner>>,
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
        child
            .0
            .exec_mut(|child| child.parent = Some(self.0.downgrade()));
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
            parent.upgrade().unwrap().exec_mut(|parent| {
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
    gens: [Gen<'a>; 4],
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
            gens: [Gen::escape_hatch_new(false); 4],
        }
    }

    pub fn name(mut self, name: &'a str, gen: Gen<'a>) -> Self {
        self.name = name;
        self.gens[0] = gen;
        self
    }

    pub fn value(mut self, value: &'a str, gen: Gen<'a>) -> Self {
        self.value = value;
        self.gens[1] = gen;
        self
    }

    pub fn on_click(mut self, on_click: impl Fn() + 'a, gen: Gen<'a>) -> Self {
        self.on_click = Some(Box::new(on_click));
        self.gens[2] = gen;
        self
    }

    pub fn children(mut self, children: Vec<View>, gen: Gen<'a>) -> Self {
        self.children = children;
        self.gens[3] = gen;
        self
    }

    pub fn __last(self, children: Vec<View>, gen: Gen<'a>) -> Self {
        self.children(children, gen)
    }

    pub fn key(mut self, key: String, _gen: Gen<'a>) -> Self {
        self.key = Some(key);
        self
    }

    // pub fn name_updated(&self, curr_gen: Gen<'a>) -> bool {
    //     self.gens[0] >= curr_gen
    // }

    pub fn value_updated(&self, curr_gen: Gen<'a>) -> bool {
        self.gens[1] >= curr_gen
    }

    pub fn build(mut self, location: (u32, u32)) -> Self {
        self.location = location;
        self
    }
}

impl<'a> Component<'a> for Native<'a> {
    type Builder = Self;

    fn render(self, _: crate::RenderContext, _: crate::HookContext) -> View {
        unimplemented!()
    }

    fn updated(&self, curr_gen: Gen) -> bool {
        for gen in self.gens {
            if gen >= curr_gen {
                return true;
            }
        }
        false
    }
    
    fn native_create(
            &self,
            renderer: &mut dyn Renderer,
            dispatch_native_event: crate::renderer::DispatchNativeEvent,
        ) -> crate::renderer::NativeHandle {
            let renderer = renderer.downcast_ref::<TestRenderer>().unwrap();
            let node = renderer.root.exec_mut(|root| root.create_node(self.name));
            node.set_value(self.value.to_string());
            if self.on_click.is_some() {
                node.set_on_click(Rc::new(move || {
                    dispatch_native_event.dispatch(NativeEvent {
                        event: Box::new(()),
                        name: "click",
                    })
                }));
            };
            Box::new(node)
    }

    fn native_update(
            self,
            _renderer: &mut dyn Renderer,
            _native_type: &NativeType,
            native_handle: &mut crate::renderer::NativeHandle,
            curr_gen: Gen,
            event: Option<crate::renderer::NativeEvent>,
        ) -> Vec<View> {
        let handle = native_handle.downcast_ref::<Node>().unwrap();
        if let (Some(_), Some(on_click)) = (event, &self.on_click) {
            on_click();
        }
        // TODO: uncomment when a create/update model is created where a component will not
        // immediately be updated after creation
        // if component.name_updated() {
        //     panic!("Names must be static but {} was updated", component.name);
        // }
        if self.value_updated(curr_gen) {
            handle.set_value(self.value.to_string());
        }
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
