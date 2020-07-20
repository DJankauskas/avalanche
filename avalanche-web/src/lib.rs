#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

use avalanche::{View, Component};
use avalanche::renderer::{Renderer, NativeType, NativeHandle, HasChildrenMarker};
use avalanche::vdom::{VNode, node_with_native_handle};

use avalanche::{InternalContext, shared::Shared};

use std::collections::{HashMap, VecDeque};

use wasm_bindgen::JsCast;
use gloo::events::EventListener;

pub struct Element {
    tag: &'static str,
    on_click: Option<Shared<dyn FnMut()>>,
    children: Vec<View>,
    updates: [u64; 1]
}

#[derive(Default)]
pub struct ElementBuilder {
    tag: Option<&'static str>,
    on_click: Option<Shared<dyn FnMut()>>,
    children: Option<Vec<View>>,
    updates: [u64; 1]
}

impl ElementBuilder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn tag(mut self, tag: &'static str, updated: bool) -> Self {
        self.tag = Some(tag);
        if updated {
            self.updates[0] |= 1;
        };
        self
    }

    pub fn on_click<F: FnMut() + 'static>(mut self, on_click: F, updated: bool) -> Self {
        use std::cell::RefCell;

        //TODO: really convoluted, try replacing FnMut() with Fn(), allowing simple Rc usage
        let on_click: Box<RefCell<dyn FnMut()>> = Box::new(RefCell::new(on_click));
        self.on_click = Some(on_click.into());
        if updated {
            self.updates[0] |= 2;
        };
        self
    }

    pub fn children<T: Into<Vec<View>>>(mut self, children: T, updated: bool) -> Self {
        self.children = Some(children.into());
        if updated {
            self.updates[0] |= 4;
        };
        self
    }

    pub fn build(self) -> Element {
        Element {
            tag: self.tag.unwrap(),
            on_click: self.on_click,
            children: self.children.unwrap_or_default(),
            updates: self.updates
        }
    }
}

impl Component for Element {
    type Builder = ElementBuilder;

    fn render(&self, _: InternalContext) -> View {
        HasChildrenMarker {
            //TODO: take children by Rc?
            children: self.children.clone()
        }.into()
    }

    fn updates(&self) -> &[u64] {
        &self.updates
    }

    fn native_type(&self) -> Option<NativeType> {
        Some(NativeType {
            handler: "oak_web",
            name: self.tag
        })
    }
}

static TIMEOUT_MSG_NAME: &str = "oak_web_message_name";

///Renders the given view in the current document's body.
pub fn mount_to_body(view: View) {
    let renderer = WebRenderer::new();
    
    let vdom = avalanche::vdom::generate_vdom(view, Box::new(renderer));
    // let native_handle = vdom.exec_mut(|vdom| renderer.create_component(vdom));

    // match native_handle {
    //     Some(native_handle) => {
    //         let body = web_sys::window().expect("window").document().expect("document").body().expect("body");
    //         let ref_to_node = &native_handle.downcast_ref::<WebNativeHandle>().expect("WebNativeHandle").node;
    //         body.append_child(&ref_to_node).expect("append node to body");
    //     },
    //     None => {},
    // };

    vdom.exec(|vdom| {
        match node_with_native_handle(vdom.root.clone().unwrap()) {
            Some(vnode) => {
                vnode.exec(|vnode| {
                    //if vnode is Some, it must have a native handle
                    let native_handle = vnode.native_handle.as_ref().unwrap();
                    let body = web_sys::window().expect("window").document().expect("document").body().expect("body");
                        let ref_to_node = &native_handle.downcast_ref::<WebNativeHandle>().expect("WebNativeHandle").node;
                        body.append_child(&ref_to_node).expect("append node to body");
                })
            },
            None => {},
        }
    });

    //TODO: more elegant solution that leaks less memory?
    //TODO: create Shared leak method to avoid extra alloc of Box call?
    Box::leak(Box::new(vdom));
}

struct WebNativeHandle {
    node: web_sys::Node,
    listeners: HashMap<&'static str, EventListener>
}

struct WebRenderer {
    window: web_sys::Window,
    document: web_sys::Document,
    _listener: EventListener,
    queued_fns: Shared<VecDeque<Box<dyn FnOnce()>>>
}

impl WebRenderer {
    fn new() -> Self {
        let window = web_sys::window().unwrap();
        let queued_fns = Shared::default();
        let mut queued_fns_clone = queued_fns.clone();

        //sets up fast execution of 0ms timeouts
        //uses approach in https://dbaron.org/log/20100309-faster-timeouts
        let listener = EventListener::new(&window, "message", move |e| {
            let e = e.clone();
            match e.dyn_into::<web_sys::MessageEvent>() {
                Ok(event) => {
                    if event.data() == TIMEOUT_MSG_NAME {
                        event.stop_propagation();
                        queued_fns_clone.exec_mut(|queue: &mut VecDeque<Box<dyn FnOnce()>>| {
                            match queue.pop_front() {
                                Some(f) => {f()},
                                None => {},
                            }
                        });
                    }
                },
                Err(_) => { /*should not be reachable*/ }
            }
        });
        WebRenderer {
            document: window.document().unwrap(),
            window,
            _listener: listener,
            queued_fns
        }
    }
}

impl Renderer for WebRenderer {
    //TODO: add support for () rendering (important!)
    fn create_component(&mut self, vnode: &VNode) -> Option<NativeHandle> {
        let action = match &vnode.native_type {
            Some(action) => action,
            None => return None,
        };

        let elem = match action.handler.as_ref() {
            "oak_web_text" => {
                let text_node = match vnode.component.downcast_ref::<Text>() {
                    Some(text) => self.document.create_text_node(&text.text),
                    None => panic!(
                        "WebRenderer: expected Text component for oak_web_text."
                    ),
                };
                WebNativeHandle {
                    node: web_sys::Node::from(text_node),
                    listeners: HashMap::new()
                }
            },
            "oak_web" => {
                assert_ne!(action.name, "", "WebRenderer: expected tag name to not be empty.");
                let comp = match vnode.component.downcast_ref::<Element>() {
                    Some(element) => element,
                    None => panic!(
                        "WebRenderer: expected WebNativeData data for oak_web handled NativeAction."
                    ),
                };

                let element = self.document.create_element(&action.name).expect(
                    "WebRenderer: element creation failed from syntax error."
                );

                let mut listeners = HashMap::new();

                //TODO: attributes
                // for (prop_name, value) in comp.attrs.iter() {
                //     element.set_attribute(prop_name, value).unwrap();
                // }

                // add_listeners(&element, &comp.listeners, &mut listeners);
                if let Some(handler) = &comp.on_click {
                    add_listener(&element, "click", handler.clone(), &mut listeners);
                }

                for with_handle in vnode.children.iter().filter_map(|child| node_with_native_handle(child.clone())) {
                    with_handle.exec(|with_handle| {
                        let handle = with_handle.native_handle.as_ref().unwrap();
                        element.append_child(&handle.downcast_ref::<WebNativeHandle>().expect("WebNativeHandle").node).unwrap();
                    })
                }
            

                WebNativeHandle {
                    node: web_sys::Node::from(element),
                    listeners
                }
            },
            _ => panic!("Custom handlers not implemented yet.")
        };

        Some(Box::new(elem))
    }
    
    fn update_component(
        &mut self, 
        native_type: &NativeType, 
        native_handle: &mut NativeHandle, 
        _old_comp: &View,
        new_comp: &View,
        children: &[Shared<VNode>]
    ) {
        let web_handle = native_handle.downcast_mut::<WebNativeHandle>().unwrap();
        match native_type.handler.as_ref() {
            "oak_web" => {
                let node = web_handle.node.clone();
                let element = node.dyn_into::<web_sys::Element>().unwrap();
                let new_comp = new_comp.downcast_ref::<Element>().unwrap();

                //old diff code from oak
                //currently not relevant due to HashMap approach
                // for (key, old_value) in old_comp.attrs.iter() {
                //     match new_comp.attrs.get(key) {
                //         Some(new_value) => {
                //             //if new value is different from old, update DOM
                //             if old_value != new_value {
                //                 element.set_attribute(key, &new_value).unwrap();
                //             }
                //         },
                //         None => {
                //             //new props don't have attribute `key`
                //             //thus remove it from element
                //             element.remove_attribute(key).unwrap();
                //         }
                //     }
                // }

                // //check new props for attrs not present in old props
                // //add those new props to the element
                // for (key, value) in new_comp.attrs.iter() {
                //     if old_comp.attrs.get(key).is_none() {
                //         element.set_attribute(key, value).unwrap();
                //     }
                // }

                // //TODO: properly diff listeners
                // //currently, they are all removed
                // //and new ones from new_props are added
                // web_handle.listeners.clear();
                // add_listeners(&element, &new_comp.listeners, &mut web_handle.listeners);

                //on_click
                if new_comp.updates[0] & 2 != 0 {
                    web_handle.listeners.remove("click");
                    if let Some(handler) = &new_comp.on_click {
                        add_listener(&element, "click", handler.clone(), &mut web_handle.listeners);
                    }
                }


                //TODO: diffing algo
                element.set_inner_html("");
                //copy-pasted from create code: factor out or replace with diffing
                for with_handle in children.iter().filter_map(|child| node_with_native_handle(child.clone())) {
                    with_handle.exec(|with_handle| {
                        let handle = with_handle.native_handle.as_ref().unwrap();
                        element.append_child(&handle.downcast_ref::<WebNativeHandle>().expect("WebNativeHandle").node).unwrap();
                    })
                }
            },
            "oak_web_text" => {
                let new_text = new_comp.downcast_ref::<Text>().expect("Text component");
                if new_text.updates[0] & 1 != 0 {
                    //TODO: compare with old text?
                    web_handle.node.set_text_content(Some(&new_text.text));
                }
            },
            _ => panic!("Custom handlers not implemented yet.")
        };
    }

    //TODO: check for custom handler
    fn remove_component(&mut self, vnode: &mut VNode) {
        match &vnode.native_handle {
            Some(handle) => {
                let node = &handle.downcast_ref::<WebNativeHandle>().unwrap().node;
                match node.parent_node() {
                    Some(parent) => {
                        parent.remove_child(node).expect("Remove from parent");
                    },
                    None => {}
                }
            },
            None => {}
        }
    }

    fn schedule_on_ui_thread(&mut self, f: Box<dyn FnOnce()>) {
        //post message for 0ms timeouts
        //technique from https://dbaron.org/log/20100309-faster-timeouts
        self.queued_fns.exec_mut(move |queue| {
            queue.push_back(f);
        });
        self.window.post_message(&TIMEOUT_MSG_NAME.into(), "*").unwrap();
    }
}

fn add_listener(
    element: &web_sys::Element,
    name: &'static str,
    mut callback: Shared<dyn FnMut()>,
    listeners: &mut HashMap<&'static str, EventListener>
) {
    let listener = EventListener::new(
        &element, name, 
        move |_event| callback.exec_mut(|f| f())
    );
    listeners.insert(name, listener);
}

///Represents a text node.
#[derive(Clone, PartialEq)]
pub struct Text {
    text: String,
    updates: [u64; 1]
}
#[derive(Default)]
pub struct TextBuilder {
    text: Option<String>,
    updates: [u64; 1]
}

impl TextBuilder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn text<T: ToString>(mut self, text: T, updated: bool) -> Self {
        self.text = Some(text.to_string());
        if updated {
            self.updates[0] |= 1;
        };
        self
    }

    pub fn build(self) -> Text {
        Text {
            text: self.text.unwrap(),
            updates: self.updates
        }
    }
}

impl Component for Text {
    type Builder = TextBuilder;

    fn render(&self, _: InternalContext) -> View {
        ().into()
    }
    fn native_type(&self) -> Option<NativeType> {
        let action = NativeType {
            handler: "oak_web_text",
            name: "",
        };

        Some(action)
    }

    fn updates(&self) -> &[u64] {
        &self.updates
    }
}
