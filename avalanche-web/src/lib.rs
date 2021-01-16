use avalanche::renderer::{NativeHandle, NativeType, Renderer};
use avalanche::vdom::VNode;
use avalanche::{Component, View};

use avalanche::{shared::Shared};

use std::rc::Rc;
use std::collections::{HashMap, VecDeque};

use crate::events::Event;
use crate::components::{Text, RawElement, Attr};
use gloo::events::{EventListener, EventListenerOptions};
use wasm_bindgen::JsCast;

pub mod events;
pub mod components;

static TIMEOUT_MSG_NAME: &str = "oak_web_message_name";

/// Renders the given view in the current document's body.
pub fn mount_to_body<C: Component + Default>() {
    let renderer = WebRenderer::new();
    let root = avalanche::vdom::Root::new(C::default().into(), renderer);

    root.native_handle(|native_handle| {
        if let Some(native_handle) = native_handle {
            let body = web_sys::window()
                .expect("window")
                .document()
                .expect("document")
                .body()
                .expect("body");
            let ref_to_node = &native_handle
                .downcast_ref::<WebNativeHandle>()
                .expect("WebNativeHandle")
                .node;
            body.append_child(&ref_to_node)
                .expect("append node to body");
        }
    });

    // TODO: more elegant solution that leaks less memory?
    Box::leak(Box::new(root));
}

struct WebNativeHandle {
    node: web_sys::Node,
    listeners: HashMap<&'static str, EventListener>,
}

struct WebRenderer {
    window: web_sys::Window,
    document: web_sys::Document,
    _listener: EventListener,
    queued_fns: Shared<VecDeque<Box<dyn FnOnce()>>>,
}

impl WebRenderer {
    fn new() -> Self {
        let window = web_sys::window().unwrap();
        let queued_fns = Shared::default();
        let queued_fns_clone = queued_fns.clone();

        // sets up fast execution of 0ms timeouts
        // uses approach in https://dbaron.org/log/20100309-faster-timeouts
        let listener = EventListener::new(&window, "message", move |e| {
            let e = e.clone();
            match e.dyn_into::<web_sys::MessageEvent>() {
                Ok(event) => {
                    if event.data() == TIMEOUT_MSG_NAME {
                        event.stop_propagation();
                        queued_fns_clone.exec_mut(|queue: &mut VecDeque<Box<dyn FnOnce()>>| {
                            match queue.pop_front() {
                                Some(f) => f(),
                                None => {}
                            }
                        });
                    }
                }
                Err(_) => { /*should not be reachable*/ }
            }
        });
        WebRenderer {
            document: window.document().unwrap(),
            window,
            _listener: listener,
            queued_fns,
        }
    }

    fn get_child(parent: &web_sys::Element, child_idx: usize) -> web_sys::Node {
        // TODO: remove debug info
        Self::try_get_child(parent, child_idx).expect(&format!("{}", child_idx))
    }

    fn try_get_child(parent: &web_sys::Element, child_idx: usize) -> Option<web_sys::Node> {
        parent.child_nodes().item(child_idx as u32)
    }

    fn assert_handler_oak_web(native_type: &NativeType) {
        assert_eq!(
            native_type.handler, "oak_web",
            "handler is not of type \"oak web\""
        )
    }

    fn handle_to_node(native_handle: &NativeHandle) -> web_sys::Node {
        let web_native = native_handle
            .downcast_ref::<WebNativeHandle>()
            .expect("WebNativeHandle");
        web_native.node.clone()
    }

    fn handle_to_element(native_handle: &NativeHandle) -> web_sys::Element {
        let node = Self::handle_to_node(native_handle);
        node.dyn_into::<web_sys::Element>()
            .expect("Element (not Text node)")
    }
}

impl Renderer for WebRenderer {
    // TODO: add support for () rendering (important!)
    fn create_component(&mut self, native_type: &NativeType, component: &View) -> NativeHandle {
        let elem = match native_type.handler.as_ref() {
            "oak_web_text" => {
                let text_node = match component.downcast_ref::<Text>() {
                    Some(text) => self.document.create_text_node(&text.text),
                    None => panic!("WebRenderer: expected Text component for oak_web_text."),
                };
                WebNativeHandle {
                    node: web_sys::Node::from(text_node),
                    listeners: HashMap::new(),
                }
            }
            "oak_web" => {
                assert_ne!(
                    native_type.name, "",
                    "WebRenderer: expected tag name to not be empty."
                );
                let raw_element = component
                    .downcast_ref::<RawElement>()
                    .expect("component of type RawElement");

                let element = self
                    .document
                    .create_element(&native_type.name)
                    .expect("WebRenderer: element creation failed from syntax error.");

                let mut listeners = HashMap::new();

                match raw_element.tag {
                    "input" => {
                        let input_element = element
                            .clone()
                            .dyn_into::<web_sys::HtmlInputElement>()
                            .expect("HTMLInputElement");
                        for (name, attr) in raw_element.attrs.iter() {
                            if let Some(attr) = &attr.0 {
                                match attr {
                                    Attr::Prop(prop) => match *name {
                                        "value" => {
                                            input_element.set_value(&prop);
                                        }
                                        "checked" => {
                                            input_element.set_checked(prop != "")
                                        }
                                        _ => {
                                            input_element
                                            .set_attribute(name, &prop)
                                            .unwrap();
                                        }
                                    }
                                    Attr::Handler(handler) => match *name {
                                        "input" if raw_element.is_controlled => {
                                            add_listener_prevent_default(
                                                &element,
                                                name,
                                                handler.clone(),
                                                &mut listeners,
                                            );
                                        }
                                        _ => {
                                            add_listener(
                                                &element,
                                                name,
                                                handler.clone(),
                                                &mut listeners,
                                            );
                                        }
                                    },
                                }
                            }
                        }
                    }
                    "textarea" => {
                        let text_area_element = element
                            .clone()
                            .dyn_into::<web_sys::HtmlTextAreaElement>()
                            .expect("HTMLTextAreaElement");
                        for (name, attr) in raw_element.attrs.iter() {
                            if let Some(attr) = &attr.0 {
                                match attr {
                                    Attr::Prop(prop) => match *name {
                                        "value" => text_area_element.set_value(prop),
                                        _ => text_area_element.set_attribute(name, &prop).unwrap(),
                                    },
                                    Attr::Handler(handler) => match *name {
                                        "input" if raw_element.is_controlled => {
                                            add_listener_prevent_default(
                                                &element,
                                                name,
                                                handler.clone(),
                                                &mut listeners,
                                            );
                                        }
                                        _ => {
                                            add_listener(
                                                &element,
                                                name,
                                                handler.clone(),
                                                &mut listeners,
                                            );
                                        }
                                    },
                                }
                            }
                        }
                    }
                    _ => {
                        for (name, attr) in raw_element.attrs.iter() {
                            if let Some(attr) = &attr.0 {
                                match attr {
                                    Attr::Prop(prop) => {
                                        element.set_attribute(name, &prop).unwrap();
                                    }
                                    Attr::Handler(handler) => {
                                        add_listener(
                                            &element,
                                            name,
                                            handler.clone(),
                                            &mut listeners,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }

                WebNativeHandle {
                    node: web_sys::Node::from(element),
                    listeners,
                }
            }
            _ => panic!("Custom handlers not implemented yet."),
        };

        Box::new(elem)
    }

    fn update_component(
        &mut self,
        native_type: &NativeType,
        native_handle: &mut NativeHandle,
        component: &View,
    ) {
        let web_handle = native_handle.downcast_mut::<WebNativeHandle>().unwrap();
        match native_type.handler.as_ref() {
            "oak_web" => {
                let node = web_handle.node.clone();
                let element = node.dyn_into::<web_sys::Element>().unwrap();
                let raw_element = component
                    .downcast_ref::<RawElement>()
                    .expect("component of type RawElement");

                if raw_element.attrs_updated {
                    match raw_element.tag {
                        "input" => {
                            let input_element = element
                                .clone()
                                .dyn_into::<web_sys::HtmlInputElement>()
                                .expect("HTMLInputElement");
                            for (name, (attr, updated)) in raw_element.attrs.iter() {
                                if *updated {
                                    match attr {
                                        Some(attr) => match attr {
                                            Attr::Prop(prop) => {
                                                match *name {
                                                    "value" => {
                                                        input_element.set_value(&prop);
                                                    }
                                                    "checked" => {
                                                        input_element.set_checked(prop != "")
                                                    }
                                                    _ => {
                                                        input_element
                                                        .set_attribute(name, &prop)
                                                        .unwrap();
                                                    }
                                                }
                                            }
                                            Attr::Handler(handler) => {
                                                if raw_element.is_controlled && *name == "input" {
                                                    update_listener_prevent_default(
                                                        &element,
                                                        name,
                                                        handler.clone(),
                                                        &mut web_handle.listeners,
                                                    );
                                                } else {
                                                    update_listener(
                                                        &element,
                                                        name,
                                                        handler.clone(),
                                                        &mut web_handle.listeners,
                                                    );
                                                }
                                            }
                                        },
                                        None => {
                                            remove_listener(name, &mut web_handle.listeners);
                                        }
                                    }
                                }
                            }
                        }
                        "textarea" => {
                            let text_area_element = element
                                .clone()
                                .dyn_into::<web_sys::HtmlTextAreaElement>()
                                .expect("HTMLTextAreaElement");
                            for (name, (attr, updated)) in raw_element.attrs.iter() {
                                if *updated {
                                    match attr {
                                        Some(attr) => match attr {
                                            Attr::Prop(prop) => {
                                                if *name == "value" {
                                                    text_area_element.set_value(&prop);
                                                } else {
                                                    text_area_element
                                                        .set_attribute(name, &prop)
                                                        .unwrap();
                                                }
                                            }
                                            Attr::Handler(handler) => {
                                                if raw_element.is_controlled && *name == "input" {
                                                    update_listener_prevent_default(
                                                        &element,
                                                        name,
                                                        handler.clone(),
                                                        &mut web_handle.listeners,
                                                    );
                                                } else {
                                                    update_listener(
                                                        &element,
                                                        name,
                                                        handler.clone(),
                                                        &mut web_handle.listeners,
                                                    );
                                                }
                                            }
                                        },
                                        None => {
                                            remove_listener(name, &mut web_handle.listeners);
                                        }
                                    }
                                }
                            }
                        }
                        _ => {
                            for (name, (attr, updated)) in raw_element.attrs.iter() {
                                if *updated {
                                    match attr {
                                        Some(attr) => match attr {
                                            Attr::Prop(prop) => {
                                                element.set_attribute(name, &prop).unwrap();
                                            }
                                            Attr::Handler(handler) => {
                                                update_listener(
                                                    &element,
                                                    name,
                                                    handler.clone(),
                                                    &mut web_handle.listeners,
                                                );
                                            }
                                        },
                                        None => {
                                            remove_listener(name, &mut web_handle.listeners);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            "oak_web_text" => {
                let new_text = component.downcast_ref::<Text>().expect("Text component");
                if new_text.updated() {
                    //TODO: compare with old text?
                    web_handle.node.set_text_content(Some(&new_text.text));
                }
            }
            _ => panic!("Custom handlers not implemented yet."),
        };
    }

    // TODO: check for custom handler
    fn remove_component(&mut self, vnode: &mut VNode) {
        match &vnode.native_handle {
            Some(handle) => {
                let node = &handle.downcast_ref::<WebNativeHandle>().unwrap().node;
                match node.parent_node() {
                    Some(parent) => {
                        parent.remove_child(node).expect("Remove from parent");
                    }
                    None => {}
                }
            }
            None => {}
        }
    }

    fn schedule_on_ui_thread(&mut self, f: Box<dyn FnOnce()>) {
        // post message for 0ms timeouts
        // technique from https://dbaron.org/log/20100309-faster-timeouts
        self.queued_fns.exec_mut(move |queue| {
            queue.push_back(f);
        });
        self.window
            .post_message(&TIMEOUT_MSG_NAME.into(), "*")
            .unwrap();
    }

    fn append_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        _child_type: &NativeType,
        child_handle: &NativeHandle,
    ) {
        Self::assert_handler_oak_web(parent_type);
        let parent_element = Self::handle_to_element(parent_handle);
        let child_node = Self::handle_to_node(child_handle);
        parent_element
            .append_with_node_1(&child_node)
            .expect("append success");
    }

    fn insert_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        index: usize,
        _child_type: &NativeType,
        child_handle: &NativeHandle,
    ) {
        self.log("inserting child");
        Self::assert_handler_oak_web(parent_type);
        let parent_element = Self::handle_to_element(parent_handle);
        let child_node = Self::handle_to_node(child_handle);
        let component_after = Self::try_get_child(&parent_element, index);
        parent_element
            .insert_before(&child_node, component_after.as_ref())
            .expect("insert success");
    }

    fn swap_children(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        a: usize,
        b: usize,
    ) {
        Self::assert_handler_oak_web(parent_type);
        let parent_element = Self::handle_to_element(parent_handle);
        let lesser = std::cmp::min(a, b);
        let greater = std::cmp::max(a, b);

        // TODO: throw exception if a and b are equal but out of bounds?
        if a != b {
            let a = Self::get_child(&parent_element, lesser);
            let b = Self::get_child(&parent_element, greater);
            let after_b = b.next_sibling();
            // note: idiosyncratic order, a is being replaced with b
            parent_element
                .replace_child(&b, &a)
                .expect("replace succeeded");
            parent_element
                .insert_before(&a, after_b.as_ref())
                .expect("insert succeeded");
        }
    }

    fn replace_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        index: usize,
        _child_type: &NativeType,
        child_handle: &NativeHandle,
    ) {
        Self::assert_handler_oak_web(parent_type);
        let parent_element = Self::handle_to_element(parent_handle);
        let curr_child_node = Self::get_child(&parent_element, index);
        let replace_child_node = Self::handle_to_node(child_handle);
        if curr_child_node != replace_child_node {
            parent_element
                .replace_child(&curr_child_node, &replace_child_node)
                .expect("successful replace");
        }
    }

    fn move_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        old: usize,
        new: usize,
    ) {
        Self::assert_handler_oak_web(parent_type);
        let parent_element = Self::handle_to_element(parent_handle);
        let curr_child_node = Self::get_child(&parent_element, old);
        let removed = parent_element
            .remove_child(&curr_child_node)
            .expect("successful remove");
        let component_after_insert = Self::try_get_child(&parent_element, new);
        parent_element
            .insert_before(&removed, component_after_insert.as_ref())
            .expect("insert success");
    }

    fn remove_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        index: usize,
    ) {
        Self::assert_handler_oak_web(parent_type);
        let parent_element = Self::handle_to_element(parent_handle);
        let child_node = Self::get_child(&parent_element, index);
        parent_element
            .remove_child(&child_node)
            .expect("successful remove");
    }

    fn log(&self, string: &str) {
        let js_val: wasm_bindgen::JsValue = string.into();
        web_sys::console::log_1(&js_val);
    }
}

fn add_listener(
    element: &web_sys::Element,
    name: &'static str,
    callback: Rc<dyn Fn(Event)>,
    listeners: &mut HashMap<&'static str, EventListener>,
) {
    let listener = EventListener::new(&element, name, move |event| callback(event.clone()));
    listeners.insert(name, listener);
}

fn add_listener_prevent_default(
    element: &web_sys::Element,
    name: &'static str,
    callback: Rc<dyn Fn(Event)>,
    listeners: &mut HashMap<&'static str, EventListener>,
) {
    let mut options = EventListenerOptions::default();
    options.passive = false;
    let listener = EventListener::new_with_options(&element, name, options, move |event| {
        event.prevent_default();
        callback(event.clone())
    });
    listeners.insert(name, listener);
}

fn update_listener(
    element: &web_sys::Element,
    name: &'static str,
    callback: Rc<dyn Fn(Event)>,
    listeners: &mut HashMap<&'static str, EventListener>,
) {
    let _ = listeners.remove(name);
    let listener = EventListener::new(&element, name, move |event| callback(event.clone()));
    listeners.insert(name, listener);
}

fn update_listener_prevent_default(
    element: &web_sys::Element,
    name: &'static str,
    callback: Rc<dyn Fn(Event)>,
    listeners: &mut HashMap<&'static str, EventListener>,
) {
    let _ = listeners.remove(name);
    let listener = EventListener::new(&element, name, move |event| {
        event.prevent_default();
        callback(event.clone());
    });
    listeners.insert(name, listener);
}

fn remove_listener(name: &'static str, listeners: &mut HashMap<&'static str, EventListener>) {
    let _ = listeners.remove(name);
}