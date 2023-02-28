use avalanche::renderer::{
    DispatchNativeEvent, NativeEvent, NativeHandle, NativeType, Renderer, Scheduler,
};
use avalanche::shared::Shared;
use avalanche::vdom::Root;
use avalanche::DefaultComponent;

use std::collections::{HashMap, VecDeque};

use crate::events::Event;
use gloo_events::{EventListener, EventListenerOptions};
use wasm_bindgen::JsCast;
use web_sys::{Element, EventTarget};

pub mod components;
pub mod events;

static TIMEOUT_MSG_NAME: &str = "avalanche_web_message_name";

/// Renders the given component onto the `element` parameter.
///
/// To unmount the component, use the returned [Root].
pub fn mount<C: DefaultComponent<'static>>(element: Element) -> Root {
    let renderer = WebRenderer::new();
    let scheduler = WebScheduler::new();
    let native_parent_type = NativeType {
        handler: "avalanche_web",
        name: "avalanche_web",
    };
    let native_parent_handle = WebNativeHandle {
        children_offset: element.child_nodes().length(),
        node: element.into(),
        _listeners: Default::default(),
    };

    let root = avalanche::vdom::Root::new::<_, _, C>(
        native_parent_type,
        Box::new(native_parent_handle),
        renderer,
        scheduler,
    );

    root
}

/// Renders the given component in the current document's body.
///
/// To unmount the component, use the returned [Root].
pub fn mount_to_body<C: DefaultComponent<'static>>() -> Root {
    let body = web_sys::window()
        .expect("window")
        .document()
        .expect("document")
        .body()
        .expect("body");
    mount::<C>(body.into())
}

struct WebScheduler {
    window: web_sys::Window,
    queued_fns: Shared<VecDeque<Box<dyn FnOnce()>>>,
    _listener: EventListener,
}

impl WebScheduler {
    fn new() -> Self {
        let window = web_sys::window().unwrap();
        let queued_fns = Shared::default();
        let queued_fns_clone = queued_fns.clone();

        // sets up fast execution of 0ms timeouts
        // uses approach in https://dbaron.org/log/20100309-faster-timeouts
        let _listener = EventListener::new(&window, "message", move |e| {
            let e = e.clone();
            if let Ok(event) = e.dyn_into::<web_sys::MessageEvent>() {
                if event.data() == TIMEOUT_MSG_NAME {
                    event.stop_propagation();
                    // f may call schedule_on_ui_thread, so it must be called outside of exec_mut
                    let f = queued_fns_clone
                        .exec_mut(|queue: &mut VecDeque<Box<dyn FnOnce()>>| queue.pop_front());
                    if let Some(f) = f {
                        f();
                    }
                }
            }
        });

        WebScheduler {
            window,
            queued_fns,
            _listener,
        }
    }
}

impl Scheduler for WebScheduler {
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
}

struct WebNativeHandle {
    node: web_sys::Node,
    _listeners: HashMap<&'static str, EventListener>,
    /// position at which renderer indexing should begin
    // TODO: more memory-efficient implementation?
    children_offset: u32,
}

struct WebRenderer {
    document: web_sys::Document,
}

impl WebRenderer {
    fn new() -> Self {
        WebRenderer {
            document: web_sys::window().unwrap().document().unwrap(),
        }
    }

    fn get_child(parent: &web_sys::Element, child_idx: usize, offset: u32) -> web_sys::Node {
        Self::try_get_child(parent, child_idx, offset).unwrap()
    }

    fn try_get_child(
        parent: &web_sys::Element,
        child_idx: usize,
        offset: u32,
    ) -> Option<web_sys::Node> {
        parent.child_nodes().item(child_idx as u32 + offset)
    }

    fn assert_handler_avalanche_web(native_type: &NativeType) {
        assert_eq!(
            native_type.handler, "avalanche_web",
            "handler is not of type \"avalanche_web\""
        )
    }

    fn handle_cast(native_handle: &NativeHandle) -> &WebNativeHandle {
        native_handle
            .downcast_ref::<WebNativeHandle>()
            .expect("WebNativeHandle")
    }

    fn node_to_element(node: web_sys::Node) -> web_sys::Element {
        node.dyn_into::<web_sys::Element>()
            .expect("Element (not Text node)")
    }
}

impl Renderer for WebRenderer {
    fn append_child(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        _child_type: &NativeType,
        child_handle: &NativeHandle,
    ) {
        Self::assert_handler_avalanche_web(parent_type);
        let parent_node = Self::handle_cast(parent_handle).node.clone();
        let parent_element = Self::node_to_element(parent_node);
        let child_node = &Self::handle_cast(child_handle).node;
        parent_element
            .append_with_node_1(child_node)
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
        Self::assert_handler_avalanche_web(parent_type);
        let parent_handle = Self::handle_cast(parent_handle);
        let parent_element = Self::node_to_element(parent_handle.node.clone());
        let child_node = &Self::handle_cast(child_handle).node;
        let component_after =
            Self::try_get_child(&parent_element, index, parent_handle.children_offset);
        parent_element
            .insert_before(child_node, component_after.as_ref())
            .expect("insert success");
    }

    fn swap_children(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        a: usize,
        b: usize,
    ) {
        Self::assert_handler_avalanche_web(parent_type);
        let parent_handle = Self::handle_cast(parent_handle);
        let parent_element = Self::node_to_element(parent_handle.node.clone());
        let lesser = std::cmp::min(a, b);
        let greater = std::cmp::max(a, b);

        // TODO: throw exception if a and b are equal but out of bounds?
        if a != b {
            let a = Self::get_child(&parent_element, lesser, parent_handle.children_offset);
            let b = Self::get_child(&parent_element, greater, parent_handle.children_offset);
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
        Self::assert_handler_avalanche_web(parent_type);
        let parent_handle = Self::handle_cast(parent_handle);
        let parent_element = Self::node_to_element(parent_handle.node.clone());
        let curr_child_node =
            Self::get_child(&parent_element, index, parent_handle.children_offset);
        let replace_child_node = &Self::handle_cast(child_handle).node;
        if &curr_child_node != replace_child_node {
            parent_element
                .replace_child(replace_child_node, &curr_child_node)
                .expect("successful replace");
        }
    }

    fn truncate_children(
        &mut self,
        parent_type: &NativeType,
        parent_handle: &mut NativeHandle,
        len: usize,
    ) {
        Self::assert_handler_avalanche_web(parent_type);
        let parent_handle = Self::handle_cast(parent_handle);
        let parent_element = Self::node_to_element(parent_handle.node.clone());

        // TODO: more efficient implementation
        while let Some(node) =
            Self::try_get_child(&parent_element, len, parent_handle.children_offset)
        {
            parent_element
                .remove_child(&node)
                .expect("successful remove");
        }
    }

    // fn remove_child(
    //     &mut self,
    //     parent_type: &NativeType,
    //     parent_handle: &mut NativeHandle,
    //     index: usize,
    // ) {
    //     Self::assert_handler_avalanche_web(parent_type);
    //     let parent_handle = Self::handle_cast(parent_handle);
    //     let parent_element = Self::node_to_element(parent_handle.node.clone());
    //     let child_node = Self::get_child(&parent_element, index, parent_handle.children_offset);
    //     parent_element
    //         .remove_child(&child_node)
    //         .expect("successful remove");
    // }

    fn log(&self, string: &str) {
        let js_val: wasm_bindgen::JsValue = string.into();
        web_sys::console::log_1(&js_val);
    }
}

fn update_generic_prop(element: &Element, name: &str, prop: Option<&str>) {
    match prop {
        Some(prop) => {
            element.set_attribute(name, prop).unwrap();
        }
        None => {
            element.remove_attribute(name).unwrap();
        }
    }
}

fn add_listener(
    element: &web_sys::Element,
    name: &'static str,
    callback: impl Fn(Event) + 'static,
    listeners: &mut HashMap<&'static str, EventListener>,
) {
    add_named_listener(element, name, name, false, callback, listeners)
}

fn add_named_listener(
    element: &web_sys::Element,
    event: &'static str,
    name: &'static str,
    passive: bool,
    callback: impl Fn(Event) + 'static,
    listeners: &mut HashMap<&'static str, EventListener>,
) {
    let options = EventListenerOptions {
        passive,
        ..Default::default()
    };
    let listener = EventListener::new_with_options(element, event, options, move |event| {
        callback(event.clone())
    });
    listeners.insert(name, listener);
}

fn create_handler(name: &'static str, dispatcher: DispatchNativeEvent) -> impl Fn(Event) + 'static {
    move |event| {
        dispatcher.dispatch(NativeEvent {
            name,
            event: Box::new(WebNativeEvent {
                current_target: event.current_target(),
                event,
            }),
        })
    }
}

/// A crate for storing an event and memoized current_target for dispatch.
pub(crate) struct WebNativeEvent {
    event: Event,
    current_target: Option<EventTarget>,
}

// Mdbook's testing doesn't quite work, so we inject our book test cases into the crate to make sure they compile.
#[cfg(doctest)]
mod book_tests {
    use doc_comment::doc_comment;
    doc_comment!(include_str!("../../docs/src/getting_started.md"));
    doc_comment!(include_str!("../../docs/src/basic_components.md"));
    doc_comment!(include_str!("../../docs/src/state.md"));
    doc_comment!(include_str!("../../docs/src/reactivity.md"));
    doc_comment!(include_str!("../../docs/src/events.md"));
    doc_comment!(include_str!("../../docs/src/props.md"));
}
