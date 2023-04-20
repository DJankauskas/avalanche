use avalanche::renderer::{
    DispatchNativeEvent, NativeEvent, NativeHandle, Renderer, Scheduler,
};
use avalanche::shared::Shared;
use avalanche::vdom::Root;
use avalanche::DefaultComponent;

use clru::CLruCache;
use rustc_hash::FxHashMap;
use std::collections::VecDeque;
use std::num::NonZeroUsize;

use gloo_events::{EventListener, EventListenerOptions};
use wasm_bindgen::{JsCast, JsValue, intern};
use web_sys::{Element, EventTarget};

pub mod bridge;
pub mod components;
pub mod events;

use crate::events::Event;

static TIMEOUT_MSG_NAME: &str = "avalanche_web_message";

/// Renders the given component onto the `element` parameter.
///
/// To unmount the component, use the returned [Root].
pub fn mount<C: DefaultComponent>(element: Element) -> Root {
    let renderer = WebRenderer::new();
    let scheduler = WebScheduler::new();

    // Clear children of the mount element to ensure children modification
    // indices are consistent with internal state
    bridge::truncate_children(element.unchecked_ref(), 0);

    let native_parent_handle = WebNativeHandle {
        node: element.into(),
        _listeners: Default::default(),
    };

    let root = avalanche::vdom::Root::new::<_, _, C>(
        Box::new(native_parent_handle),
        renderer,
        scheduler,
    );

    root
}

/// Renders the given component in the current document's body.
///
/// To unmount the component, use the returned [Root].
pub fn mount_to_body<C: DefaultComponent>() -> Root {
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
    _listeners: FxHashMap<&'static str, EventListener>,
}

struct WebRenderer {
    string_cache: CLruCache<String, u32>,
}

const STRING_CACHE_CAPACITY: usize = 64;

impl WebRenderer {
    fn new() -> Self {
        // Intern string data for sending window messages
        intern("*");
        intern(TIMEOUT_MSG_NAME);

        WebRenderer {
            string_cache: CLruCache::new(NonZeroUsize::new(STRING_CACHE_CAPACITY).unwrap()),
        }
    }

    fn handle_cast(native_handle: &NativeHandle) -> &WebNativeHandle {
        native_handle
            .downcast_ref::<WebNativeHandle>()
            .expect("WebNativeHandle")
    }

    /// Looks up the given string in the string cache.
    // TODO: don't cache giant strings in lookup LRU
    pub(crate) fn string_idx(&mut self, string: &str) -> u32 {
        if string.is_empty() {
            return 0;
        }
        match self.string_cache.get(string) {
            Some(idx) => *idx,
            None => {
                // If we've reached capacity, evict the least-used entry from 
                // bridge string cache, and insert new string there. Otherwise,
                // place the string at the next valid index
                let idx = if self.string_cache.len() == STRING_CACHE_CAPACITY {
                    let value_to_insert_at = *self.string_cache.back().unwrap().1;
                    value_to_insert_at
                } else {
                    self.string_cache.len() as u32 + 1
                };
                // TODO: reuse allocation?
                let encoded_string: Vec<u16> = string.encode_utf16().collect();
                self.string_cache.put(string.to_owned(), idx);
                bridge::intern_string_at(&encoded_string, idx);
                idx
            }
        }
    }
    
    pub(crate) fn set_attribute(&mut self, element: &JsValue, name: &str, value: &str) {
        let name_idx = self.string_idx(name);
        let value_idx = self.string_idx(value);
        bridge::set_attribute(element, name_idx, value_idx);

    }
}

impl Renderer for WebRenderer {
    fn append_child(
        &mut self,
        parent_handle: &NativeHandle,
        child_handle: &NativeHandle,
    ) {
        let parent_node = &Self::handle_cast(parent_handle).node;
        let child_node = &Self::handle_cast(child_handle).node;
        bridge::append_child(parent_node, child_node);
    }

    fn insert_child(
        &mut self,
        parent_handle: &NativeHandle,
        index: usize,
        child_handle: &NativeHandle,
    ) {
        let parent_node = &Self::handle_cast(parent_handle).node;
        let child_node = &Self::handle_cast(child_handle).node;
        bridge::insert_child(parent_node, index as u32, child_node)
    }

    fn swap_children(
        &mut self,
        parent_handle: &NativeHandle,
        a: usize,
        b: usize,
    ) {
        let parent_node = &Self::handle_cast(parent_handle).node;
        let lesser_idx = std::cmp::min(a, b);
        let greater_idx = std::cmp::max(a, b);

        // TODO: throw exception if a and b are equal but out of bounds?
        if a != b {
            bridge::swap_children(parent_node, lesser_idx as u32, greater_idx as u32);
        }
    }

    fn replace_child(
        &mut self,
        _parent_handle: &NativeHandle,
        _index: usize,
        _child_handle: &NativeHandle,
    ) {
        /*
        let parent_handle = Self::handle_cast(parent_handle);
        let parent_element = Self::node_to_element(parent_handle.node.clone());
        let curr_child_node =
            Self::get_child(&parent_element, index);
        let replace_child_node = &Self::handle_cast(child_handle).node;
        if &curr_child_node != replace_child_node {
            parent_element
                .replace_child(replace_child_node, &curr_child_node)
                .expect("successful replace");
        }
        */
        unimplemented!("currently unused by avalanche")
    }

    fn truncate_children(
        &mut self,
        parent_handle: &NativeHandle,
        len: usize,
    ) {
        let parent_node = &Self::handle_cast(parent_handle).node;
        bridge::truncate_children(parent_node, len as u32);
    }

    // fn remove_child(
    //     &mut self,
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

fn add_listener(
    element: &web_sys::Element,
    name: &'static str,
    callback: impl Fn(Event) + 'static,
    listeners: &mut FxHashMap<&'static str, EventListener>,
) {
    add_named_listener(element, name, name, false, callback, listeners)
}

fn add_named_listener(
    element: &web_sys::Element,
    event: &'static str,
    name: &'static str,
    passive: bool,
    callback: impl Fn(Event) + 'static,
    listeners: &mut FxHashMap<&'static str, EventListener>,
) {
    let options = EventListenerOptions {
        passive,
        ..Default::default()
    };
    
    // reduce overhead of passing event names etc to JS
    intern(event);
    intern(name);

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
