#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

use avalanche::renderer::{HasChildrenMarker, NativeHandle, NativeType, Renderer};
use avalanche::vdom::VNode;
use avalanche::{Component, View};

use avalanche::{shared::Shared, InternalContext};

use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

use crate::events::*;
use gloo::events::EventListener;
use wasm_bindgen::JsCast;

pub mod events;

enum Attr {
    Prop(String),
    Handler(Rc<dyn Fn(Event)>),
}
#[derive(Default)]
#[doc(hidden)]
#[non_exhaustive]
pub struct RawElement {
    ///bool represents whether the attr was updated
    attrs: HashMap<&'static str, (Option<Attr>, bool)>,
    attrs_updated: bool,
    children: Vec<View>,
    children_updated: bool,
    key: Option<String>,
    location: (u32, u32),
    tag: &'static str,
}

impl RawElement {
    fn attr(&mut self, name: &'static str, attr: Option<Attr>, updated: bool) {
        self.attrs.insert(name, (attr, updated));
        self.attrs_updated |= updated;
    }

    fn children(&mut self, children: Vec<View>, updated: bool) {
        self.children = children;
        self.children_updated = updated;
    }
}

impl Component for RawElement {
    type Builder = ();

    fn render(&self, _context: InternalContext) -> View {
        HasChildrenMarker {
            children: self.children.clone(),
        }
        .into()
    }

    fn updated(&self) -> bool {
        self.attrs_updated || self.children_updated
    }

    fn init_state(&self) -> Box<dyn std::any::Any> {
        Box::new(())
    }

    fn native_type(&self) -> Option<NativeType> {
        Some(NativeType {
            handler: "oak_web",
            name: self.tag,
        })
    }

    fn location(&self) -> Option<(u32, u32)> {
        Some(self.location)
    }

    fn key(&self) -> Option<&str> {
        self.key.as_deref()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Dir {
    Ltr,
    Rtl,
    Auto,
}

impl std::fmt::Display for Dir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Dir::Ltr => "ltr",
                Dir::Rtl => "rtl",
                Dir::Auto => "auto",
            }
        )
    }
}

#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub enum Translate {
    Yes,
    No,
}

impl std::fmt::Display for Translate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Translate::Yes => "yes",
                Translate::No => "no",
            }
        )
    }
}

macro_rules! def_component {
    (
        $native_tag:expr;
        $tag:ident;
        $tag_builder:ident;
    ) => {
        pub struct $tag;

        // Dummy implenentation of Component for $tag
        // Used only for Builder; all tags create RawElements
        impl ::avalanche::Component for $tag {
            type Builder = $tag_builder;

            fn render(&self, _: InternalContext) -> View {
                unreachable!()
            }

            fn updated(&self) -> bool {
                unreachable!()
            }
        }

        pub struct $tag_builder {
            raw: RawElement,
        }

        impl $tag_builder {
            pub fn new() -> Self {
                Self {
                    raw: std::default::Default::default(),
                }
            }

            pub fn build(mut self, location: (u32, u32)) -> RawElement {
                self.raw.location = location;
                self.raw.tag = $native_tag;
                self.raw
            }

            pub fn key(mut self, key: String, _updated: bool) -> Self {
                self.raw.key = Some(key);
                self
            }

            pub fn hidden(mut self, val: Option<bool>, updated: bool) -> Self {
                if let Some(hidden) = &val {
                    if *hidden {
                        self.raw
                            .attr("hidden", Some(Attr::Prop(String::new())), updated);
                    }
                };
                self
            }

            pub fn child(mut self, child: View, updated: bool) -> Self {
                self.raw.children(vec![child], updated);
                self
            }

            pub fn children<T: Into<Vec<View>>>(mut self, children: T, updated: bool) -> Self {
                self.raw.children(children.into(), updated);
                self
            }
        }
    };
}

macro_rules! def_component_attrs {
    (
        $mac:ident;
        props: $($propnative:expr => $propident:ident : $proptype:ty),*;
        listeners: $($listennative:expr => $listenident:ident : $listentype:ty),*;
    ) => {
        macro_rules! $mac {
            ($builder:ty) => {
                impl $builder {
                    $(
                        pub fn $propident(mut self, val: Option<$proptype>, updated: bool) -> Self {
                            self.raw.attr(
                                $propnative,
                                val.map(|k| Attr::Prop(k.to_string())),
                                updated
                            );
                            self
                        }
                    )*

                    $(
                        pub fn $listenident(mut self, f: impl Fn($listentype) + 'static, updated: bool) -> Self {
                            self.raw.attr(
                                $listennative,
                                Some(Attr::Handler(std::rc::Rc::new(move |e| f(e.dyn_into::<$listentype>().unwrap())))),
                                updated
                            );
                            self
                        }
                    )*
                }
            }
        }
    }
}

def_component_attrs! {
    add_global_attrs;
    props:
        "accesskey" => access_key: String,
        "class" => class:  String,
        //TODO: this is enumerable
        //for forwards-compatability, make enum?
        "contenteditable" => content_editable: bool,
        "dir" => dir: Dir,
        "draggable" => draggable: bool,
        "id" => id: String,
        "lang" => lang: String,
        "placeholder" => placeholder: String,
        "slot" => slot: String,
        "spellcheck" => spell_check: bool,
        "style" => style: String,
        "tabindex" => tab_index: i16,
        "title" => title: String,
        "translate" => translate: Translate;
    listeners:
        //Focus events
        "blur" => on_blur: FocusEvent,
        "focus" => on_focus: FocusEvent,
        //focusin, focusout?

        //Clipboard events
        //TODO: these are unstable web_sys apis
        //cut, copy, and paste

        //Composition events
        "compositionstart" => on_composition_start: CompositionEvent,
        "compositionupdate" => on_composition_update: CompositionEvent,
        "compositionend" => on_composition_end: CompositionEvent,

        //Form events
        "change" => on_change: Event,
        "input" => on_input: Event,
        //TODO: for form only?
        "reset" => on_reset: Event,
        "submit" => on_submit: Event,
        "invalid" => on_invalid: Event,

        //Image events
        "load" => on_load: Event,
        "error" => on_error: Event,

        //Keyboard events
        "keydown" => on_key_down: KeyboardEvent,
        "keyup" => on_key_up: KeyboardEvent,

        //Media events
        "canplay" => on_can_play: Event,
        "canplaythrough" => on_can_play_through: Event,
        "durationchange" => on_duration_change: Event,
        "emptied" => on_emptied: Event,
        "ended" => on_ended: Event,
        "loadeddata" => on_loaded_data: Event,
        "loadedmetadata" => on_loaded_metadata: Event,
        "pause" => on_pause: Event,
        "play" => on_play: Event,
        "playing" => on_playing: Event,
        "ratechange" => on_rate_change: Event,
        "seeked" => on_seeked: Event,
        "seeking" => on_seeking: Event,
        "stalled" => on_stalled: Event,
        "suspend" => on_suspend: Event,
        "timeupdate" => on_time_update: Event,
        "volumechange" => on_volume_change: Event,
        "waiting" => on_waiting: Event,

        //Mouse events
        "auxclick" => on_aux_click: MouseEvent,
        "click" => on_click: MouseEvent,
        "contextmenu" => on_context_menu: MouseEvent,
        "dblclick" => on_double_click: MouseEvent,
        "mousedown" => on_mouse_down: MouseEvent,
        "mouseenter" => on_mouse_enter: MouseEvent,
        "mouseleave" => on_mouse_leave: MouseEvent,
        "mousemove" => on_mouse_move: MouseEvent,
        "mouseover" => on_mouse_over: MouseEvent,
        "mouseout" => on_mouse_out: MouseEvent,
        "mouseup" => on_mouse_up: MouseEvent,
        "pointerlockchange" => on_pointer_lock_change: Event,
        "pointerlockerror" => on_pointer_lock_error: Event,
        "select" => on_select: Event,

        //Wheel event
        "wheel" => on_wheel: WheelEvent,

        //Drag and drop events
        "drag" => on_drag: DragEvent,
        "dragend" => on_drag_end: DragEvent,
        "dragenter" => on_drag_enter: DragEvent,
        "dragstart" => on_drag_start: DragEvent,
        "dragleave" => on_drag_leave: DragEvent,
        "dragover" => on_drag_over: DragEvent,
        "drop" => on_drop: DragEvent,

        //Touch events
        "touchcancel" => on_touch_cancel: TouchEvent,
        "touchend" => on_touch_end: TouchEvent,
        "touchmove" => on_touch_move: TouchEvent,
        "touchstart" => on_touch_start: TouchEvent,

        //Pointer events
        "pointerover" => on_pointer_over: PointerEvent,
        "pointerenter" => on_pointer_enter: PointerEvent,
        "pointerdown" => on_pointer_down: PointerEvent,
        "pointermove" => on_pointer_move: PointerEvent,
        "pointerup" => on_pointer_up: PointerEvent,
        "pointercancel" => on_pointer_cancel: PointerEvent,
        "pointerout" => on_pointer_out: PointerEvent,
        "pointerleave" => on_pointer_leave: PointerEvent,
        "gotpointercapture" => on_got_pointer_capture: PointerEvent,
        "lostpointercapture" => on_lost_pointer_capture: PointerEvent,

        //Scroll event
        "scroll" => on_scroll: Event,

        //Animation events
        "animationstart" => on_animation_start: AnimationEvent,
        "animationcancel" => on_animation_cancel: AnimationEvent,
        "animationend" => on_animation_end: AnimationEvent,
        "animationinteraction" => on_animation_interaction: AnimationEvent,

        //Transition events
        "transitionstart" => on_transition_start: TransitionEvent,
        "transitioncancel" => on_transition_cancel: TransitionEvent,
        "transitionend" => on_transition_end: TransitionEvent,
        "transitionrun" => on_transition_run: TransitionEvent,

        //Progress events
        "abort" => on_abort: Event,
        "loadstart" => on_load_start: ProgressEvent,
        "progress" => on_progress: ProgressEvent;
}

def_component! {
    "div";
    Div;
    DivBuilder;
}

add_global_attrs! {DivBuilder}

def_component! {
    "button";
    Button;
    ButtonBuilder;
}

add_global_attrs! {ButtonBuilder}

def_component! {
    "h1";
    H1;
    H1Builder;
}

add_global_attrs! {H1Builder}

def_component! {
    "h2";
    H2;
    H2Builder;
}

add_global_attrs! {H2Builder}

def_component! {
    "h3";
    H3;
    H3Builder;
}

add_global_attrs! {H3Builder}

def_component! {
    "h4";
    H4;
    H4Builder;
}

add_global_attrs! {H4Builder}

def_component! {
    "h5";
    H5;
    H5Builder;
}

add_global_attrs! {H5Builder}

def_component! {
    "h6";
    H6;
    H6Builder;
}

add_global_attrs! {H6Builder}

static TIMEOUT_MSG_NAME: &str = "oak_web_message_name";

///Renders the given view in the current document's body.
pub fn mount_to_body(view: View) {
    let renderer = WebRenderer::new();

    let root = avalanche::vdom::generate_root(view, Box::new(renderer));

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

    //TODO: more elegant solution that leaks less memory?
    //TODO: create Shared leak method to avoid extra alloc of Box call?
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
    //TODO: add support for () rendering (important!)
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

                for (name, attr) in raw_element.attrs.iter() {
                    if let Some(attr) = &attr.0 {
                        match attr {
                            Attr::Prop(prop) => {
                                element.set_attribute(name, &prop).unwrap();
                            }
                            Attr::Handler(handler) => {
                                add_listener(&element, name, handler.clone(), &mut listeners);
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

    //TODO: check for custom handler
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
        //post message for 0ms timeouts
        //technique from https://dbaron.org/log/20100309-faster-timeouts
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

        let lesser_child = Self::get_child(&parent_element, lesser);
        let greater_child = Self::get_child(&parent_element, greater);
        parent_element.insert_before(&greater_child, Some(&lesser_child)).expect("insert succeeded");

        let lesser_child = Self::get_child(&parent_element, lesser);
        parent_element.insert_before(&lesser_child, Some(&greater_child)).expect("insert succeeded");
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

fn remove_listener(name: &'static str, listeners: &mut HashMap<&'static str, EventListener>) {
    let _ = listeners.remove(name);
}

///Represents a text node.
#[derive(Clone, PartialEq)]
pub struct Text {
    text: String,
    updated: bool,
    location: (u32, u32),
    key: Option<String>,
}
#[derive(Default)]
pub struct TextBuilder {
    text: Option<String>,
    updated: bool,
    key: Option<String>,
}

impl TextBuilder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn text<T: ToString>(mut self, text: T, updated: bool) -> Self {
        self.text = Some(text.to_string());
        self.updated = updated;
        self
    }

    pub fn key(mut self, key: String, _updated: bool) -> Self {
        self.text = Some(key);
        self
    }

    pub fn build(self, location: (u32, u32)) -> Text {
        Text {
            text: self.text.unwrap(),
            updated: self.updated,
            key: self.key,
            location,
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

    fn updated(&self) -> bool {
        self.updated
    }

    fn location(&self) -> Option<(u32, u32)> {
        Some(self.location)
    }

    fn key(&self) -> Option<&str> {
        self.key.as_deref()
    }
}
