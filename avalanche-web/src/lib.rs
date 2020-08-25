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
use std::rc::Rc;

use wasm_bindgen::JsCast;
use web_sys::Event;
use gloo::events::EventListener;

enum Attr {
    Prop(String),
    Handler(Rc<dyn Fn(Event)>)
}
#[derive(Default)]
struct RawElement {
    ///bool represents whether the attr was updated
    attrs: HashMap<&'static str, (Option<Attr>, bool)>,
    attrs_updated: bool,
    children: Vec<View>,
    children_updated: bool
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

    fn get<'a>(tag: &'static str, from: &'a View) -> &'a Self {
        match tag {
            "div" => {
                &from.downcast_ref::<Div>().unwrap().raw
            }
            "button" => {
                &from.downcast_ref::<Button>().unwrap().raw
            }
            _ => panic!(tag)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Dir {
    Ltr,
    Rtl,
    Auto
}

impl std::fmt::Display for Dir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Dir::Ltr => "ltr",
            Dir::Rtl => "rtl",
            Dir::Auto => "auto"
        })
    }
}

#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub enum Translate {
    Yes,
    No
}

impl std::fmt::Display for Translate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Translate::Yes => "yes",
            Translate::No => "no",
        })
    }
}

macro_rules! def_component {
    (
        $native_tag:expr;
        $tag:ident;
        $tag_builder:ident;
        props: $($propnative:expr => $propident:ident : $proptype:ty),+;
        listeners: $($listennative:expr => $listenident:ident : $listentype:ty),+;
    ) => {
        pub struct $tag {
            raw: RawElement,
        }

        impl ::avalanche::Component for $tag {
            type Builder = $tag_builder;

            fn render(&self, _: InternalContext) -> View {
                HasChildrenMarker {
                    //TODO: take children by Rc?
                    children: self.raw.children.clone()
                }.into()
            }

            fn updated(&self) -> bool {
                self.raw.attrs_updated || self.raw.children_updated
            }

            fn native_type(&self) -> Option<NativeType> {
                Some(NativeType {
                    handler: "oak_web",
                    name: $native_tag
                })
            }
        }

        pub struct $tag_builder {
            raw: RawElement
        }

        impl $tag_builder {
            pub fn new() -> Self {
                Self {
                    raw: std::default::Default::default()
                }
            }

            pub fn build(self) -> $tag {
                $tag {
                    raw: self.raw
                }
            }

            pub fn hidden(mut self, val: Option<bool>, updated: bool) -> Self {
                if let Some(hidden) = &val {
                    if *hidden {
                        self.raw.attr(
                            "hidden", 
                            Some(Attr::Prop(String::new())), 
                            updated
                        );
                    }
                };
                self
            }

            pub fn children<T: Into<Vec<View>>>(mut self, children: T, updated: bool) -> Self {
                self.raw.children(children.into(), updated);
                self
            }

            $(
                pub fn $propident(mut self, val: Option<$proptype>, updated: bool) -> Self {
                    self.raw.attr(
                        $propnative, 
                        val.map(|k| Attr::Prop(k.to_string())), 
                        updated
                    );
                    self
                }
            )+

            $(
                pub fn $listenident<F: Fn($listentype) + 'static>(mut self, val: Option<F>, updated: bool) -> Self {
                    self.raw.attr(
                        $listennative,
                        val.map(|f| Attr::Handler(std::rc::Rc::new(move |e| f(e.dyn_into::<$listentype>().unwrap())))), 
                        updated
                    );
                    self
                }
            )+
        }
    };
}

def_component!{
    "div";
    Div;
    DivBuilder;
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
        "click" => on_click: web_sys::MouseEvent;
}

def_component!{
    "button";
    Button;
    ButtonBuilder;
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
        "click" => on_click: web_sys::MouseEvent;
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
                let raw_element = RawElement::get(action.name, &vnode.component);

                let element = self.document.create_element(&action.name).expect(
                    "WebRenderer: element creation failed from syntax error."
                );

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
                let raw_element = RawElement::get(native_type.name, new_comp);
                
                if raw_element.attrs_updated {
                    for (name, (attr, updated)) in raw_element.attrs.iter() {
                        if *updated {
                            match attr {
                                Some(attr) => {
                                    match attr {
                                        Attr::Prop(prop) => {
                                            element.set_attribute(name, &prop).unwrap();
                                        }
                                        Attr::Handler(handler) => {
                                            update_listener(&element, name, handler.clone(), &mut web_handle.listeners);
                                        }
                                    }
                                }
                                None => {
                                    remove_listener(name, &mut web_handle.listeners);
                                }
                            }
                        }
                    }
                }

                if raw_element.children_updated {
                    //TODO: diffing algo
                    element.set_inner_html("");
                    //copy-pasted from create code: factor out or replace with diffing
                    for with_handle in children.iter().filter_map(|child| node_with_native_handle(child.clone())) {
                        with_handle.exec(|with_handle| {
                            let handle = with_handle.native_handle.as_ref().unwrap();
                            element.append_child(&handle.downcast_ref::<WebNativeHandle>().expect("WebNativeHandle").node).unwrap();
                        })
                    }
                }
            },
            "oak_web_text" => {
                let new_text = new_comp.downcast_ref::<Text>().expect("Text component");
                if new_text.updated() {
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
    callback: Rc<dyn Fn(Event)>,
    listeners: &mut HashMap<&'static str, EventListener>
) {
    let listener = EventListener::new(
        &element, name, 
        move |event| callback(event.clone())
    );
    listeners.insert(name, listener);
}

fn update_listener(
    element: &web_sys::Element,
    name: &'static str,
    callback: Rc<dyn Fn(Event)>,
    listeners: &mut HashMap<&'static str, EventListener>
) {
    let _ = listeners.remove(name);
    let listener = EventListener::new(
        &element, name, 
        move |event| callback(event.clone())
    );
    listeners.insert(name, listener);
}

fn remove_listener(
    name: &'static str,
    listeners: &mut HashMap<&'static str, EventListener>
) {
    let _ = listeners.remove(name);
}

///Represents a text node.
#[derive(Clone, PartialEq)]
pub struct Text {
    text: String,
    updated: bool
}
#[derive(Default)]
pub struct TextBuilder {
    text: Option<String>,
    updated: bool
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

    pub fn build(self) -> Text {
        Text {
            text: self.text.unwrap(),
            updated: self.updated
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
}
