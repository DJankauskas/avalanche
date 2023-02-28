use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Display;
use std::marker::PhantomData;
use std::cmp::max;

use wasm_bindgen::JsCast;

use crate::{events::*, WebNativeEvent, WebNativeHandle, update_generic_prop, WebRenderer, add_named_listener, add_listener, create_handler};
use avalanche::{Component, View};
use avalanche::renderer::{Renderer, NativeType, NativeHandle, NativeEvent, DispatchNativeEvent};
use avalanche::tracked::Gen;
use avalanche::hooks::{HookContext, RenderContext};

/// Represents a text node.
#[derive(Clone, PartialEq)]
pub struct TextImpl<'a> {
    pub(crate) text: Cow<'a, str>,
    gen: Gen<'a>,
    location: (u32, u32),
    key: Option<String>,
}
pub struct Text<'a> {
    text: Option<Cow<'a, str>>,
    gen: Gen<'a>,
    key: Option<String>,
}

impl<'a> Default for Text<'a> {
    fn default() -> Self {
        Self {
            text: None,
            gen: Gen::escape_hatch_new(false),
            key: None,
        }
    }
}

impl<'a> Text<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn text<T: Into<Cow<'a, str>>>(mut self, text: T, gen: Gen<'a>) -> Self {
        self.text = Some(text.into());
        self.gen = gen;
        self
    }

    pub fn __last<T: Into<Cow<'a, str>>>(mut self, text: T, gen: Gen<'a>) -> Self {
        self.text = Some(text.into());
        self.gen = gen;
        self
    }

    pub fn key<T: ToString>(mut self, key: T, _gen: Gen<'a>) -> Self {
        self.key = Some(key.to_string());
        self
    }

    pub fn build(self, location: (u32, u32)) -> TextImpl<'a> {
        TextImpl {
            text: self.text.unwrap(),
            gen: self.gen,
            key: self.key,
            location,
        }
    }
}

impl<'a> Component<'a> for TextImpl<'a> {
    fn render(self, _: RenderContext, _: HookContext) -> View {
        unimplemented!()
    }
    fn native_type(&self) -> Option<NativeType> {
        let action = NativeType {
            handler: "avalanche_web_text",
            name: "",
        };

        Some(action)
    }
    
    fn native_create(&self, renderer: &mut dyn Renderer, _dispatch_native_event: DispatchNativeEvent) -> NativeHandle {
        let renderer = renderer.downcast_ref::<WebRenderer>().unwrap();
        let text_node = renderer.document.create_text_node(&self.text);
        Box::new(WebNativeHandle {
            node: web_sys::Node::from(text_node),
            _listeners: HashMap::new(),
            children_offset: 0,
        })
    }

    fn native_update(
        self,
        _renderer: &mut dyn Renderer,
        _native_type: &NativeType,
        native_handle: &mut NativeHandle,
        _curr_gen: Gen,
        _event: Option<NativeEvent>,
    ) -> Vec<View> {
        let web_handle = native_handle.downcast_mut::<WebNativeHandle>().unwrap();
        // TODO: compare with old text?
        web_handle.node.set_text_content(Some(self.text.as_ref()));
        Vec::new()
    }

    fn updated(&self, curr_gen: Gen) -> bool {
        self.gen >= curr_gen
    }

    fn location(&self) -> Option<(u32, u32)> {
        Some(self.location)
    }

    fn key(&self) -> Option<String> {
        self.key.clone()
    }
}

pub(crate) enum Attr<'a> {
    Prop(Option<Cow<'a, str>>),
    Handler(Box<dyn Fn(WebNativeEvent) + 'a>),
}

trait IntoCowStr<'a> {
    fn into_cow_str(self) -> Cow<'a, str>;
}

impl<'a> IntoCowStr<'a> for bool {
    fn into_cow_str(self) -> Cow<'a, str> {
        let str = if self { "true" } else { "false" };
        Cow::Borrowed(str)
    }
}

impl<'a> IntoCowStr<'a> for i16 {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Owned(self.to_string())
    }
}

impl<'a> IntoCowStr<'a> for i32 {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Owned(self.to_string())
    }
}

impl<'a> IntoCowStr<'a> for u32 {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Owned(self.to_string())
    }
}

impl<'a> IntoCowStr<'a> for f64 {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Owned(self.to_string())
    }
}

impl<'a> IntoCowStr<'a> for Cow<'a, str> {
    fn into_cow_str(self) -> Cow<'a, str> {
        self
    }
}

#[doc(hidden)]
pub struct RawElement<'a> {
    /// The `Gen<'a>` represents the generation on which the attr was last updated
    pub(crate) attrs: HashMap<&'static str, (Attr<'a>, Gen<'a>)>,
    pub(crate) max_gen: Gen<'a>, 
    pub(crate) children: Vec<View>,
    pub(crate) children_gen: Gen<'a>,
    pub(crate) value_controlled: bool,
    pub(crate) checked_controlled: bool,
    pub(crate) key: Option<String>,
    pub(crate) location: (u32, u32),
    pub(crate) tag: &'static str,
}

impl<'a> Default for RawElement<'a> {
    fn default() -> Self {
        Self {
            attrs: Default::default(),
            max_gen: Gen::escape_hatch_new(false),
            children: Default::default(),
            children_gen: Gen::escape_hatch_new(false),
            value_controlled: Default::default(),
            checked_controlled: Default::default(),
            key: Default::default(),
            location: Default::default(),
            tag: Default::default(),
        }
    }
}

impl<'a> RawElement<'a> {
    fn set_attr(&mut self, name: &'static str, attr: Attr<'a>, gen: Gen<'a>) {
        self.attrs.insert(name, (attr, gen));
        self.max_gen = max(self.max_gen, gen);
    }

    fn set_children(&mut self, children: Vec<View>, gen: Gen<'a>) {
        self.children = children;
        self.children_gen = gen;
    }
}

impl<'a> Component<'a> for RawElement<'a> {
    fn render(self, _: RenderContext, _: HookContext) -> View {
        unimplemented!()
    }
    
    fn native_create(&self, renderer: &mut dyn Renderer, dispatch_native_event: DispatchNativeEvent) -> NativeHandle {
        let renderer = renderer.downcast_ref::<WebRenderer>().unwrap();
        let element = renderer
            .document
            .create_element(self.tag)
            .expect("WebRenderer: element creation failed from syntax error.");

        let mut listeners = HashMap::new();

        if self.value_controlled {
            add_named_listener(
                &element,
                "input",
                "#v",
                false,
                |e| e.prevent_default(),
                &mut listeners,
            );
        }
        if self.checked_controlled {
            add_named_listener(
                &element,
                "change",
                "#c",
                false,
                |e| e.prevent_default(),
                &mut listeners,
            );
        }

        match self.tag {
            "input" => {
                let input_element = element
                    .clone()
                    .dyn_into::<web_sys::HtmlInputElement>()
                    .expect("HTMLInputElement");

                for (name, (attr, _)) in self.attrs.iter() {
                    match attr {
                        Attr::Prop(prop) => {
                            if let Some(prop) = prop {
                                match *name {
                                    "value" => {
                                        input_element.set_value(prop);
                                    }
                                    "checked" => {
                                        input_element.set_checked(!prop.is_empty());
                                    }
                                    _ => {
                                        input_element.set_attribute(name, prop).unwrap();
                                    }
                                }
                            }
                        }
                        Attr::Handler(_) => {
                            let dispatcher = dispatch_native_event.clone();
                            add_listener(
                                &element,
                                name,
                                create_handler(name, dispatcher),
                                &mut listeners,
                            )
                        }
                    }
                }
            }
            "textarea" => {
                let text_area_element = element
                    .clone()
                    .dyn_into::<web_sys::HtmlTextAreaElement>()
                    .expect("HTMLTextAreaElement");

                for (name, (attr, _)) in self.attrs.iter() {
                    match attr {
                        Attr::Prop(prop) => {
                            if let Some(prop) = prop {
                                match *name {
                                    "value" => text_area_element.set_value(prop),
                                    _ => {
                                        text_area_element.set_attribute(name, prop).unwrap()
                                    }
                                }
                            }
                        }
                        Attr::Handler(_) => {
                            let dispatcher = dispatch_native_event.clone();
                            add_listener(
                                &element,
                                name,
                                create_handler(name, dispatcher),
                                &mut listeners,
                            )
                        }
                    }
                }
            }
            _ => {
                for (name, (attr, _)) in self.attrs.iter() {
                    match attr {
                        Attr::Prop(prop) => {
                            if let Some(prop) = prop {
                                element.set_attribute(name, prop).unwrap();
                            }
                        }
                        Attr::Handler(_) => {
                            let dispatcher = dispatch_native_event.clone();
                            add_listener(
                                &element,
                                name,
                                create_handler(name, dispatcher),
                                &mut listeners,
                            )
                        }
                    }
                }
            }
        }

        Box::new(WebNativeHandle {
            node: web_sys::Node::from(element),
            _listeners: listeners,
            children_offset: 0,
        })
    }

    fn native_update(
        self,
        _renderer: &mut dyn Renderer,
        _native_type: &NativeType,
        native_handle: &mut NativeHandle,
        curr_gen: Gen,
        event: Option<NativeEvent>,
    ) -> Vec<View> {
        let web_handle = native_handle.downcast_mut::<WebNativeHandle>().unwrap();
        let node = web_handle.node.clone();
        let element = node.dyn_into::<web_sys::Element>().unwrap();

        if let Some(native_event) = event {
            match &self.attrs[&native_event.name].0 {
                Attr::Handler(handler) => {
                    handler(
                        *native_event
                            .event
                            .downcast::<WebNativeEvent>()
                            .expect("web_sys::Event for native event"),
                    );
                }
                Attr::Prop(_) => {
                    // TODO: panic due to missing prop?
                }
            }
        }

        if self.max_gen >= curr_gen {
            match self.tag {
                "input" => {
                    let input_element = element
                        .clone()
                        .dyn_into::<web_sys::HtmlInputElement>()
                        .expect("HTMLInputElement");
                    for (name, (attr, gen)) in self.attrs.iter() {
                        if *gen >= curr_gen {
                            if let Attr::Prop(prop) = attr {
                                match *name {
                                    "value" => {
                                        if let Some(prop) = prop {
                                            input_element.set_value(prop);
                                        }
                                    }
                                    "checked" => {
                                        input_element.set_checked(prop.is_some());
                                    }
                                    _ => {
                                        update_generic_prop(&element, name, prop.as_deref())
                                    }
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
                    for (name, (attr, gen)) in self.attrs.iter() {
                        if *gen >= curr_gen {
                            if let Attr::Prop(prop) = attr {
                                if *name == "value" {
                                    if let Some(prop) = prop {
                                        text_area_element.set_value(prop);
                                    }
                                } else {
                                    update_generic_prop(&element, name, prop.as_deref())
                                }
                            }
                        }
                    }
                }
                _ => {
                    for (name, (attr, gen)) in self.attrs.iter() {
                        if *gen >= curr_gen {
                            if let Attr::Prop(prop) = attr {
                                update_generic_prop(&element, name, prop.as_deref())
                            }
                        }
                    }
                }
            }
        }
        self.children
    }

    fn updated(&self, curr_gen: Gen) -> bool {
        max(self.max_gen, self.children_gen) >= curr_gen
    }

    fn native_type(&self) -> Option<NativeType> {
        Some(NativeType {
            handler: "avalanche_web",
            name: self.tag,
        })
    }

    fn location(&self) -> Option<(u32, u32)> {
        Some(self.location)
    }

    fn key(&self) -> Option<String> {
        self.key.clone()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Dir {
    Ltr,
    Rtl,
    Auto,
}

impl Dir {
    fn as_str(self) -> &'static str {
        match self {
            Dir::Ltr => "ltr",
            Dir::Rtl => "rtl",
            Dir::Auto => "auto",
        }
    }
}

impl std::fmt::Display for Dir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> IntoCowStr<'a> for Dir {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Borrowed(self.as_str())
    }
}

#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub enum Translate {
    Yes,
    No,
}

impl Translate {
    fn as_str(self) -> &'static str {
        match self {
            Translate::Yes => "yes",
            Translate::No => "no",
        }
    }
}

impl std::fmt::Display for Translate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> IntoCowStr<'a> for Translate {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Borrowed(self.as_str())
    }
}

// TODO: is there a way to avoid making this public?
/// An internal trait used as a workaround for the fact that inherent types cannot have associated types.
/// `NativeElement` is used for typing on event handlers.
#[doc(hidden)]
pub trait AssociatedNativeElement {
    type NativeElement: JsCast;
}

macro_rules! def_component {
    (
        $native_tag:expr;
        $native_element:path;
        $tag:ident;
        $tag_impl:ident;
    ) => {
        pub struct $tag_impl<'a>(PhantomData<&'a ()>);

        // Dummy implenentation of Component for $tag
        // Used only for Impl; all tags create RawElements
        impl<'a> ::avalanche::Component<'a> for $tag<'a> {
            fn render(self, _: RenderContext, _: HookContext) -> View {
                unreachable!()
            }

            fn updated(&self, _: Gen) -> bool {
                unreachable!()
            }
            
            fn location(&self) -> Option<(u32, u32)> {
                unreachable!()
            }
        }

        pub struct $tag<'a> {
            raw: RawElement<'a>,
        }

        impl<'a> $tag<'a> {
            pub fn new() -> Self {
                Default::default()
            }

            pub fn build(mut self, location: (u32, u32)) -> RawElement<'a> {
                self.raw.location = location;
                self.raw.tag = $native_tag;
                self.raw
            }

            pub fn key<S: ToString>(mut self, key: S, _gen: Gen<'a>) -> Self {
                self.raw.key = Some(key.to_string());
                self
            }

            pub fn child(mut self, child: View, gen: Gen<'a>) -> Self {
                self.raw.set_children(vec![child], gen);
                self
            }

            pub fn children<T: Into<Vec<View>>>(mut self, children: T, gen: Gen<'a>) -> Self {
                self.raw.set_children(children.into(), gen);
                self
            }

            pub fn __last<T: Into<Vec<View>>>(mut self, children: T, gen: Gen<'a>) -> Self {
                self.raw.set_children(children.into(), gen);
                self
            }
        }

        impl<'a> Default for $tag<'a> {
            fn default() -> Self {
                Self {
                    raw: std::default::Default::default(),
                }
            }
        }

        impl<'a> AssociatedNativeElement for $tag<'a> {
            type NativeElement = $native_element;
        }

        add_global_attrs! {$tag}
    };
}

macro_rules! def_component_attrs {
    (
        $mac:ident;
        $l:lifetime;
        props: $($propnative:expr => $propident:ident : $proptype:ty),*;
        $(bool_props: $($boolpropnative: expr => $boolpropident:ident),*;)?
        $(listeners: $($listennative:expr => $listenident:ident : $listentype:ty),*;)?
    ) => {
        macro_rules! $mac {
            ($tag:ident) => {
                impl<$l> $tag<$l> {
                    $(
                        pub fn $propident<T>(mut self, val: T, gen: Gen<'a>) -> Self where T : Into<$proptype> {
                            self.raw.set_attr(
                                $propnative,
                                Attr::Prop(Some(Into::<$proptype>::into(val).into_cow_str())),
                                gen
                            );
                            self
                        }
                    )*

                    $(
                        $(
                            pub fn $boolpropident(mut self, val: bool, gen: Gen<'a>) -> Self {
                                self.raw.set_attr(
                                    $boolpropnative,
                                    Attr::Prop(val.then(|| Cow::Borrowed($boolpropnative))),
                                    gen
                                );
                                self
                            }
                        )*
                    )?

                    $(
                        $(
                            pub fn $listenident(mut self, f: impl Fn(TypedEvent::<$listentype, <$tag as AssociatedNativeElement>::NativeElement>) + 'a, gen: Gen<'a>) -> Self {
                                self.raw.set_attr(
                                    $listennative,
                                    Attr::Handler(Box::new(move |e: WebNativeEvent| f(
                                        TypedEvent::<$listentype, <$tag as AssociatedNativeElement>::NativeElement>::new(e.event.dyn_into::<$listentype>().unwrap(), e.current_target)
                                    ))),
                                    gen
                                );
                                self
                            }
                        )*
                    )?
                }
            }
        }
    }
}

def_component_attrs! {
    add_global_attrs;
    'a;
    props:
        "accesskey" => access_key: Cow<'a, str>,
        "class" => class:  Cow<'a, str>,
        // TODO: this is enumerable
        // for forwards-compatability, make enum?
        "contenteditable" => content_editable: bool,
        "dir" => dir: Dir,
        "draggable" => draggable: bool,
        "id" => id: Cow<'a, str>,
        "lang" => lang: Cow<'a, str>,
        "placeholder" => placeholder: Cow<'a, str>,
        "slot" => slot: Cow<'a, str>,
        "spellcheck" => spell_check: bool,
        "style" => style: Cow<'a, str>,
        "tabindex" => tab_index: i16,
        "title" => title: Cow<'a, str>,
        "translate" => translate: Translate;
    bool_props:
        "hidden" => hidden;
    listeners:
        // Focus events
        "blur" => on_blur: FocusEvent,
        "focus" => on_focus: FocusEvent,
        //focusin, focusout?

        // Clipboard events
        // TODO: these are unstable web_sys apis
        // cut, copy, and paste

        // Composition events
        "compositionstart" => on_composition_start: CompositionEvent,
        "compositionupdate" => on_composition_update: CompositionEvent,
        "compositionend" => on_composition_end: CompositionEvent,

        // Form events
        "change" => on_change: Event,
        "input" => on_input: Event,
        // TODO: for form only?
        "reset" => on_reset: Event,
        "submit" => on_submit: Event,
        "invalid" => on_invalid: Event,

        // Image events
        "load" => on_load: Event,
        "error" => on_error: Event,

        // Keyboard events
        "keydown" => on_key_down: KeyboardEvent,
        "keyup" => on_key_up: KeyboardEvent,

        // Media events
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

        // Mouse events
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

        // Wheel event
        "wheel" => on_wheel: WheelEvent,

        // Drag and drop events
        "drag" => on_drag: DragEvent,
        "dragend" => on_drag_end: DragEvent,
        "dragenter" => on_drag_enter: DragEvent,
        "dragstart" => on_drag_start: DragEvent,
        "dragleave" => on_drag_leave: DragEvent,
        "dragover" => on_drag_over: DragEvent,
        "drop" => on_drop: DragEvent,

        // Touch events
        "touchcancel" => on_touch_cancel: TouchEvent,
        "touchend" => on_touch_end: TouchEvent,
        "touchmove" => on_touch_move: TouchEvent,
        "touchstart" => on_touch_start: TouchEvent,

        // Pointer events
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

        // Scroll event
        "scroll" => on_scroll: Event,

        // Animation events
        "animationstart" => on_animation_start: AnimationEvent,
        "animationcancel" => on_animation_cancel: AnimationEvent,
        "animationend" => on_animation_end: AnimationEvent,
        "animationinteraction" => on_animation_interaction: AnimationEvent,

        // Transition events
        "transitionstart" => on_transition_start: TransitionEvent,
        "transitioncancel" => on_transition_cancel: TransitionEvent,
        "transitionend" => on_transition_end: TransitionEvent,
        "transitionrun" => on_transition_run: TransitionEvent,

        // Progress events
        "abort" => on_abort: Event,
        "loadstart" => on_load_start: ProgressEvent,
        "progress" => on_progress: ProgressEvent;
}

def_component_attrs! {
    add_cite_attr;
    'a;
    props:
        "cite" => cite: Cow<'a, str>;
}

def_component_attrs! {
    add_datetime_attr;
    'a;
    props:
        "datetime" => date_time: Cow<'a, str>;
}

def_component_attrs! {
    add_string_value_attr;
    'a;
    props:
        "value" => value: Cow<'a, str>;
}

def_component_attrs! {
    add_name_attr;
    'a;
    props:
        "name" => name: Cow<'a, str>;
}

def_component! {
    "div";
    web_sys::HtmlDivElement;
    Div;
    DivImpl;
}

def_component! {
    "h1";
    web_sys::HtmlHeadingElement;
    H1;
    H1Impl;
}

def_component! {
    "h2";
    web_sys::HtmlHeadingElement;
    H2;
    H2Impl;
}

def_component! {
    "h3";
    web_sys::HtmlHeadingElement;
    H3;
    H3Impl;
}

def_component! {
    "h4";
    web_sys::HtmlHeadingElement;
    H4;
    H4Impl;
}

def_component! {
    "h5";
    web_sys::HtmlHeadingElement;
    H5;
    H5Impl;
}

def_component! {
    "h6";
    web_sys::HtmlHeadingElement;
    H6;
    H6Impl;
}

// TODO: should meta-type tags be implemented?

def_component! {
    "body";
    web_sys::HtmlBodyElement;
    Body;
    BodyImpl;
}

def_component! {
    "address";
    web_sys::HtmlSpanElement;
    Address;
    AddressImpl;
}

def_component! {
    "article";
    web_sys::HtmlElement;
    Article;
    ArticleImpl;
}

def_component! {
    "aside";
    web_sys::HtmlElement;
    Aside;
    AsideImpl;
}

def_component! {
    "footer";
    web_sys::HtmlElement;
    Footer;
    FooterImpl;
}

def_component! {
    "header";
    web_sys::HtmlElement;
    Header;
    HeaderImpl;
}

def_component! {
    "hgroup";
    web_sys::HtmlElement;
    HGroup;
    HGroupImpl;
}

def_component! {
    "main";
    web_sys::HtmlElement;
    Main;
    MainImpl;
}

def_component! {
    "nav";
    web_sys::HtmlElement;
    Nav;
    NavImpl;
}

def_component! {
    "section";
    web_sys::HtmlElement;
    Section;
    SectionImpl;
}

def_component! {
    "blockquote";
    web_sys::HtmlQuoteElement;
    BlockQuote;
    BlockQuoteImpl;
}

add_cite_attr! {BlockQuote}

def_component! {
    "dd";
    web_sys::HtmlElement;
    Dd;
    DdImpl;
}

def_component! {
    "dl";
    web_sys::HtmlElement;
    Dl;
    DlImpl;
}

def_component! {
    "dt";
    web_sys::HtmlElement;
    Dt;
    DtImpl;
}

def_component! {
    "figcaption";
    web_sys::HtmlElement;
    FigCaption;
    FigCaptionImpl;
}

def_component! {
    "figure";
    web_sys::HtmlElement;
    Figure;
    FigureImpl;
}

def_component! {
    "hr";
    web_sys::HtmlHrElement;
    Hr;
    HrImpl;
}

def_component! {
    "li";
    web_sys::HtmlLiElement;
    Li;
    LiImpl;
}

def_component_attrs! {
    add_li_attrs;
    'a;
    props:
        "value" => value: u32;
}
add_li_attrs! {Li}

def_component! {
    "ol";
    web_sys::HtmlOListElement;
    Ol;
    OlImpl;
}

def_component_attrs! {
    add_ol_attrs;
    'a;
    props:
        "start" => start: i32,
        "type" => type_: Cow<'a, str>;
    bool_props:
        "reversed" => reversed;
}
add_ol_attrs! {Ol}

def_component! {
    "p";
    web_sys::HtmlParagraphElement;
    P;
    PImpl;
}

def_component! {
    "pre";
    web_sys::HtmlPreElement;
    Pre;
    PreImpl;
}

def_component! {
    "ul";
    web_sys::HtmlUListElement;
    Ul;
    UlImpl;
}

def_component! {
    "a";
    web_sys::HtmlAnchorElement;
    A;
    AImpl;
}

def_component_attrs! {
    add_a_attrs;
    'a;
    props:
        "download" => download: Cow<'a, str>,
        "href" => href: Cow<'a, str>,
        "hreflanf" => href_lang: Cow<'a, str>,
        "ping" => ping: Cow<'a, str>,
        "referrerpolicy" => referrer_policy: Cow<'a, str>,
        "rel" => rel: Cow<'a, str>,
        "target" => target: Cow<'a, str>,
        "type" => type_: Cow<'a, str>;
}
add_a_attrs! {A}

def_component! {
    "abbr";
    web_sys::HtmlElement;
    Abbr;
    AbbrImpl;
}

def_component! {
    "b";
    web_sys::HtmlElement;
    B;
    BImpl;
}

def_component! {
    "bdi";
    web_sys::HtmlElement;
    Bdi;
    BdiImpl;
}

def_component! {
    "bdo";
    web_sys::HtmlElement;
    Bdo;
    BdoImpl;
}

def_component! {
    "br";
    web_sys::HtmlBrElement;
    Br;
    BrImpl;
}

def_component! {
    "cite";
    web_sys::HtmlSpanElement;
    Cite;
    CiteImpl;
}

def_component! {
    "code";
    web_sys::HtmlSpanElement;
    Code;
    CodeImpl;
}

def_component! {
    "data";
    web_sys::HtmlDataElement;
    Data;
    DataImpl;
}
add_string_value_attr! {Data}

def_component! {
    "dfn";
    web_sys::HtmlElement;
    Dfn;
    DfnImpl;
}

def_component! {
    "em";
    web_sys::HtmlSpanElement;
    Em;
    EmImpl;
}

def_component! {
    "i";
    web_sys::HtmlElement;
    I;
    IImpl;
}

def_component! {
    "kbd";
    web_sys::HtmlElement;
    Kbd;
    KbdImpl;
}

def_component! {
    "mark";
    web_sys::HtmlElement;
    Mark;
    MarkImpl;
}

def_component! {
    "q";
    web_sys::HtmlQuoteElement;
    Q;
    QImpl;
}
add_cite_attr! {Q}

def_component! {
    "rp";
    web_sys::HtmlElement;
    Rp;
    RpImpl;
}

def_component! {
    "rt";
    web_sys::HtmlElement;
    Rt;
    RtImpl;
}

def_component! {
    "rtc";
    web_sys::HtmlElement;
    Rtc;
    RtcImpl;
}

def_component! {
    "ruby";
    web_sys::HtmlElement;
    Ruby;
    RubyImpl;
}

def_component! {
    "s";
    web_sys::HtmlElement;
    S;
    SImpl;
}

def_component! {
    "samp";
    web_sys::HtmlElement;
    Samp;
    SampImpl;
}

def_component! {
    "small";
    web_sys::HtmlElement;
    Small;
    SmallImpl;
}

def_component! {
    "span";
    web_sys::HtmlSpanElement;
    Span;
    SpanImpl;
}

def_component! {
    "strong";
    web_sys::HtmlElement;
    Strong;
    StrongImpl;
}

def_component! {
    "sub";
    web_sys::HtmlElement;
    Sub;
    SubImpl;
}

def_component! {
    "sup";
    web_sys::HtmlElement;
    Sup;
    SupImpl;
}

def_component! {
    "time";
    web_sys::HtmlTimeElement;
    Time;
    TimeImpl;
}

add_datetime_attr! {Time}

def_component! {
    "u";
    web_sys::HtmlElement;
    U;
    UImpl;
}

def_component! {
    "var";
    web_sys::HtmlElement;
    Var;
    VarImpl;
}

def_component! {
    "wbr";
    web_sys::HtmlElement;
    Wbr;
    WbrImpl;
}

def_component! {
    "area";
    web_sys::HtmlAreaElement;
    Area;
    AreaImpl;
}
add_a_attrs! {Area}

def_component_attrs! {
    add_area_attrs;
    'a;
    props:
        "coords" => coords: Cow<'a, str>,
        "shape" => shape: Cow<'a, str>;
}
add_area_attrs! {Area}

#[derive(Copy, Clone, Debug)]
pub enum CrossOrigin {
    Anonymous,
    UseCredentials,
}

impl CrossOrigin {
    fn as_str(self) -> &'static str {
        match self {
            CrossOrigin::Anonymous => "anonymous",
            CrossOrigin::UseCredentials => "use-credentials",
        }
    }
}

impl Display for CrossOrigin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> IntoCowStr<'a> for CrossOrigin {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Borrowed(self.as_str())
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Preload {
    None,
    Metadata,
    Auto,
}

impl Preload {
    fn as_str(self) -> &'static str {
        match self {
            Preload::None => "none",
            Preload::Metadata => "metadata",
            Preload::Auto => "auto",
        }
    }
}

impl Display for Preload {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> IntoCowStr<'a> for Preload {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Borrowed(self.as_str())
    }
}

def_component_attrs! {
    add_media_attrs;
    'a;
    props:
        "crossorigin" => cross_origin: CrossOrigin,
        "preload" => preload: Preload,
        "src" => src: Cow<'a, str>;
    bool_props:
        "autoplay" => autoplay,
        "controls" => controls,
        "disableRemotePlayback" => disable_remote_playback,
        "loop" => loop_,
        "muted" => muted;
}

def_component! {
    "audio";
    web_sys::HtmlAudioElement;
    Audio;
    AudioImpl;
}
add_media_attrs! {Audio}

def_component_attrs! {
    add_width_height_attrs;
    'a;
    props:
        "width" => width: f64,
        "height" => height: f64;
}

def_component! {
    "video";
    web_sys::HtmlVideoElement;
    Video;
    VideoImpl;
}
add_media_attrs! {Video}
add_width_height_attrs! {Video}

def_component_attrs! {
    add_video_attrs;
    'a;
    props:
        "poster" => poster: Cow<'a, str>;
    bool_props:
        "autoPictureInPicture" => auto_picture_in_picture,
        "disablePictureInPicture" => disable_picture_in_picture,
        "playsinline" => plays_inline;
}
add_video_attrs! {Video}

def_component! {
    "img";
    web_sys::HtmlImageElement;
    Img;
    ImgImpl;
}
add_width_height_attrs! {Img}

#[derive(Copy, Clone, Debug)]
pub enum Decoding {
    Sync,
    Async,
    Auto,
}

impl Decoding {
    fn as_str(self) -> &'static str {
        match self {
            Decoding::Sync => "sync",
            Decoding::Async => "async",
            Decoding::Auto => "auto",
        }
    }
}

impl Display for Decoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> IntoCowStr<'a> for Decoding {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Borrowed(self.as_str())
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Loading {
    Eager,
    Lazy,
}

impl Loading {
    fn as_str(self) -> &'static str {
        match self {
            Loading::Eager => "eager",
            Loading::Lazy => "lazy",
        }
    }
}

impl Display for Loading {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> IntoCowStr<'a> for Loading {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Borrowed(self.as_str())
    }
}

def_component_attrs! {
    add_img_attrs;
    'a;
    props:
        "alt" => alt: Cow<'a, str>,
        "crossorigin" => cross_origin: CrossOrigin,
        "decoding" => decoding: Decoding,
        "loading" => loading: Loading,
        "referrerpolicy" => referrer_policy: Cow<'a, str>,
        "sizes" => sizes: Cow<'a, str>,
        "src" => src: Cow<'a, str>,
        "srcset" => src_set: Cow<'a, str>,
        "usemap" => use_map: Cow<'a, str>;
    bool_props:
        "ismap" => is_map;
}
add_img_attrs! {Img}

def_component! {
    "map";
    web_sys::HtmlMapElement;
    Map;
    MapImpl;
}
add_name_attr! {Map}

def_component! {
    "track";
    web_sys::HtmlTrackElement;
    Track;
    TrackImpl;
}

#[derive(Copy, Clone, Debug)]
pub enum TrackKind {
    Subtitles,
    Captions,
    Descriptions,
    Chapters,
    Metadata,
}

impl TrackKind {
    fn as_str(self) -> &'static str {
        match self {
            TrackKind::Subtitles => "subtitles",
            TrackKind::Captions => "captions",
            TrackKind::Descriptions => "descriptions",
            TrackKind::Chapters => "chapters",
            TrackKind::Metadata => "metadata",
        }
    }
}

impl Display for TrackKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> IntoCowStr<'a> for TrackKind {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Borrowed(self.as_str())
    }
}

def_component_attrs! {
    add_track_attrs;
    'a;
    props:
        "kind" => kind: TrackKind,
        "label" => label: Cow<'a, str>,
        "src" => src: Cow<'a, str>,
        "srclang" => src_lang: Cow<'a, str>;
    bool_props:
        "default" => default;
}
add_track_attrs! {Track}

def_component_attrs! {
    add_base_img_attrs;
    'a;
    props:
        "alt" => alt: Cow<'a, str>,
        "height" => height: f64,
        "src" => src: Cow<'a, str>,
        "width" => width: f64;
}

def_component! {
    "embed";
    web_sys::HtmlEmbedElement;
    Embed;
    EmbedImpl;
}
add_width_height_attrs! {Embed}

def_component_attrs! {
    add_embed_attrs;
    'a;
    props:
        "src" => src: Cow<'a, str>,
        "type" => type_: Cow<'a, str>;
}
add_embed_attrs! {Embed}

def_component! {
    "iframe";
    web_sys::HtmlIFrameElement;
    IFrame;
    IFrameImpl;
}
add_width_height_attrs! {IFrame}

def_component_attrs! {
    add_iframe_attrs;
    'a;
    props:
        "allow" => allow: Cow<'a, str>,
        "csp" => csp: Cow<'a, str>,
        "loading" => loading: Loading,
        "name" => name: Cow<'a, str>,
        "referrerpolicy" => referrer_policy: Cow<'a, str>,
        "sandbox" => sandbox: Cow<'a, str>,
        "src" => src: Cow<'a, str>,
        "srcdoc" => src_doc: Cow<'a, str>;
    bool_props:
        "allowfullscreen" => allow_full_screen,
        "allowpaymentrequest" => allow_payment_request;
}
add_iframe_attrs! {IFrame}

def_component! {
    "object";
    web_sys::HtmlObjectElement;
    Object;
    ObjectImpl;
}
add_width_height_attrs! {Object}
add_name_attr! {Object}

def_component_attrs! {
    add_object_attrs;
    'a;
    props:
        "data" => data: Cow<'a, str>,
        "form" => form: Cow<'a, str>,
        "type" => type_: Cow<'a, str>,
        "usemap" => use_map: Cow<'a, str>;
    bool_props:
        "typemustmatch" => type_must_match;
}
add_object_attrs! {Object}

def_component! {
    "param";
    web_sys::HtmlParamElement;
    Param;
    ParamImpl;
}
add_string_value_attr! {Param}
add_name_attr! {Param}

def_component! {
    "picture";
    web_sys::HtmlPictureElement;
    Picture;
    PictureImpl;
}

def_component! {
    "ins";
    web_sys::HtmlModElement;
    Ins;
    InsImpl;
}
add_cite_attr! {Ins}
add_datetime_attr! {Ins}

def_component! {
    "del";
    web_sys::HtmlModElement;
    Del;
    DelImpl;
}
add_cite_attr! {Del}
add_datetime_attr! {Del}

def_component! {
   "caption";
   web_sys::HtmlTableCaptionElement;
   Caption;
   CaptionImpl;
}

def_component! {
    "col";
    web_sys::HtmlTableColElement;
    Col;
    ColImpl;
}

def_component_attrs! {
    add_col_attrs;
    'a;
    props:
        "span" => span: u32;
}
add_col_attrs! {Col}

// same as above
def_component! {
    "colgroup";
    web_sys::HtmlTableColElement;
    ColGroup;
    ColGroupImpl;
}

def_component! {
    "table";
    web_sys::HtmlTableElement;
    Table;
    TableImpl;
}

def_component! {
    "tbody";
    web_sys::HtmlTableSectionElement;
    TBody;
    TBodyImpl;
}

def_component! {
    "td";
    web_sys::HtmlTableCellElement;
    Td;
    TdImpl;
}

def_component_attrs! {
    add_td_th_attrs;
    'a;
    props:
        "colspan" => col_span: u32,
        "rowspan" => row_span: u32,
        "headers" => headers: Cow<'a, str>;
}
add_td_th_attrs! {Td}

def_component! {
    "tfoot";
    web_sys::HtmlTableSectionElement;
    TFoot;
    TFootImpl;
}

// TODO: attrs
def_component! {
    "th";
    web_sys::HtmlTableCellElement;
    Th;
    ThImpl;
}
add_td_th_attrs! {Th}

#[derive(Copy, Clone, Debug)]
pub enum Scope {
    Row,
    Col,
    RowGroup,
    ColGroup,
    Auto,
}

impl Scope {
    fn as_str(self) -> &'static str {
        match self {
            Scope::Row => "row",
            Scope::Col => "col",
            Scope::RowGroup => "rowgroup",
            Scope::ColGroup => "colgroup",
            Scope::Auto => "auto",
        }
    }
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> IntoCowStr<'a> for Scope {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Borrowed(self.as_str())
    }
}

def_component_attrs! {
    add_th_attrs;
    'a;
    props:
        "abbr" => abbr: Cow<'a, str>,
        "scope" => scope: Scope;
}
add_th_attrs! {Th}

def_component! {
    "thead";
    web_sys::HtmlTableSectionElement;
    THead;
    THeadImpl;
}

def_component! {
    "tr";
    web_sys::HtmlTableRowElement;
    Tr;
    TrImpl;
}

def_component_attrs! {
    add_form_field_attrs;
    'a;
    props:
        "form" => form: Cow<'a, str>,
        "name" => name: Cow<'a, str>;
    bool_props:
        "autofocus" => auto_focus,
        "disabled" => disabled;
}

def_component_attrs! {
    add_form_submit_attrs;
    'a;
    props:
        "formaction" => form_ation: Cow<'a, str>,
        "formenctype" => form_enc_type: Cow<'a, str>,
        "formmethod" => form_method: Cow<'a, str>,
        "formtarget" => form_target: Cow<'a, str>;
    bool_props:
        "formnovalidate" => form_no_validate;
}

def_component_attrs! {
    add_type_attr;
    'a;
    props:
        "type" => type_: Cow<'a, str>;
}

def_component! {
    "button";
    web_sys::HtmlButtonElement;
    Button;
    ButtonImpl;
}
add_type_attr! {Button}
add_form_field_attrs! {Button}
add_form_submit_attrs! {Button}
add_string_value_attr! {Button}

def_component! {
    "datalist";
    web_sys::HtmlDataListElement;
    DataList;
    DataListImpl;
}

def_component! {
    "fieldset";
    web_sys::HtmlFieldSetElement;
    FieldSet;
    FieldSetImpl;
}

def_component_attrs! {
    add_field_set_attrs;
    'a;
    props:
        "form" => form: Cow<'a, str>;
    bool_props:
        "disabled" => disabled;
}
add_field_set_attrs! {FieldSet}
add_name_attr! {FieldSet}

def_component! {
    "form";
    web_sys::HtmlFormElement;
    Form;
    FormImpl;
}
add_name_attr! {Form}

def_component_attrs! {
    add_form_attrs;
    'a;
    props:
        "accept-charset" => accept_charset: Cow<'a, str>,
        "autocomplete" => auto_complete: Cow<'a, str>,
        "rel" => rel: Cow<'a, str>,
        "action" => action: Cow<'a, str>,
        "enctype" => enc_type: Cow<'a, str>,
        "method" => method: Cow<'a, str>,
        "target" => target: Cow<'a, str>;
    bool_props:
        "novalidate" => no_validate;
}
add_form_attrs! {Form}

def_component_attrs! {
    add_textinput_attrs;
    'a;
    props:
        "autocomplete" => auto_complete: Cow<'a, str>,
        "maxlength" => max_length: u32,
        "minlength" => min_length: u32;
    bool_props:
        "readonly" => read_only,
        "required" => required;
}

def_component! {
    "input";
    web_sys::HtmlInputElement;
    Input;
    InputImpl;
}
add_form_field_attrs! {Input}
add_textinput_attrs! {Input}
add_form_submit_attrs! {Input}
add_base_img_attrs! {Input}
add_type_attr! {Input}

def_component_attrs! {
    add_input_attrs;
    'a;
    props:
        "capture" => capture: Cow<'a, str>,
        "dirname" => dir_name: Cow<'a, str>,
        "inputmode" => input_mode: Cow<'a, str>,
        "list" => list: Cow<'a, str>,
        "min" => min: Cow<'a, str>,
        "max" => max: Cow<'a, str>;
    bool_props:
        "multiple" => multiple;
}
add_input_attrs! {Input}

impl<'a> Input<'a> {
    pub fn value<S: Into<Cow<'a, str>>>(mut self, val: S, gen: Gen<'a>) -> Self {
        self.raw.value_controlled = true;
        self.raw
            .set_attr("value", Attr::Prop(Some(val.into())), gen);
        self
    }

    pub fn checked(mut self, val: bool, gen: Gen<'a>) -> Self {
        self.raw.checked_controlled = true;
        self.raw.set_attr(
            "checked",
            Attr::Prop(val.then(|| Cow::Borrowed("checked"))),
            gen,
        );
        self
    }
}

def_component! {
    "textarea";
    web_sys::HtmlTextAreaElement;
    TextArea;
    TextAreaImpl;
}
add_form_field_attrs! {TextArea}
add_textinput_attrs! {TextArea}

impl<'a> TextArea<'a> {
    pub fn value<S: Into<Cow<'a, str>>>(mut self, val: S, gen: Gen<'a>) -> Self {
        self.raw.value_controlled = true;
        self.raw
            .set_attr("value", Attr::Prop(Some(val.into())), gen);
        self
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Wrap {
    Soft,
    Hard,
    Off,
}

impl Wrap {
    fn as_str(self) -> &'static str {
        match self {
            Wrap::Soft => "soft",
            Wrap::Hard => "hard",
            Wrap::Off => "off",
        }
    }
}

impl Display for Wrap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> IntoCowStr<'a> for Wrap {
    fn into_cow_str(self) -> Cow<'a, str> {
        Cow::Borrowed(self.as_str())
    }
}

def_component_attrs! {
    add_textarea_attrs;
    'a;
    props:
        "cols" => cols: u32,
        "rows" => rows: u32,
        "wrap" => wrap: Wrap;
}
add_textarea_attrs! {TextArea}

def_component_attrs! {
    add_label_attrs;
    'a;
    props:
        "for" => for_: Cow<'a, str>;
}

def_component! {
    "label";
    web_sys::HtmlLabelElement;
    Label;
    LabelImpl;
}
add_label_attrs! {Label}

def_component! {
    "legend";
    web_sys::HtmlLegendElement;
    Legend;
    LegendImpl;
}

def_component_attrs! {
    add_meter_attrs;
    'a;
    props:
        "min" => min: f64,
        "max" => max: f64,
        "low" => low: f64,
        "high" => high: f64,
        "optimum" => optimum: f64,
        "form" => form: Cow<'a, str>,
        "value" => value: Cow<'a, str>;
}

def_component! {
    "meter";
    web_sys::HtmlMeterElement;
    Meter;
    MeterImpl;
}
add_meter_attrs! {Meter}

def_component! {
    "optgroup";
    web_sys::HtmlOptGroupElement;
    OptGroup;
    OptGroupImpl;
}

def_component_attrs! {
    add_label_value_attrs;
    'a;
    props:
        "value" => value: Cow<'a, str>,
        "label" => label: Cow<'a, str>;
}
add_label_value_attrs! {OptGroup}

// TODO: doc alias for Option
def_component! {
    "option";
    web_sys::HtmlOptionElement;
    Opt;
    OptImpl;
}
add_label_value_attrs! {Opt}

def_component_attrs! {
    add_opt_attrs;
    'a;
    props:
        ;
    bool_props:
        "disabled" => disabled,
        "selected" => selected;
}
add_opt_attrs! {Opt}

def_component! {
    "output";
    web_sys::HtmlOutputElement;
    Output;
    OutputImpl;
}

def_component_attrs! {
    add_output_attrs;
    'a;
    props:
        "for" => for_: Cow<'a, str>,
        "form" => form: Cow<'a, str>,
        "name" => name: Cow<'a, str>;
}
add_output_attrs! {Output}

def_component! {
    "progress";
    web_sys::HtmlProgressElement;
    Progress;
    ProgressImpl;
}

def_component_attrs! {
    add_progress_attrs;
    'a;
    props:
        "max" => max: f64,
        "value" => value: f64;
}

add_progress_attrs! {Progress}

// TODO: add controlled functionality
def_component! {
    "select";
    web_sys::HtmlSelectElement;
    Select;
    SelectImpl;
}

add_form_field_attrs! {Select}

def_component_attrs! {
    add_select_attrs;
    'a;
    props:
        "autocomplete" => auto_complete: Cow<'a, str>,
        "size" => size: u32;
    bool_props:
        "multiple" => multiple,
        "required" => required;
}
add_select_attrs! {Select}

def_component_attrs! {
    add_open_attr;
    'a;
    props:
        ;
    bool_props:
        "open" => open;
}

def_component! {
    "details";
    web_sys::HtmlDetailsElement;
    Details;
    DetailsImpl;
}
add_open_attr! {Details}

def_component! {
    "dialog";
    web_sys::HtmlDialogElement;
    Dialog;
    DialogImpl;
}
add_open_attr! {Dialog}

def_component! {
    "summary";
    web_sys::HtmlElement;
    Summary;
    SummaryImpl;
}
