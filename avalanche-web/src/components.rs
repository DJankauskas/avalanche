use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Display;
use std::marker::PhantomData;
use std::cmp::max;

use wasm_bindgen::JsCast;

use crate::{events::*, WebNativeEvent};
use avalanche::{Component, HookContext, RenderContext, View};
use avalanche::renderer::NativeType;
use avalanche::tracked::Gen;

/// Represents a text node.
#[derive(Clone, PartialEq)]
pub struct Text<'a> {
    pub(crate) text: Cow<'a, str>,
    gen: Gen<'a>,
    location: (u32, u32),
    key: Option<String>,
}
pub struct TextBuilder<'a> {
    text: Option<Cow<'a, str>>,
    gen: Gen<'a>,
    key: Option<String>,
}

impl<'a> Default for TextBuilder<'a> {
    fn default() -> Self {
        Self {
            text: None,
            gen: Gen::escape_hatch_new(false),
            key: None,
        }
    }
}

impl<'a> TextBuilder<'a> {
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

    pub fn build(self, location: (u32, u32)) -> Text<'a> {
        Text {
            text: self.text.unwrap(),
            gen: self.gen,
            key: self.key,
            location,
        }
    }
}

avalanche::impl_any_ref!(Text<'a>);

impl<'a> Component<'a> for Text<'a> {
    type Builder = TextBuilder<'a>;

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

    fn children(self) -> Vec<View> {
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

avalanche::impl_any_ref!(RawElement<'a>);

impl<'a> Component<'a> for RawElement<'a> {
    type Builder = ();

    fn render(self, _: RenderContext, _: HookContext) -> View {
        unimplemented!()
    }

    fn children(self) -> Vec<View> {
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
        $tag_builder:ident;
    ) => {
        pub struct $tag<'a>(PhantomData<&'a ()>);

        ::avalanche::impl_any_ref!($tag<'a>);

        // Dummy implenentation of Component for $tag
        // Used only for Builder; all tags create RawElements
        impl<'a> ::avalanche::Component<'a> for $tag<'a> {
            type Builder = $tag_builder<'a>;

            fn render(self, _: RenderContext, _: HookContext) -> View {
                unreachable!()
            }

            fn updated(&self, _: Gen) -> bool {
                unreachable!()
            }
        }

        pub struct $tag_builder<'a> {
            raw: RawElement<'a>,
        }

        impl<'a> $tag_builder<'a> {
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

        impl<'a> Default for $tag_builder<'a> {
            fn default() -> Self {
                Self {
                    raw: std::default::Default::default(),
                }
            }
        }

        impl<'a> AssociatedNativeElement for $tag_builder<'a> {
            type NativeElement = $native_element;
        }

        add_global_attrs! {$tag_builder}
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
            ($builder:ident) => {
                impl<$l> $builder<$l> {
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
                            pub fn $listenident(mut self, f: impl Fn(TypedEvent::<$listentype, <$builder as AssociatedNativeElement>::NativeElement>) + 'a, gen: Gen<'a>) -> Self {
                                self.raw.set_attr(
                                    $listennative,
                                    Attr::Handler(Box::new(move |e: WebNativeEvent| f(
                                        TypedEvent::<$listentype, <$builder as AssociatedNativeElement>::NativeElement>::new(e.event.dyn_into::<$listentype>().unwrap(), e.current_target)
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
    DivBuilder;
}

def_component! {
    "h1";
    web_sys::HtmlHeadingElement;
    H1;
    H1Builder;
}

def_component! {
    "h2";
    web_sys::HtmlHeadingElement;
    H2;
    H2Builder;
}

def_component! {
    "h3";
    web_sys::HtmlHeadingElement;
    H3;
    H3Builder;
}

def_component! {
    "h4";
    web_sys::HtmlHeadingElement;
    H4;
    H4Builder;
}

def_component! {
    "h5";
    web_sys::HtmlHeadingElement;
    H5;
    H5Builder;
}

def_component! {
    "h6";
    web_sys::HtmlHeadingElement;
    H6;
    H6Builder;
}

// TODO: should meta-type tags be implemented?

def_component! {
    "body";
    web_sys::HtmlBodyElement;
    Body;
    BodyBuilder;
}

def_component! {
    "address";
    web_sys::HtmlSpanElement;
    Address;
    AddressBuilder;
}

def_component! {
    "article";
    web_sys::HtmlElement;
    Article;
    ArticleBuilder;
}

def_component! {
    "aside";
    web_sys::HtmlElement;
    Aside;
    AsideBuilder;
}

def_component! {
    "footer";
    web_sys::HtmlElement;
    Footer;
    FooterBuilder;
}

def_component! {
    "header";
    web_sys::HtmlElement;
    Header;
    HeaderBuilder;
}

def_component! {
    "hgroup";
    web_sys::HtmlElement;
    HGroup;
    HGroupBuilder;
}

def_component! {
    "main";
    web_sys::HtmlElement;
    Main;
    MainBuilder;
}

def_component! {
    "nav";
    web_sys::HtmlElement;
    Nav;
    NavBuilder;
}

def_component! {
    "section";
    web_sys::HtmlElement;
    Section;
    SectionBuilder;
}

def_component! {
    "blockquote";
    web_sys::HtmlQuoteElement;
    BlockQuote;
    BlockQuoteBuilder;
}

add_cite_attr! {BlockQuoteBuilder}

def_component! {
    "dd";
    web_sys::HtmlElement;
    Dd;
    DdBuilder;
}

def_component! {
    "dl";
    web_sys::HtmlElement;
    Dl;
    DlBuilder;
}

def_component! {
    "dt";
    web_sys::HtmlElement;
    Dt;
    DtBuilder;
}

def_component! {
    "figcaption";
    web_sys::HtmlElement;
    FigCaption;
    FigCaptionBuilder;
}

def_component! {
    "figure";
    web_sys::HtmlElement;
    Figure;
    FigureBuilder;
}

def_component! {
    "hr";
    web_sys::HtmlHrElement;
    Hr;
    HrBuilder;
}

def_component! {
    "li";
    web_sys::HtmlLiElement;
    Li;
    LiBuilder;
}

def_component_attrs! {
    add_li_attrs;
    'a;
    props:
        "value" => value: u32;
}
add_li_attrs! {LiBuilder}

def_component! {
    "ol";
    web_sys::HtmlOListElement;
    Ol;
    OlBuilder;
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
add_ol_attrs! {OlBuilder}

def_component! {
    "p";
    web_sys::HtmlParagraphElement;
    P;
    PBuilder;
}

def_component! {
    "pre";
    web_sys::HtmlPreElement;
    Pre;
    PreBuilder;
}

def_component! {
    "ul";
    web_sys::HtmlUListElement;
    Ul;
    UlBuilder;
}

def_component! {
    "a";
    web_sys::HtmlAnchorElement;
    A;
    ABuilder;
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
add_a_attrs! {ABuilder}

def_component! {
    "abbr";
    web_sys::HtmlElement;
    Abbr;
    AbbrBuilder;
}

def_component! {
    "b";
    web_sys::HtmlElement;
    B;
    BBuilder;
}

def_component! {
    "bdi";
    web_sys::HtmlElement;
    Bdi;
    BdiBuilder;
}

def_component! {
    "bdo";
    web_sys::HtmlElement;
    Bdo;
    BdoBuilder;
}

def_component! {
    "br";
    web_sys::HtmlBrElement;
    Br;
    BrBuilder;
}

def_component! {
    "cite";
    web_sys::HtmlSpanElement;
    Cite;
    CiteBuilder;
}

def_component! {
    "code";
    web_sys::HtmlSpanElement;
    Code;
    CodeBuilder;
}

def_component! {
    "data";
    web_sys::HtmlDataElement;
    Data;
    DataBuilder;
}
add_string_value_attr! {DataBuilder}

def_component! {
    "dfn";
    web_sys::HtmlElement;
    Dfn;
    DfnBuilder;
}

def_component! {
    "em";
    web_sys::HtmlSpanElement;
    Em;
    EmBuilder;
}

def_component! {
    "i";
    web_sys::HtmlElement;
    I;
    IBuilder;
}

def_component! {
    "kbd";
    web_sys::HtmlElement;
    Kbd;
    KbdBuilder;
}

def_component! {
    "mark";
    web_sys::HtmlElement;
    Mark;
    MarkBuilder;
}

def_component! {
    "q";
    web_sys::HtmlQuoteElement;
    Q;
    QBuilder;
}
add_cite_attr! {QBuilder}

def_component! {
    "rp";
    web_sys::HtmlElement;
    Rp;
    RpBuilder;
}

def_component! {
    "rt";
    web_sys::HtmlElement;
    Rt;
    RtBuilder;
}

def_component! {
    "rtc";
    web_sys::HtmlElement;
    Rtc;
    RtcBuilder;
}

def_component! {
    "ruby";
    web_sys::HtmlElement;
    Ruby;
    RubyBuilder;
}

def_component! {
    "s";
    web_sys::HtmlElement;
    S;
    SBuilder;
}

def_component! {
    "samp";
    web_sys::HtmlElement;
    Samp;
    SampBuilder;
}

def_component! {
    "small";
    web_sys::HtmlElement;
    Small;
    SmallBuilder;
}

def_component! {
    "span";
    web_sys::HtmlSpanElement;
    Span;
    SpanBuilder;
}

def_component! {
    "strong";
    web_sys::HtmlElement;
    Strong;
    StrongBuilder;
}

def_component! {
    "sub";
    web_sys::HtmlElement;
    Sub;
    SubBuilder;
}

def_component! {
    "sup";
    web_sys::HtmlElement;
    Sup;
    SupBuilder;
}

def_component! {
    "time";
    web_sys::HtmlTimeElement;
    Time;
    TimeBuilder;
}

add_datetime_attr! {TimeBuilder}

def_component! {
    "u";
    web_sys::HtmlElement;
    U;
    UBuilder;
}

def_component! {
    "var";
    web_sys::HtmlElement;
    Var;
    VarBuilder;
}

def_component! {
    "wbr";
    web_sys::HtmlElement;
    Wbr;
    WbrBuilder;
}

def_component! {
    "area";
    web_sys::HtmlAreaElement;
    Area;
    AreaBuilder;
}
add_a_attrs! {AreaBuilder}

def_component_attrs! {
    add_area_attrs;
    'a;
    props:
        "coords" => coords: Cow<'a, str>,
        "shape" => shape: Cow<'a, str>;
}
add_area_attrs! {AreaBuilder}

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
    AudioBuilder;
}
add_media_attrs! {AudioBuilder}

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
    VideoBuilder;
}
add_media_attrs! {VideoBuilder}
add_width_height_attrs! {VideoBuilder}

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
add_video_attrs! {VideoBuilder}

def_component! {
    "img";
    web_sys::HtmlImageElement;
    Img;
    ImgBuilder;
}
add_width_height_attrs! {ImgBuilder}

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
add_img_attrs! {ImgBuilder}

def_component! {
    "map";
    web_sys::HtmlMapElement;
    Map;
    MapBuilder;
}
add_name_attr! {MapBuilder}

def_component! {
    "track";
    web_sys::HtmlTrackElement;
    Track;
    TrackBuilder;
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
add_track_attrs! {TrackBuilder}

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
    EmbedBuilder;
}
add_width_height_attrs! {EmbedBuilder}

def_component_attrs! {
    add_embed_attrs;
    'a;
    props:
        "src" => src: Cow<'a, str>,
        "type" => type_: Cow<'a, str>;
}
add_embed_attrs! {EmbedBuilder}

def_component! {
    "iframe";
    web_sys::HtmlIFrameElement;
    IFrame;
    IFrameBuilder;
}
add_width_height_attrs! {IFrameBuilder}

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
add_iframe_attrs! {IFrameBuilder}

def_component! {
    "object";
    web_sys::HtmlObjectElement;
    Object;
    ObjectBuilder;
}
add_width_height_attrs! {ObjectBuilder}
add_name_attr! {ObjectBuilder}

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
add_object_attrs! {ObjectBuilder}

def_component! {
    "param";
    web_sys::HtmlParamElement;
    Param;
    ParamBuilder;
}
add_string_value_attr! {ParamBuilder}
add_name_attr! {ParamBuilder}

def_component! {
    "picture";
    web_sys::HtmlPictureElement;
    Picture;
    PictureBuilder;
}

def_component! {
    "ins";
    web_sys::HtmlModElement;
    Ins;
    InsBuilder;
}
add_cite_attr! {InsBuilder}
add_datetime_attr! {InsBuilder}

def_component! {
    "del";
    web_sys::HtmlModElement;
    Del;
    DelBuilder;
}
add_cite_attr! {DelBuilder}
add_datetime_attr! {DelBuilder}

def_component! {
   "caption";
   web_sys::HtmlTableCaptionElement;
   Caption;
   CaptionBuilder;
}

def_component! {
    "col";
    web_sys::HtmlTableColElement;
    Col;
    ColBuilder;
}

def_component_attrs! {
    add_col_attrs;
    'a;
    props:
        "span" => span: u32;
}
add_col_attrs! {ColBuilder}

// same as above
def_component! {
    "colgroup";
    web_sys::HtmlTableColElement;
    ColGroup;
    ColGroupBuilder;
}

def_component! {
    "table";
    web_sys::HtmlTableElement;
    Table;
    TableBuilder;
}

def_component! {
    "tbody";
    web_sys::HtmlTableSectionElement;
    TBody;
    TBodyBuilder;
}

def_component! {
    "td";
    web_sys::HtmlTableCellElement;
    Td;
    TdBuilder;
}

def_component_attrs! {
    add_td_th_attrs;
    'a;
    props:
        "colspan" => col_span: u32,
        "rowspan" => row_span: u32,
        "headers" => headers: Cow<'a, str>;
}
add_td_th_attrs! {TdBuilder}

def_component! {
    "tfoot";
    web_sys::HtmlTableSectionElement;
    TFoot;
    TFootBuilder;
}

// TODO: attrs
def_component! {
    "th";
    web_sys::HtmlTableCellElement;
    Th;
    ThBuilder;
}
add_td_th_attrs! {ThBuilder}

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
add_th_attrs! {ThBuilder}

def_component! {
    "thead";
    web_sys::HtmlTableSectionElement;
    THead;
    THeadBuilder;
}

def_component! {
    "tr";
    web_sys::HtmlTableRowElement;
    Tr;
    TrBuilder;
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
    ButtonBuilder;
}
add_type_attr! {ButtonBuilder}
add_form_field_attrs! {ButtonBuilder}
add_form_submit_attrs! {ButtonBuilder}
add_string_value_attr! {ButtonBuilder}

def_component! {
    "datalist";
    web_sys::HtmlDataListElement;
    DataList;
    DataListBuilder;
}

def_component! {
    "fieldset";
    web_sys::HtmlFieldSetElement;
    FieldSet;
    FieldSetBuilder;
}

def_component_attrs! {
    add_field_set_attrs;
    'a;
    props:
        "form" => form: Cow<'a, str>;
    bool_props:
        "disabled" => disabled;
}
add_field_set_attrs! {FieldSetBuilder}
add_name_attr! {FieldSetBuilder}

def_component! {
    "form";
    web_sys::HtmlFormElement;
    Form;
    FormBuilder;
}
add_name_attr! {FormBuilder}

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
add_form_attrs! {FormBuilder}

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
    InputBuilder;
}
add_form_field_attrs! {InputBuilder}
add_textinput_attrs! {InputBuilder}
add_form_submit_attrs! {InputBuilder}
add_base_img_attrs! {InputBuilder}
add_type_attr! {InputBuilder}

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
add_input_attrs! {InputBuilder}

impl<'a> InputBuilder<'a> {
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
    TextAreaBuilder;
}
add_form_field_attrs! {TextAreaBuilder}
add_textinput_attrs! {TextAreaBuilder}

impl<'a> TextAreaBuilder<'a> {
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
add_textarea_attrs! {TextAreaBuilder}

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
    LabelBuilder;
}
add_label_attrs! {LabelBuilder}

def_component! {
    "legend";
    web_sys::HtmlLegendElement;
    Legend;
    LegendBuilder;
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
    MeterBuilder;
}
add_meter_attrs! {MeterBuilder}

def_component! {
    "optgroup";
    web_sys::HtmlOptGroupElement;
    OptGroup;
    OptGroupBuilder;
}

def_component_attrs! {
    add_label_value_attrs;
    'a;
    props:
        "value" => value: Cow<'a, str>,
        "label" => label: Cow<'a, str>;
}
add_label_value_attrs! {OptGroupBuilder}

// TODO: doc alias for Option
def_component! {
    "option";
    web_sys::HtmlOptionElement;
    Opt;
    OptBuilder;
}
add_label_value_attrs! {OptBuilder}

def_component_attrs! {
    add_opt_attrs;
    'a;
    props:
        ;
    bool_props:
        "disabled" => disabled,
        "selected" => selected;
}
add_opt_attrs! {OptBuilder}

def_component! {
    "output";
    web_sys::HtmlOutputElement;
    Output;
    OutputBuilder;
}

def_component_attrs! {
    add_output_attrs;
    'a;
    props:
        "for" => for_: Cow<'a, str>,
        "form" => form: Cow<'a, str>,
        "name" => name: Cow<'a, str>;
}
add_output_attrs! {OutputBuilder}

def_component! {
    "progress";
    web_sys::HtmlProgressElement;
    Progress;
    ProgressBuilder;
}

def_component_attrs! {
    add_progress_attrs;
    'a;
    props:
        "max" => max: f64,
        "value" => value: f64;
}

add_progress_attrs! {ProgressBuilder}

// TODO: add controlled functionality
def_component! {
    "select";
    web_sys::HtmlSelectElement;
    Select;
    SelectBuilder;
}

add_form_field_attrs! {SelectBuilder}

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
add_select_attrs! {SelectBuilder}

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
    DetailsBuilder;
}
add_open_attr! {DetailsBuilder}

def_component! {
    "dialog";
    web_sys::HtmlDialogElement;
    Dialog;
    DialogBuilder;
}
add_open_attr! {DialogBuilder}

def_component! {
    "summary";
    web_sys::HtmlElement;
    Summary;
    SummaryBuilder;
}
