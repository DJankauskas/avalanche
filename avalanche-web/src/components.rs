use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use wasm_bindgen::JsCast;

use crate::events::*;
use avalanche::renderer::{HasChildrenMarker, NativeType};
use avalanche::{Component, Context, View};

/// Represents a text node.
#[derive(Clone, PartialEq)]
pub struct Text {
    pub(crate) text: String,
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

    pub fn __last<T: ToString>(mut self, text: T, updated: bool) -> Self {
        self.text = Some(text.to_string());
        self.updated = updated;
        self
    }

    pub fn key<T: ToString>(mut self, key: T, _updated: bool) -> Self {
        self.key = Some(key.to_string());
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

    fn render(&self, _: Context) -> View {
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

pub(crate) enum Attr {
    Prop(Option<Cow<'static, str>>),
    Handler(Rc<dyn Fn(Event)>),
}
#[derive(Default)]
#[doc(hidden)]
pub struct RawElement {
    /// The `bool` represents whether the attr was updated.
    pub(crate) attrs: HashMap<&'static str, (Attr, bool)>,
    pub(crate) attrs_updated: bool,
    pub(crate) children: Vec<View>,
    pub(crate) children_updated: bool,
    pub(crate) value_controlled: bool,
    pub(crate) checked_controlled: bool,
    pub(crate) key: Option<String>,
    pub(crate) location: (u32, u32),
    pub(crate) tag: &'static str,
}

impl RawElement {
    fn attr(&mut self, name: &'static str, attr: Attr, updated: bool) {
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

    fn render(&self, _: Context) -> View {
        HasChildrenMarker {
            children: self.children.clone(),
        }
        .into()
    }

    fn updated(&self) -> bool {
        self.attrs_updated || self.children_updated
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
        pub struct $tag;

        // Dummy implenentation of Component for $tag
        // Used only for Builder; all tags create RawElements
        impl ::avalanche::Component for $tag {
            type Builder = $tag_builder;

            fn render(&self, _: Context) -> View {
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
                Default::default()
            }

            pub fn build(mut self, location: (u32, u32)) -> RawElement {
                self.raw.location = location;
                self.raw.tag = $native_tag;
                self.raw
            }

            pub fn key<S: ToString>(mut self, key: S, _updated: bool) -> Self {
                self.raw.key = Some(key.to_string());
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

            pub fn __last<T: Into<Vec<View>>>(mut self, children: T, updated: bool) -> Self {
                self.raw.children(children.into(), updated);
                self
            }
        }

        impl Default for $tag_builder {
            fn default() -> Self {
                Self {
                    raw: std::default::Default::default(),
                }
            }
        }

        impl AssociatedNativeElement for $tag_builder {
            type NativeElement = $native_element;
        }

        add_global_attrs! {$tag_builder}
    };
}

macro_rules! def_component_attrs {
    (
        $mac:ident;
        props: $($propnative:expr => $propident:ident : $proptype:ty),*;
        $(bool_props: $($boolpropnative: expr => $boolpropident:ident),*;)?
        $(listeners: $($listennative:expr => $listenident:ident : $listentype:ty),*;)?
    ) => {
        macro_rules! $mac {
            ($builder:ty) => {
                impl $builder {
                    $(
                        pub fn $propident<T>(mut self, val: T, updated: bool) -> Self where T : Into<$proptype> {
                            self.raw.attr(
                                $propnative,
                                Attr::Prop(Some(Cow::Owned(Into::<$proptype>::into(val).to_string()))),
                                updated
                            );
                            self
                        }
                    )*

                    $(
                        $(
                            pub fn $boolpropident(mut self, val: bool, updated: bool) -> Self {
                                self.raw.attr(
                                    $boolpropnative,
                                    Attr::Prop(val.then(|| Cow::Borrowed($boolpropnative))),
                                    updated
                                );
                                self
                            }
                        )*
                    )?

                    $(
                        $(
                            pub fn $listenident(mut self, f: impl Fn(TypedEvent::<$listentype, <$builder as AssociatedNativeElement>::NativeElement>) + 'static, updated: bool) -> Self {
                                self.raw.attr(
                                    $listennative,
                                    Attr::Handler(std::rc::Rc::new(move |e: Event| f(
                                        TypedEvent::<$listentype, <$builder as AssociatedNativeElement>::NativeElement>::new(e.dyn_into::<$listentype>().unwrap())
                                    ))),
                                    updated
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
    props:
        "accesskey" => access_key: String,
        "class" => class:  String,
        // TODO: this is enumerable
        // for forwards-compatability, make enum?
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
    props:
        "cite" => cite: String;
}

def_component_attrs! {
    add_datetime_attr;
    props:
        "datetime" => date_time: String;
}

def_component_attrs! {
    add_string_value_attr;
    props:
        "value" => value: String;
}

def_component_attrs! {
    add_name_attr;
    props:
        "name" => name: String;
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
    props:
        "start" => start: i32,
        "type" => type_: String;
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
    props:
        "download" => download: String,
        "href" => href: String,
        "hreflanf" => href_lang: String,
        "ping" => ping: String,
        "referrerpolicy" => referrer_policy: String,
        "rel" => rel: String,
        "target" => target: String,
        "type" => type_: String;
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
    props:
        "coords" => coords: String,
        "shape" => shape: String;
}
add_area_attrs! {AreaBuilder}

pub enum CrossOrigin {
    Anonymous,
    UseCredentials,
}

impl Display for CrossOrigin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CrossOrigin::Anonymous => "anonymous",
                CrossOrigin::UseCredentials => "use-credentials",
            }
        )
    }
}

pub enum Preload {
    None,
    Metadata,
    Auto,
}

impl Display for Preload {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Preload::None => "none",
                Preload::Metadata => "metadata",
                Preload::Auto => "auto",
            }
        )
    }
}

def_component_attrs! {
    add_media_attrs;
    props:
        "crossorigin" => cross_origin: CrossOrigin,
        "preload" => preload: Preload,
        "src" => src: String;
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
    props:
        "poster" => poster: String;
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

pub enum Decoding {
    Sync,
    Async,
    Auto,
}

impl Display for Decoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Decoding::Sync => "sync",
                Decoding::Async => "async",
                Decoding::Auto => "auto",
            }
        )
    }
}

pub enum Loading {
    Eager,
    Lazy,
}

impl Display for Loading {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Loading::Eager => "eager",
                Loading::Lazy => "lazy",
            }
        )
    }
}

def_component_attrs! {
    add_img_attrs;
    props:
        "alt" => alt: String,
        "crossorigin" => cross_origin: CrossOrigin,
        "decoding" => decoding: Decoding,
        "loading" => loading: Loading,
        "referrerpolicy" => referrer_policy: String,
        "sizes" => sizes: String,
        "src" => src: String,
        "srcset" => src_set: String,
        "usemap" => use_map: String;
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

pub enum TrackKind {
    Subtitles,
    Captions,
    Descriptions,
    Chapters,
    Metadata,
}

impl Display for TrackKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TrackKind::Subtitles => "subtitles",
                TrackKind::Captions => "captions",
                TrackKind::Descriptions => "descriptions",
                TrackKind::Chapters => "chapters",
                TrackKind::Metadata => "metadata",
            }
        )
    }
}

def_component_attrs! {
    add_track_attrs;
    props:
        "kind" => kind: TrackKind,
        "label" => label: String,
        "src" => src: String,
        "srclang" => src_lang: String;
    bool_props:
        "default" => default;
}
add_track_attrs! {TrackBuilder}

def_component_attrs! {
    add_base_img_attrs;
    props:
        "alt" => alt: String,
        "height" => height: f64,
        "src" => src: String,
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
    props:
        "src" => src: String,
        "type" => type_: String;
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
    props:
        "allow" => allow: String,
        "csp" => csp: String,
        "loading" => loading: Loading,
        "name" => name: String,
        "referrerpolicy" => referrer_policy: String,
        "sandbox" => sandbox: String,
        "src" => src: String,
        "srcdoc" => src_doc: String;
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
    props:
        "data" => data: String,
        "form" => form: String,
        "type" => type_: String,
        "usemap" => use_map: String;
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
    props:
        "colspan" => col_span: u32,
        "rowspan" => row_span: u32,
        "headers" => headers: String;
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

pub enum Scope {
    Row,
    Col,
    RowGroup,
    ColGroup,
    Auto,
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Scope::Row => "row",
                Scope::Col => "col",
                Scope::RowGroup => "rowgroup",
                Scope::ColGroup => "colgroup",
                Scope::Auto => "auto",
            }
        )
    }
}

def_component_attrs! {
    add_th_attrs;
    props:
        "abbr" => abbr: String,
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
    props:
        "form" => form: String,
        "name" => name: String;
    bool_props:
        "autofocus" => auto_focus,
        "disabled" => disabled;
}

def_component_attrs! {
    add_form_submit_attrs;
    props:
        "formaction" => form_ation: String,
        "formenctype" => form_enc_type: String,
        "formmethod" => form_method: String,
        "formtarget" => form_target: String;
    bool_props:
        "formnovalidate" => form_no_validate;
}

def_component_attrs! {
    add_type_attr;
    props:
        "type" => type_: String;
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
    props:
        "form" => form: String;
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
    props:
        "accept-charset" => accept_charset: String,
        "autocomplete" => auto_complete: String,
        "rel" => rel: String,
        "action" => action: String,
        "enctype" => enc_type: String,
        "method" => method: String,
        "target" => target: String;
    bool_props:
        "novalidate" => no_validate;
}
add_form_attrs! {FormBuilder}

def_component_attrs! {
    add_textinput_attrs;
    props:
        "autocomplete" => auto_complete: String,
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
    props:
        "capture" => capture: String,
        "dirname" => dir_name: String,
        "inputmode" => input_mode: String,
        "list" => list: String,
        "min" => min: String,
        "max" => max: String;
    bool_props:
        "multiple" => multiple;
}
add_input_attrs! {InputBuilder}

impl InputBuilder {
    pub fn value<S: Into<String>>(mut self, val: S, updated: bool) -> Self {
        self.raw.value_controlled = true;
        self.raw
            .attr("value", Attr::Prop(Some(Cow::Owned(val.into()))), updated);
        self
    }

    pub fn checked(mut self, val: bool, updated: bool) -> Self {
        self.raw.checked_controlled = true;
        self.raw.attr(
            "checked",
            Attr::Prop(val.then(|| Cow::Borrowed("checked"))),
            updated,
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

impl TextAreaBuilder {
    pub fn value<S: ToString>(mut self, val: S, updated: bool) -> Self {
        self.raw.value_controlled = true;
        self.raw
            .attr("value", Attr::Prop(Some(Cow::Owned(val.to_string()))), updated);
        self
    }
}

pub enum Wrap {
    Soft,
    Hard,
    Off,
}

impl Display for Wrap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Wrap::Soft => "soft",
                Wrap::Hard => "hard",
                Wrap::Off => "off",
            }
        )
    }
}

def_component_attrs! {
    add_textarea_attrs;
    props:
        "cols" => cols: u32,
        "rows" => rows: u32,
        "wrap" => wrap: Wrap;
}
add_textarea_attrs! {TextAreaBuilder}

def_component_attrs! {
    add_label_attrs;
    props:
        "for" => for_: String;
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
    props:
        "min" => min: f64,
        "max" => max: f64,
        "low" => low: f64,
        "high" => high: f64,
        "optimum" => optimum: f64,
        "form" => form: String,
        "value" => value: String;
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
    props:
        "value" => value: String,
        "label" => label: String;
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
    props:
        "for" => for_: String,
        "form" => form: String,
        "name" => name: String;
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
    props:
        "autocomplete" => auto_complete: String,
        "size" => size: u32;
    bool_props:
        "multiple" => multiple,
        "required" => required;
}
add_select_attrs! {SelectBuilder}

def_component_attrs! {
    add_open_attr;
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
