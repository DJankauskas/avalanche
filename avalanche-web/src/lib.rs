use avalanche::renderer::{HasChildrenMarker, NativeHandle, NativeType, Renderer};
use avalanche::vdom::VNode;
use avalanche::{Component, View};

use avalanche::{shared::Shared, InternalContext};

use std::rc::Rc;
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use crate::events::*;
use gloo::events::{EventListener, EventListenerOptions};
use wasm_bindgen::JsCast;

pub mod events;

enum Attr {
    Prop(String),
    Handler(Rc<dyn Fn(Event)>),
}
#[derive(Default)]
#[doc(hidden)]
pub struct RawElement {
    ///bool represents whether the attr was updated
    attrs: HashMap<&'static str, (Option<Attr>, bool)>,
    attrs_updated: bool,
    children: Vec<View>,
    children_updated: bool,
    is_controlled: bool,
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
        impl ::avalanche::Component for crate::$tag {
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
                        pub fn $propident<T>(mut self, val: Option<T>, updated: bool) -> Self where T : Into<$proptype> {
                            self.raw.attr(
                                $propnative,
                                val.map(|k| Attr::Prop(Into::<$proptype>::into(k).to_string())),
                                updated
                            );
                            self
                        }
                    )*

                    $(
                        $(
                            pub fn $boolpropident(mut self, val: Option<bool>, updated: bool) -> Self {
                                if let Some(val) = val {
                                    if val {
                                        self.raw.attr(
                                            $boolpropnative,
                                            // TODO: use Cow<String> for Prop to avoid alloc
                                            Some(Attr::Prop(String::from($boolpropnative))),
                                            updated
                                        );
                                    }
                                }
                                self
                            }
                        )*
                    )?

                    $(
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
    bool_props:
        "hidden" => hidden;
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
    Div;
    DivBuilder;
}

def_component! {
    "h1";
    H1;
    H1Builder;
}

def_component! {
    "h2";
    H2;
    H2Builder;
}

def_component! {
    "h3";
    H3;
    H3Builder;
}

def_component! {
    "h4";
    H4;
    H4Builder;
}

def_component! {
    "h5";
    H5;
    H5Builder;
}

def_component! {
    "h6";
    H6;
    H6Builder;
}

// TODO: should meta-type tags be implemented?

def_component! {
    "body";
    Body;
    BodyBuilder;
}

def_component! {
    "address";
    Address;
    AddressBuilder;
}

def_component! {
    "article";
    Article;
    ArticleBuilder;
}

def_component! {
    "aside";
    Aside;
    AsideBuilder;
}

def_component! {
    "footer";
    Footer;
    FooterBuilder;
}

def_component! {
    "header";
    Header;
    HeaderBuilder;
}

def_component! {
    "hgroup";
    HGroup;
    HGroupBuilder;
}

def_component! {
    "main";
    Main;
    MainBuilder;
}

def_component! {
    "nav";
    Nav;
    NavBuilder;
}

def_component! {
    "section";
    Section;
    SectionBuilder;
}

def_component! {
    "blockquote";
    BlockQuote;
    BlockQuoteBuilder;
}

add_cite_attr! {BlockQuoteBuilder}

def_component! {
    "dd";
    Dd;
    DdBuilder;
}

def_component! {
    "dl";
    Dl;
    DlBuilder;
}

def_component! {
    "dt";
    Dt;
    DtBuilder;
}

def_component! {
    "figcaption";
    FigCaption;
    FigCaptionBuilder;
}

def_component! {
    "figure";
    Figure;
    FigureBuilder;
}

def_component! {
    "hr";
    Hr;
    HrBuilder;
}

def_component! {
    "li";
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
    P;
    PBuilder;
}

def_component! {
    "pre";
    Pre;
    PreBuilder;
}

def_component! {
    "ul";
    Ul;
    UlBuilder;
}

def_component! {
    "a";
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
    Abbr;
    AbbrBuilder;
}

def_component! {
    "b";
    B;
    BBuilder;
}

def_component! {
    "bdi";
    Bdi;
    BdiBuilder;
}

def_component! {
    "bdo";
    Bdo;
    BdoBuilder;
}

def_component! {
    "br";
    Br;
    BrBuilder;
}

def_component! {
    "cite";
    Cite;
    CiteBuilder;
}

def_component! {
    "code";
    Code;
    CodeBuilder;
}

def_component! {
    "data";
    Data;
    DataBuilder;
}
add_string_value_attr! {DataBuilder}

def_component! {
    "dfn";
    Dfn;
    DfnBuilder;
}

def_component! {
    "em";
    Em;
    EmBuilder;
}

def_component! {
    "i";
    I;
    IBuilder;
}

def_component! {
    "kbd";
    Kbd;
    KbdBuilder;
}

def_component! {
    "mark";
    Mark;
    MarkBuilder;
}

def_component! {
    "q";
    Q;
    QBuilder;
}
add_cite_attr! {QBuilder}

def_component! {
    "rp";
    Rp;
    RpBuilder;
}

def_component! {
    "rt";
    Rt;
    RtBuilder;
}

def_component! {
    "rtc";
    Rtc;
    RtcBuilder;
}

def_component! {
    "ruby";
    Ruby;
    RubyBuilder;
}

def_component! {
    "s";
    S;
    SBuilder;
}

def_component! {
    "samp";
    Samp;
    SampBuilder;
}

def_component! {
    "small";
    Small;
    SmallBuilder;
}

def_component! {
    "span";
    Span;
    SpanBuilder;
}

def_component! {
    "strong";
    Strong;
    StrongBuilder;
}

def_component! {
    "sub";
    Sub;
    SubBuilder;
}

def_component! {
    "sup";
    Sup;
    SupBuilder;
}

def_component! {
    "time";
    Time;
    TimeBuilder;
}

add_datetime_attr! {TimeBuilder}

def_component! {
    "u";
    U;
    UBuilder;
}

def_component! {
    "var";
    Var;
    VarBuilder;
}

def_component! {
    "wbr";
    Wbr;
    WbrBuilder;
}

// TODO: multimedia

def_component_attrs! {
    add_base_img_attrs;
    props:
        "alt" => alt: String,
        "height" => height: u32,
        "src" => src: String,
        "width" => width: u32;
}

def_component! {
    "ins";
    Ins;
    InsBuilder;
}
add_cite_attr! {InsBuilder}
add_datetime_attr! {InsBuilder}

def_component! {
    "del";
    Del;
    DelBuilder;
}
add_cite_attr! {DelBuilder}
add_datetime_attr! {DelBuilder}

def_component! {
   "caption";
   Caption;
   CaptionBuilder;
}

def_component! {
    "col";
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
    ColGroup;
    ColGroupBuilder;
}

def_component! {
    "table";
    Table;
    TableBuilder;
}

def_component! {
    "tbody";
    TBody;
    TBodyBuilder;
}

def_component! {
    "td";
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
    TFoot;
    TFootBuilder;
}

// TODO: attrs
def_component! {
    "th";
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
    THead;
    THeadBuilder;
}

def_component! {
    "tr";
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
    add_button_attrs;
    props:
        "type" => type_: String;
}

def_component! {
    "button";
    Button;
    ButtonBuilder;
}
add_button_attrs! {ButtonBuilder}
add_form_field_attrs! {ButtonBuilder}
add_form_submit_attrs! {ButtonBuilder}
add_string_value_attr! {ButtonBuilder}

def_component! {
    "datalist";
    DataList;
    DataListBuilder;
}

def_component! {
    "fieldset";
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
    Input;
    InputBuilder;
}
add_form_field_attrs! {InputBuilder}
add_textinput_attrs! {InputBuilder}
add_form_submit_attrs! {InputBuilder}
add_base_img_attrs! {InputBuilder}

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
        "checked" => checked,
        "multiple" => multiple;
}
add_input_attrs! {InputBuilder}

impl InputBuilder {
    pub fn value<S: ToString>(mut self, val: S, updated: bool) -> Self {
        self.raw.is_controlled = true;
        self.raw
            .attr("value", Some(Attr::Prop(val.to_string())), updated);
        self
    }
}

def_component! {
    "textarea";
    TextArea;
    TextAreaBuilder;
}
add_form_field_attrs! {TextAreaBuilder}
add_textinput_attrs! {TextAreaBuilder}

impl TextAreaBuilder {
    pub fn value<S: ToString>(mut self, val: S, updated: bool) -> Self {
        self.raw.is_controlled = true;
        self.raw
            .attr("value", Some(Attr::Prop(val.to_string())), updated);
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
    Label;
    LabelBuilder;
}
add_label_attrs! {LabelBuilder}

def_component! {
    "legend";
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
    Meter;
    MeterBuilder;
}
add_meter_attrs! {MeterBuilder}

def_component! {
    "optgroup";
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

static TIMEOUT_MSG_NAME: &str = "oak_web_message_name";

/// Renders the given view in the current document's body.
pub fn mount_to_body(view: View) {
    let renderer = WebRenderer::new();
    let root = avalanche::vdom::Root::new(view, renderer);

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
                                        "value" => input_element.set_value(prop),
                                        _ => input_element.set_attribute(name, &prop).unwrap(),
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
                                                if *name == "value" {
                                                    input_element.set_value(&prop);
                                                } else {
                                                    input_element
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
        self.key = Some(key);
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
