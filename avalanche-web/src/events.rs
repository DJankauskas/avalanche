use std::ops::Deref;
use wasm_bindgen::JsCast;

pub(crate) use web_sys::AnimationEvent;
pub(crate) use web_sys::CompositionEvent;
pub(crate) use web_sys::DragEvent;
pub(crate) use web_sys::Event;
use web_sys::EventTarget;
pub(crate) use web_sys::FocusEvent;
pub(crate) use web_sys::KeyboardEvent;
pub(crate) use web_sys::MouseEvent;
pub(crate) use web_sys::PointerEvent;
pub(crate) use web_sys::ProgressEvent;
pub(crate) use web_sys::TouchEvent;
pub(crate) use web_sys::TransitionEvent;
pub(crate) use web_sys::WheelEvent;

/// A typed wrapper over `web_sys`'s event types, allowing for typed access to the native element
/// reference returned by [current_target](TypedEvent::current_target), as well as access to the methods of
/// event `E`.
pub struct TypedEvent<E: JsCast + Clone + Into<Event>, C: JsCast> {
    event: E,
    current_target: Option<C>,
}

impl<E: JsCast + Clone + Into<Event>, C: JsCast + Clone> TypedEvent<E, C> {
    /// Constructs a new [`TypedEvent`] using the given event. It is the caller's
    /// responsibility to ensure the component type `C` is correct in context.
    pub(crate) fn new(event: E, current_target: Option<EventTarget>) -> Self {
        Self {
            event,
            current_target: current_target.map(|ct| ct.unchecked_into::<C>()),
        }
    }

    /// Returns the event's current target, or [`None`](Option::None) if not available or the type of the
    /// current target does not match type `C`.
    pub fn current_target(&self) -> Option<C> {
        self.current_target.clone()
    }
}

impl<E: JsCast + Clone + Into<Event>, C: JsCast> Deref for TypedEvent<E, C> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        &self.event
    }
}
