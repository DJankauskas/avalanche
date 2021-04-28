use std::{marker::PhantomData, ops::Deref};
use wasm_bindgen::JsCast;

pub(crate) use web_sys::AnimationEvent;
pub(crate) use web_sys::CompositionEvent;
pub(crate) use web_sys::DragEvent;
pub(crate) use web_sys::Event;
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
    phantom: PhantomData<C>,
}

impl<E: JsCast + Clone + Into<Event>, C: JsCast> TypedEvent<E, C> {
    /// Constructs a new [`TypedEvent`] using the given event. It is the caller's
    /// responsibility to ensure the component type `C` is correct in context.
    pub(crate) fn new(event: E) -> Self {
        Self {
            event,
            phantom: PhantomData,
        }
    }

    /// Returns the event's current target, or [`None`](Option::None) if not available or the type of the
    /// current target does not match type `C`.
    pub fn current_target(&self) -> Option<C> {
        let event: Event = self.event.clone().into();
        Some(event.current_target()?.dyn_into::<C>().ok()?)
    }
}

impl<E: JsCast + Clone + Into<Event>, C: JsCast> Deref for TypedEvent<E, C> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        &self.event
    }
}

//TODO: handle unstable
//pub use web_sys::ClipboardEvent;
