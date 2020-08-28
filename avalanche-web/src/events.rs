//use wasm_bindgen::JsCast;

pub use web_sys::Event;
pub use web_sys::AnimationEvent;
pub use web_sys::CompositionEvent;
pub use web_sys::DragEvent;
pub use web_sys::KeyboardEvent;
pub use web_sys::FocusEvent;
pub use web_sys::MouseEvent;
pub use web_sys::ProgressEvent;
pub use web_sys::PointerEvent;
pub use web_sys::TransitionEvent;
pub use web_sys::TouchEvent;
pub use web_sys::WheelEvent;

//TODO: handle unstable
//pub use web_sys::ClipboardEvent;

// pub struct Event<T: JsCast> {
//     internal_event: web_sys::Event,
//     phantom: std::marker::PhantomData<T>
// }

// impl<T: JsCast> Event<T> {
//     pub fn current_target(&self) -> Option<T> {
//         let current_target = self.internal_event.current_target();
//         current_target.and_then(|c| c.dyn_into::<T>().ok())
//     }
// }

// impl<T: JsCast> std::ops::Deref for Event<T> {
//     type Target = web_sys::Event;

//     fn deref(&self) -> &Self::Target {
//         &self.internal_event
//     }
// }