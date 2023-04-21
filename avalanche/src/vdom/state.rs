use std::{any::Any, marker::PhantomData, panic::Location};

/// A wrapper over a `Box`, with a raw pointer to its memory, so that
/// references derived from it do not have a `Box`'s provenance and
/// remain valid when the `SharedBox` is moved.
pub struct SharedBox<T: ?Sized> {
    value: *mut T,
    _marker: PhantomData<T>,
}

impl<T: ?Sized> SharedBox<T> {
    pub fn new(value: Box<T>) -> Self {
        Self {
            value: Box::into_raw(value),
            _marker: PhantomData,
        }
    }

    /// safety: Caller must ensure that the box is destroyed only after the end of
    /// the provided lifetime 'a.
    /// a reference from `get_mut` must not be active while
    /// a reference returned from this method is
    unsafe fn get_ref<'a>(&self) -> &'a T {
        &*self.value
    }

    pub fn get_mut(&mut self) -> &mut T {
        // safety: as the receiver of this method is &mut,
        // Rust reference invariants ensure this is safe
        unsafe { &mut *self.value }
    }
}

impl<T: ?Sized> Drop for SharedBox<T> {
    fn drop(&mut self) {
        // safety: by construction of `self.value` in `new`, it is
        // a valid `Box`-allocated memory location, and thus can be converted back
        // into a `Box`.
        unsafe {
            drop(Box::from_raw(self.value));
        }
    }
}

/// A wrapper over `ComponentState` allowing for safe additions and immutable access of state duing component rendering.
pub(crate) struct ComponentStateAccess<'a> {
    /// `inner`'s `Rc` elements MUST NOT be removed or destroyed in any fashion
    /// during the lifetime `'a`. In addition, `&mut` references pointing to the
    /// interior of `Rc` elements MUST NOT be created or accessed during the lifetime `'a`.
    /// Violating this leads to memory unsafety.
    inner: &'a mut super::ComponentState,
}

impl<'a> ComponentStateAccess<'a> {
    pub fn new(inner: &'a mut super::ComponentState) -> Self {
        Self { inner }
    }

    pub fn get_or_insert_with(
        &mut self,
        key: Location<'static>,
        value: impl FnOnce() -> SharedBox<dyn Any>,
    ) -> &'a dyn Any {
        let elem = self.inner.entry(key).or_insert_with(value);

        // safety: The box cannot be destroyed or mutably dereferenced until the end of the lifetime
        // 'a, as per the guarantees on inner.
        unsafe { elem.get_ref() }
    }
}
