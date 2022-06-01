use std::any::Any;
use std::cell::Cell;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::mem::ManuallyDrop;

/// A trait that signals a type supports type erasure within the `DynBox` and
/// `DynAny` types. End users should not need to implement this trait, except if
/// implementing custom native components.

/// Safety: Static must be either the type for which this is implemented,
/// or, if the type is the result of parameterizing a type constructor
/// with the lifetime 'a, the result of that type constructor parameterized
/// with 'static. Every type (constructor) must have at most one implementation
/// of this trait. Types that implement this trait may only be parameterized over
/// the lifetime 'a (and types).
#[doc(hidden)]
pub unsafe trait AnyRef<'a>: 'a {
    type Static: 'static;
}

// Safety: trivial.
unsafe impl<'a> AnyRef<'a> for () {
    type Static = ();
}

/// This macro should only be used by implementors of native components.
/// Implement `AnyRef<'a>` for either a `'static` type or a type parameterized
/// by one lifetime parameter. For a `'static` type `T`, implement with 
/// `impl_any_ref!(T)`, and for a type `T` parameterized with a lifetime parameter,
/// implement with `impl_any_ref!(T<'a>)`. Note that this macro does not currently support
/// generic types. 
// Safety: the restrictions on macro inputs, as well as how they are used, 
// ensures that implementations satisfy the requirements of `AnyRef`.
#[macro_export]
macro_rules! impl_any_ref {
    ($ident:ident) => {
       unsafe impl<'a> ::avalanche::any_ref::AnyRef<'a> for $ident {
           type Static = $ident ;
       } 
    };

    ($ident:ident <$l:lifetime>) => {
       unsafe impl<$l> ::avalanche::any_ref::AnyRef<$l> for $ident<$l> {
           type Static = $ident <'static>;
       } 
    };
}

/// Safety: T and U must be valid inputs to `std::mem::transmute`.
/// However this implementation works in generic contexts. Care must
/// be taken to ensure these invariants are upheld in such code.
unsafe fn transmute<T, U>(val: T) -> U {
    let manually_drop = ManuallyDrop::new(val);
    // safety: by the invariants the caller must maintain.
    // the use of manually_drop ensures that we will not drop data the copy
    // then uses.
    std::mem::transmute_copy::<T, U>(&manually_drop)
}


// DynBox works by casting potentially non-'static data to a type
// that is guaranteed to be 'static, using an unsafe trait to maintain
// the invariant that such a cast must be legal and provide the 'static
// type. At downcasting, if downcasting to the 'static type succeeds,
// the cast is performed in reverse. A PhantomData instance taking in 
// a lifetime is used to ensure a DynBox or DynRef cannot outlast the
// data it is holding or referencing.

/// Allows heap-allocated, owned, and type-erased manipulation of data
/// that is potentially non-`'static`.
pub struct DynBox<'a> {
    data: Box<dyn Any>,
    /// Uses lifetime and introduces covariance and contravariance
    _phantom: PhantomData<Cell<&'a ()>>,
}


impl<'a> DynBox<'a> {
    pub(crate) fn new<T: AnyRef<'a>>(val: T) -> Self {
        // Safety: by the invariants on AnyRef this cast is not UB.
        // Casting away a non-'static reference is acceptable because we
        // do not access the underlying transmuted variable at all before it
        // is transmuted back to its original type.
        let val = unsafe { transmute::<T, T::Static>(val) };
        Self {
            data: Box::new(val),
            _phantom: PhantomData,
        }
    }
    
    pub(crate) fn as_ref<'r>(&'r self) -> DynRef<'a, 'r> {
        DynRef { data: &*self.data, _phantom: PhantomData }
    }

    /// Attempts to downcast a `DynBox` to a concrete value.
    pub fn downcast<T: AnyRef<'a>>(self) -> Result<T, Self> {
        match Box::<dyn Any>::downcast::<T::Static>(self.data) {
            Ok(val) => {
                // Safety: by the guarentees on AnyRef the cast is legal.
                // The lifetime is also valid because it is guarenteed to
                // be the case by the bounds on the new function.
                let val = unsafe { transmute::<T::Static, T>(*val) };
                Ok(val)
            }
            Err(data) => Err(DynBox {
                data,
                _phantom: PhantomData,
            }),
        }
    }
}

impl<'a> Debug for DynBox<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DynBox").finish_non_exhaustive()
    }
}

/// A reference to type-erased data.
pub struct DynRef<'a, 'r> {
    data: &'r dyn Any,
    /// Uses lifetime and introduces covariance and contravariance
    _phantom: PhantomData<Cell<&'a ()>>,
}


impl<'a, 'r> DynRef<'a, 'r> {
    /// Returns some reference to the inner value if it is of type `T`, or `None` if it isn’t.
    pub fn downcast_ref<T: AnyRef<'a>>(self) -> Option<&'a T> {
        match self.data.downcast_ref::<T::Static>() {
            Some(val) => {
                // Safety: by the guarentees on AnyRef the cast is legal.
                // The lifetime is also valid because it is guarenteed to
                // be the case by the bounds on the new function.
                let val = unsafe { transmute::<&T::Static, &T>(val) };
                Some(val)
            },
            None => None
        }
    }
}

impl<'a, 'r> Debug for DynRef<'a, 'r> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DynRefp").finish_non_exhaustive()
    }
}
