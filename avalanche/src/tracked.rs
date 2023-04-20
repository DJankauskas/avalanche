use std::marker::PhantomData;

#[derive(Copy, Clone)]
/// A wrapped value that stores whether it has been modified since the last render.
///
/// To access the value, use [tracked], and to check whether it is updated, use [updated].
#[non_exhaustive]
pub struct Tracked<T> {
    /// The value of the tracked value
    /// Public due to implementation of [tracked!] macro,
    /// but not semver stable and must only be used internally
    #[doc(hidden)]
    pub __avalanche_internal_value: T,
    /// Whether the tracked value has been updated since the last render
    /// Public due to implementation of [updated!] macro,
    /// but not semver stable and must only be used internally
    #[doc(hidden)]
    pub __avalanche_internal_gen: Gen<'static>,
}

impl<T> Tracked<T> {
    /// Creates a tracked value with value `value` created at the given generation. [Gen] can be
    /// obtained using the [store](crate::hooks::store) hook.
    pub fn new(value: T, gen: Gen) -> Self {
        Self {
            __avalanche_internal_value: value,
            __avalanche_internal_gen: gen.gen.into(),
        }
    }

    /// Provides mutable access to the tracked value, updating its generation to
    /// the current (provided) one.
    pub fn mutate(&mut self, gen: Gen) -> &mut T {
        if self.__avalanche_internal_gen < gen {
            self.__avalanche_internal_gen = gen.gen.into();
        };
        &mut self.__avalanche_internal_value
    }
}

/// Unwraps and propogates a [Tracked](crate::tracked::Tracked) value.
///
/// By default, `tracked` takes ownership of its input. Passing a reference instead
/// will return a reference to the input's inner value.
///
/// Within a `#[component]` body, wraps the expression containing it in a `Tracked` instance maintaining
/// whether any of the `tracked!()` values were updated. Closure values are wrapped in `Tracked` if they contain `tracked`
/// or `updated` calls with external identifiers as inputs.
///
/// Outside of `#[component]`, provides access to the tracked value
/// without rewrapping the containing expression.
///
/// ## Example
/// ```rust
/// # use avalanche::{component, Tracked, View, tracked, updated, state};
/// #[component]
/// fn ComponentBody(a: u32, d: u32) -> View {
///     // a has been updated since the last render, d has not been
///
///     let b: Tracked<u32> = tracked!(a);
///
///     // c has type Tracked<u32>
///     let c = tracked!(a) + tracked!(b);
///     assert_eq!(tracked!(c), 16);
///     assert!(updated!(c));
///
///     // Tracked closure behavior
///     let closure = || {
///         let (a, _) = state(self, || 2);
///         tracked!(a) + tracked!(d)
///     };
///     // closure depends on external identifier d,
///     // so it is wrapped in Tracked, but since d is not updated,
///     // neither is closure.
///     assert!(!updated!(closure));
///
///     // ..
///     # ().into()
/// }
/// ```
#[macro_export]
macro_rules! tracked {
    ($e:expr) => {
        $e.__avalanche_internal_value
    };
}

/// Like the [tracked](tracked!) macro, but more performant when changes to the _location_ of a [Tracked] value is not
/// important, but its identity, or _key_, is.
///
/// This is an experimental API.
///
/// For example, for a variable `vec` of type
/// `Tracked<Vec<Tracked<T>>`, in the expression `tracked!(tracked!(vec)[0])`, the location of the resulting value
/// is important because we retrieve it by index. However, with this construct, any change within `vec` will
/// mark the value as updated, even if what is at the zeroth index stayed the same.
///
/// However, when we know that the location of a tracked value
/// is either not important or always static, `keyed_tracked` should be used instead for values coming
/// from the `store` hook to enable more efficient, fine-grained updates.
///
// TODO: is this the right approach? Also, better documentation + examples
#[macro_export]
macro_rules! tracked_keyed {
    ($e:expr) => {
        $e.__avalanche_internal_value
    };
}

/// Returns whether the given [Tracked] value has been updated since the last render.
///
/// It can only be called within a `#[component]` function, and behaves similarly to [tracked](tracked!).
#[macro_export]
macro_rules! updated {
    ($e:expr) => {
        ::std::compile_error!("updated must be invoked within a #[component]")
    };
    (@internal $internal_ctxt:expr; $gen:expr) => {
        $internal_ctxt.gen <= $gen
    };
}

/// Like [updated], but with the tracking performance properties of [tracked_keyed].
///
/// This is an experimental API.
#[macro_export]
macro_rules! updated_keyed {
    ($e:expr) => {
        ::std::compile_error!("updated must be invoked within a #[component]")
    };
    (@internal $internal_ctxt:expr; $gen:expr) => {
        $internal_ctxt.gen <= $gen
    };
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
/// Represents the generation in which a value was last updated, which may be the generation of
/// its creation. This is for internal use only and is not bound by semver.
///
/// Generations are ordered: a more recent generation is greater than an older one.
pub(crate) struct InternalGen {
    pub(crate) gen: u32,
}

// TODO: support wrap edge cases
impl InternalGen {
    /// Creates the `Gen` at which a component tree starts at.
    pub(crate) fn new() -> Self {
        InternalGen { gen: 1 }
    }

    /// Increments the generation.
    pub(crate) fn inc(&mut self) {
        self.gen = self.gen.wrapping_add(1);
    }
}

impl<'a> From<InternalGen> for Gen<'a> {
    fn from(gen: InternalGen) -> Self {
        Self {
            gen,
            phantom: PhantomData,
        }
    }
}

/// Represents what iteration an `avalanche` tree is at for tracking purposes.
/// Mostly useful for creating and mutating [`Tracked`] values.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Gen<'a> {
    /// The actual generation of the wrapper.
    pub(crate) gen: InternalGen,
    /// Forces values to be treated as references,
    /// preventing them from being stored across generations and
    /// creating outdated tracking values.
    phantom: PhantomData<&'a InternalGen>,
}

impl<'a> Gen<'a> {
    /// Creates a `Gen` that will always register as updated if `updated`
    /// is true, and always not updated if false. Using this function is usually
    /// not what you want, and is generally helpful only for manual component implementations
    /// that need a default not updated generation.
    pub fn escape_hatch_new(updated: bool) -> Self {
        let gen = if updated { u32::MAX } else { 0 };
        Self {
            gen: InternalGen { gen },
            phantom: PhantomData,
        }
    }
}
