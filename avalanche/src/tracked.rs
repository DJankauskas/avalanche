use std::{
    cell::Cell,
    iter::FusedIterator,
    ops::{Bound, Deref, DerefMut, Index, IndexMut},
};

use crate::hooks::Gen;

#[derive(Copy, Clone)]
/// A wrapped value that stores whether it has been modified since the last render.
/// 
/// To access the value, use [tracked], and to check whether it is updated, use [updated].
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
    pub __avalanche_internal_updated: bool,
}

impl<T> Tracked<T> {
    /// Creates a tracked value with value `value`. If it has been updated, `updated` should be `true`,
    /// otherwise `false`. Usually, you will not need to create `Tracked` values manually.
    pub fn new(value: T, updated: bool) -> Self {
        Self {
            __avalanche_internal_value: value,
            __avalanche_internal_updated: updated,
        }
    }

    /// Returns whether the tracked value has been updated since the last render.
    #[doc(hidden)]
    pub fn internal_updated(&self) -> bool {
        self.__avalanche_internal_updated
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
/// # use avalanche::{component, View, Tracked, tracked, updated};
/// #[component]
/// fn ComponentBody() -> View {
///     let a = Tracked::new(8, true);
///     let b: Tracked<u32> = tracked!(a);
/// 
///     // c has type Tracked<u32>
///     let c = tracked!(a) + tracked!(b);
///     assert_eq!(tracked!(c), 16);
///     assert!(updated!(c));
/// 
///     // Tracked closure behavior
///     let d = Tracked::new(2, false);
///     let closure = || {
///         let a = Tracked::new(2, true);
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

/// Returns whether the given [Tracked](crate::tracked::Tracked) value has been updated.
/// 
/// It behaves similarly to [tracked](tracked!), so it returns `Tracked<bool>` in a `#[component]`
/// context, and `bool` everywhere else. It does not take ownership of the input value.
#[macro_export]
macro_rules! updated {
    ($e:expr) => {
        $e.__avalanche_internal_updated
    };
}

/// A wrapped [Vec](std::vec::Vec) tracking each element's updated status individually.
/// 
/// This type is returned by the [vec](crate::hooks::vec) hook. It means that, when modifying or creating individual
/// elements with `VecSetter`, only those individual elements will be marked as updated. That allows for efficient
/// rendering of lists of data, as only changed children derived from changed elements will need to be rerendered.
/// 
/// Provided iteration methods like [iter](crate::tracked::Vec::iter()) return `Tracked<T>` instead of `T`, marking whether individual elements
/// are updated or not. Indexing with `[]`, and methods accessed by dereferencing `tracked::Vec` to get a slice, just return `T`,
/// making them less granular as they will end up marked as updated if any element is.
/// 
/// Individual elements are not tracked _positionally_, or by index, but by _identity_. That means that, if we have a `tracked::Vec` `data`
/// with elements `[1, 3, 4]`, and we then call `data.insert(1, 2)`, `tracked!(tracked!(data).iter().nth(1).unwrap())` will be marked as updated on rerender, but 
/// every other element will be marked as not updated.
pub struct Vec<T> {
    /// The elements being tracked by the data structure.
    pub(crate) data: std::vec::Vec<T>,
    /// The generation during which corresponding elements of `data` (by index)
    /// were last created or modified. This value can never be 0.
    pub(crate) gens: std::vec::Vec<Gen>,
    /// The current generation of data modification of the data structure.
    pub(crate) curr_gen: Cell<Gen>,
}

/// Allows mutating a [tracked::Vec](crate::tracked::Vec)'s underlying [Vec](std::vec::Vec) data.
/// 
/// On drop, marks all elements of that data as updated.
pub struct VecMutRef<'a, T> {
    vec: &'a mut Vec<T>,
}

impl<'a, T> Deref for VecMutRef<'a, T> {
    type Target = ::std::vec::Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.vec.data
    }
}

impl<'a, T> DerefMut for VecMutRef<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vec.data
    }
}

impl<'a, T> Drop for VecMutRef<'a, T> {
    /// After arbitrary modifications were performed by consumers of the mut ref,
    /// resize `gens` and fill it with the next generation to mark all values as (potentially)
    /// updated.
    fn drop(&mut self) {
        let data_len = self.vec.data.len();
        let curr_gen = self.vec.curr_gen.get();
        if data_len > self.vec.gens.len() {
            self.vec.gens.fill(curr_gen);
            self.vec.gens.resize(data_len, curr_gen);
        } else {
            self.vec.gens.resize(data_len, curr_gen);
            self.vec.gens.fill(curr_gen);
        }
    }
}

impl<T> Vec<T> {
    pub(crate) fn new(data: std::vec::Vec<T>, gen: Gen) -> Self {
        Self {
            gens: vec![gen.next(); data.len()],
            data,
            curr_gen: Cell::new(gen),
        }
    }

    // pub fn as_untracked_slice

    /// Provides mutable access to the underlying [`std::vec::Vec<T>`] backing the `Vec`.
    /// This marks all elements of the `Vec` as having been updated, regardless
    /// of what actions were performed on it, if any.
    pub fn as_raw_vec(&mut self) -> VecMutRef<T> {
        VecMutRef { vec: self }
    }

    pub fn iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = Tracked<&T>> + DoubleEndedIterator + FusedIterator + ExactSizeIterator + 'a
    {
        self.data
            .iter()
            .zip(self.gens.iter())
            .map(move |(val, gen)| Tracked {
                __avalanche_internal_value: val,
                __avalanche_internal_updated: gen.updated(self.curr_gen.get()),
            })
    }

    pub fn windows<'a>(
        &'a self,
        size: usize,
    ) -> impl Iterator<Item = Tracked<&[T]>> + DoubleEndedIterator + FusedIterator + ExactSizeIterator + 'a
    {
        self.data
            .windows(size)
            .zip(self.gens.windows(size))
            .map(move |(val, gen)| Tracked {
                __avalanche_internal_value: val,
                __avalanche_internal_updated: gen
                    .iter()
                    .any(|&val| val.updated(self.curr_gen.get())),
            })
    }

    pub fn chunks<'a>(
        &'a self,
        chunk_size: usize,
    ) -> impl Iterator<Item = Tracked<&[T]>> + DoubleEndedIterator + FusedIterator + ExactSizeIterator + 'a
    {
        self.data
            .chunks(chunk_size)
            .zip(self.gens.chunks(chunk_size))
            .map(move |(val, gen)| Tracked {
                __avalanche_internal_value: val,
                __avalanche_internal_updated: gen
                    .iter()
                    .any(|&val| val.updated(self.curr_gen.get())),
            })
    }

    pub fn chunks_exact<'a>(
        &'a self,
        chunk_size: usize,
    ) -> impl Iterator<Item = Tracked<&[T]>> + DoubleEndedIterator + FusedIterator + ExactSizeIterator + 'a
    {
        self.data
            .chunks_exact(chunk_size)
            .zip(self.gens.chunks_exact(chunk_size))
            .map(move |(val, gen)| Tracked {
                __avalanche_internal_value: val,
                __avalanche_internal_updated: gen
                    .iter()
                    .any(|&val| val.updated(self.curr_gen.get())),
            })
    }

    pub fn rhunks<'a>(
        &'a self,
        chunk_size: usize,
    ) -> impl Iterator<Item = Tracked<&[T]>> + DoubleEndedIterator + FusedIterator + ExactSizeIterator + 'a
    {
        self.data
            .rchunks(chunk_size)
            .zip(self.gens.rchunks(chunk_size))
            .map(move |(val, gen)| Tracked {
                __avalanche_internal_value: val,
                __avalanche_internal_updated: gen
                    .iter()
                    .any(|&val| val.updated(self.curr_gen.get())),
            })
    }

    pub fn rchunks_exact<'a>(
        &'a self,
        chunk_size: usize,
    ) -> impl Iterator<Item = Tracked<&[T]>> + DoubleEndedIterator + FusedIterator + ExactSizeIterator + 'a
    {
        self.data
            .rchunks_exact(chunk_size)
            .zip(self.gens.rchunks_exact(chunk_size))
            .map(move |(val, gen)| Tracked {
                __avalanche_internal_value: val,
                __avalanche_internal_updated: gen
                    .iter()
                    .any(|&val| val.updated(self.curr_gen.get())),
            })
    }

    pub fn push(&mut self, value: T) {
        self.data.push(value);
        self.gens.push(self.curr_gen.get().next());
    }

    pub fn pop(&mut self) -> Option<T> {
        let _ = self.gens.pop();
        self.data.pop()
    }

    pub fn insert(&mut self, index: usize, element: T) {
        self.data.insert(index, element);
        self.gens.insert(index, self.curr_gen.get().next());
    }

    pub fn remove(&mut self, index: usize) -> T {
        self.gens.remove(index);
        self.data.remove(index)
    }

    pub fn swap_remove(&mut self, index: usize) -> T {
        self.gens.swap_remove(index);
        self.data.swap_remove(index)
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.gens.clear();
    }

    pub fn retain<F: FnMut(&T) -> bool>(&mut self, mut f: F) {
        let mut gens_idx = 0;
        let gens_mut = &mut self.gens;
        self.data.retain(|val| {
            let retain = f(val);
            if !retain {
                gens_mut[gens_idx] = Gen { gen: 0 };
            }
            gens_idx += 1;
            retain
        });
        self.gens.retain(|&gen| gen != Gen { gen: 0 });
    }

    pub fn reserve(&mut self, additional: usize) {
        self.data.reserve(additional);
        self.gens.reserve(additional);
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        self.data.reserve_exact(additional);
        self.gens.reserve_exact(additional);
    }

    pub fn shrink_to_fit(&mut self) {
        self.data.shrink_to_fit();
        self.gens.shrink_to_fit();
    }
}

impl<T> IntoIterator for Vec<T> {
    type Item = Tracked<T>;
    type IntoIter = std::vec::IntoIter<Tracked<T>>;

    #[allow(clippy::needless_collect)]
    fn into_iter(self) -> Self::IntoIter {
        let curr_gen = self.curr_gen.get();

        // Note: collect necessary as map signature uses Gen, a private type, which we cannot 
        // export
        let data: std::vec::Vec<_> = self
            .data
            .into_iter()
            .zip(self.gens.into_iter())
            .map(move |(val, gen)| Tracked {
                __avalanche_internal_value: val,
                __avalanche_internal_updated: gen.updated(curr_gen),
            })
            .collect();
        data.into_iter()
    }
}

impl<T> Deref for Vec<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &*self.data
    }
}

impl<T, Idx: TrackedVecIndex<T>> Index<Idx> for Vec<T> {
    type Output = Idx::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        index.get(self)
    }
}

impl<T, Idx: TrackedVecIndex<T>> IndexMut<Idx> for Vec<T> {
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        self.gens[index.span()].fill(self.curr_gen.get().next());
        index.get_mut(self)
    }
}

mod private {
    pub trait Sealed {}

    impl Sealed for usize {}
}

/// Emulates the role of [std::slice::SliceIndex].
pub trait TrackedVecIndex<T>: private::Sealed + Copy {
    type Output: ?Sized;

    fn get(self, vec: &Vec<T>) -> &Self::Output;

    fn get_mut(self, vec: &mut Vec<T>) -> &mut Self::Output;

    /// returns [start, end]
    fn span(self) -> (Bound<usize>, Bound<usize>);
}

impl<T> TrackedVecIndex<T> for usize {
    type Output = T;

    fn get(self, vec: &Vec<T>) -> &Self::Output {
        &vec.data[self]
    }

    fn get_mut(self, vec: &mut Vec<T>) -> &mut Self::Output {
        &mut vec.data[self]
    }

    fn span(self) -> (Bound<usize>, Bound<usize>) {
        (Bound::Included(self), Bound::Included(self))
    }
}

// TODO: implement for all SliceIndex types

// impl<T, Idx: SliceIndex<[T]>> Index<Idx> for TrackedVec<T> {
//     type Output = Idx::Output;

//     fn index(&self, index: Idx) -> &Self::Output {
//         Index::index(&self.data, index)
//     }
// }
