use std::{any::Any, iter::FusedIterator, ops::{Bound, Deref, DerefMut, Index, IndexMut}, rc::Rc};

use crate::{renderer::Scheduler, shared::Shared, ComponentPos, UseState, UseStateSetter};

#[derive(Copy, Clone)]
pub struct Tracked<T> {
    /// The value of the tracked value
    /// Public due to implementation of [tracked!] macro,
    /// but not semver stable and must only be used internally
    #[doc(hidden)]
    pub __avalanche_internal_value: T,
    /// Whether the tracked value has been updated since the last render
    updated: bool,
}

impl<T> Tracked<T> {
    // TODO: should this be a public api?
    pub fn new(value: T, updated: bool) -> Self {
        Self {
            __avalanche_internal_value: value,
            updated,
        }
    }

    /// Returns whether the tracked value has been updated since the last render.
    #[doc(hidden)]
    pub fn internal_updated(&self) -> bool {
        self.updated
    }
}

// TODO: Add examples.
/// Unwraps a [Tracked] value.
/// Within a `#[component]` or `#[hook]` context, wraps the expression containing it in a [Tracked] instance maintaining
/// whether any of the `tracked!()` values were updated.
/// Otherwise, provides access to the tracked value without rewrapping the containing expression.
#[macro_export]
macro_rules! tracked {
    ($e:expr) => {
        $e.__avalanche_internal_value
    };
}

// TODO: Add examples.
/// Yields whether a [Tracked] value has been updated.
/// Within a `#[component]` or `#[hook]` context, wraps the expression containing it in a [Tracked] instance maintaining
/// whether any of the `tracked!()` or `updated!()` values were updated.
/// Otherwise, returns a `bool`.
#[macro_export]
macro_rules! updated {
    ($e:expr) => {
        ::avalanche::Tracked::internal_updated(&$e)
    };
}

pub struct UseVec<T: 'static> {
    state: UseState<Vec<T>>,
}

impl<T> UseVec<T> {
    /// This function is called by the framework. Users gain access to a reference to the
    /// state type and a setter when they call it.
    #[doc(hidden)]
    pub fn hook<'a>(
        &'a mut self,
        component_pos: ComponentPos<'a>,
        scheduler: &Shared<dyn Scheduler>,
        get_self: fn(&mut Box<dyn Any>) -> &mut UseVec<T>,
    ) -> impl FnOnce() -> (Tracked<&'a Vec<T>>, UseStateSetter<Vec<T>>) {
        let scheduler = scheduler.clone();
        let closure = move || {
            let updated = self.state.updated;
            if self.state.updated {
                self.state.updated = false;
            }
            if let None = self.state.state {
                self.state.state = Some(Vec::new());
                self.state.updated = true;
            };
            if updated {
                self.state.state.as_mut().unwrap().current_gen += 1;
            }
            let state_ref = Tracked::new(self.state.state.as_ref().unwrap(), updated);
            let setter = UseStateSetter::new(
                component_pos,
                scheduler,
                Rc::new(move |val| &mut get_self(val).state),
            );
            (state_ref, setter)
        };
        closure
    }
}

impl<T> Default for UseVec<T> {
    fn default() -> Self {
        Self {
            state: Default::default(),
        }
    }
}

pub struct Vec<T> {
    /// The elements being tracked by the data structure.
    data: std::vec::Vec<T>,
    /// The generation during which corresponding elements of `data` (by index)
    /// were last created or modified. This value can never be 0.
    gens: std::vec::Vec<u32>,
    /// The current generation of data modification of the data structure.
    current_gen: u32,
}

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
        let new_gen = self.vec.current_gen + 1;
        if data_len > self.vec.gens.len() {
            self.vec.gens.fill(new_gen);
            self.vec.gens.resize(data_len, new_gen);
        } else {
            self.vec.gens.resize(data_len, new_gen);
            self.vec.gens.fill(new_gen);
        }
    }
}

impl<T> Vec<T> {
    fn new() -> Self {
        Self {
            data: std::vec::Vec::new(),
            gens: std::vec::Vec::new(),
            current_gen: 0,
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
                updated: *gen >= self.current_gen,
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
                updated: gen.iter().any(|&val| val >= self.current_gen),
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
                updated: gen.iter().any(|&val| val >= self.current_gen),
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
                updated: gen.iter().any(|&val| val >= self.current_gen),
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
                updated: gen.iter().any(|&val| val >= self.current_gen),
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
                updated: gen.iter().any(|&val| val >= self.current_gen),
            })
    }

    pub fn push(&mut self, value: T) {
        self.data.push(value);
        self.gens.push(self.current_gen + 1);
    }

    pub fn pop(&mut self) -> Option<T> {
        let _ = self.gens.pop();
        self.data.pop()
    }

    pub fn insert(&mut self, index: usize, element: T) {
        self.data.insert(index, element);
        self.gens.insert(index, self.current_gen + 1);
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
                gens_mut[gens_idx] = 0;
            }
            gens_idx += 1;
            retain
        });
        self.gens.retain(|&gen| gen != 0);
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
        self.gens[index.span()].fill(self.current_gen + 1);
        index.get_mut(self)
    }
}

mod private {
    pub trait Sealed {}

    impl Sealed for usize {}
}

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
