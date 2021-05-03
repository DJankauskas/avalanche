use std::cell::RefCell;
use std::rc::Rc;

pub struct Shared<T: ?Sized> {
    rc: Rc<RefCell<T>>,
}

impl<T> Shared<T> {
    pub fn new(val: T) -> Self {
        Shared {
            rc: Rc::new(RefCell::new(val)),
        }
    }
}

impl<T: ?Sized> Shared<T> {
    /// Allows constructing an instance of Shared with a `?Sized T`. 
    // Note: this leaks implemmentation details, but this is necessary as `CoerceUnsized` is unstable.
    pub(crate) fn new_dyn(val: Rc<RefCell<T>>) -> Self {
        Shared {
            rc: val
        }
    }
}

impl<T: ?Sized> Shared<T> {
    /// Executes the given function with an immutable reference to the wrapped value.
    ///
    /// # Arguments
    ///
    /// * `func` - The function to execute with a reference to the wrapped value.
    ///
    /// # Panics
    ///
    /// Panics if the wrapped value is being used by an `exec_mut` call.
    /// This is only possible if the method was called on a different `Shared` variable corresponding to the same shared value.
    ///
    /// # Example
    ///
    /// ```
    /// # use avalanche::shared::Shared;
    /// let num = Shared::new(10u8);
    /// let squared = num.exec(|&n| n * n);
    /// assert_eq!(squared, 100);
    /// ```
    pub fn exec<Ret, F: FnOnce(&T) -> Ret>(&self, f: F) -> Ret {
        f(&self.rc.borrow())
    }

    /// Executes the given function with a mutable reference to the wrapped value.
    ///
    /// # Arguments
    ///
    /// * `func` - The function to execute with a reference to the wrapped value.
    ///
    /// # Panics
    /// Panics if the wrapped value is being used by another `exec` or `exec_mut` call.
    /// This is only possible if the method was called on a different `Shared` variable corresponding to the same shared value.
    ///
    /// # Example
    ///
    /// ```
    /// # use avalanche::shared::Shared;
    /// let sequence = Shared::new([1u8, 2, 3]);
    /// sequence.exec_mut(|nums| nums.iter_mut().for_each(|num| *num *= *num));
    /// sequence.exec(|nums| assert_eq!(nums, &[1, 4, 9]));
    /// ```
    pub fn exec_mut<Ret, F: FnOnce(&mut T) -> Ret>(&self, f: F) -> Ret {
        f(&mut self.rc.borrow_mut())
    }
}

impl<T: ?Sized> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared {
            rc: self.rc.clone(),
        }
    }
}

impl<T: Default> Default for Shared<T> {
    fn default() -> Self {
        Self::new(Default::default())
    }
}
impl<T> From<T> for Shared<T> {
    fn from(val: T) -> Self {
        Self::new(val)
    }
}
