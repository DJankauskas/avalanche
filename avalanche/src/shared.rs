use std::rc::Rc;
use std::cell::RefCell;

pub struct Shared<T: ?Sized> {
    rc: Rc<RefCell<T>>
}

impl<T> Shared<T> {
    pub fn new(val: T) -> Self {
        Shared {
            rc: Rc::new(RefCell::new(val))
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
    /// # use shared::Shared;
    /// let num = Shared::new(10u8);
    /// let squared = num.exec(|*n| n * n);
    /// println!("Shared squared: {}", squared); //prints "100"
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
    /// # use shared::Shared;
    /// let sequence = Shared::new(vec![1u8, 2, 3]);
    /// sequence.exec_mut(|nums| nums.iter_mut().map(|num| num *= *num));
    /// sequence.exec(|nums| println!("{:?}"), nums); //prints "[1,4,9]"
    /// ```
    pub fn exec_mut<Ret, F: FnOnce(&mut T) -> Ret>(&mut self, f: F) -> Ret {
        f(&mut self.rc.borrow_mut())
    }
}

impl<T: ?Sized> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared {
            rc: self.rc.clone()
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

//Hack to allow creation of unsized Shared content
//as CoerceUnsized is nightly-only
//remove if it becomes unneeded
impl<T: ?Sized> From<Box<RefCell<T>>> for Shared<T> {
    fn from(b: Box<RefCell<T>>) -> Self {
        Self {
            rc: b.into()
        }
    }
}