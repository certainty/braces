use std::cell::RefCell;
use std::rc::Rc;

/// A place is a set-able place
/// There are a number of things that are set-able places in scheme
/// 1. cons cell in proper and improper list
/// 2. vector elements
/// 4. byte-vector elements
/// 5. variables
#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct Place<T> {
    inner: Rc<RefCell<T>>,
}

impl<T> Place<T> {
    pub fn new(v: T) -> Self {
        Self {
            inner: Rc::new(RefCell::new(v)),
        }
    }

    pub fn to_inner(self) -> T {
        self.inner.into_inner()
    }

    pub fn set(&mut self, v: T) {
        self.inner.replace(v);
    }

    pub fn with_ref<F, O>(&self, f: F) -> O
    where
        F: FnOnce(&T) -> O,
    {
        f(&self.inner.borrow())
    }
}

impl<T> PartialEq for Place<T> {
    fn eq(&self, other: &Place<T>) -> bool {
        self.inner.as_ptr() == other.inner.as_ptr()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::value::Value;

    #[test]
    pub fn test_ref_value() {
        let v1 = Place::new(Value::Bool(true));
        let v2 = Place::new(Value::Bool(true));
        let v3 = v1.clone();

        assert_ne!(v1, v2);
        assert_eq!(v1, v1);
        assert_eq!(v1, v3);
    }
}
