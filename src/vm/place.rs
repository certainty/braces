use std::cell::RefCell;
use std::rc::Rc;

/// A Reference is a setable place. The scheme report specifies that
/// some things are implicitly locations (references) or collections of locations (references).
/// This type is what the VM uses to represent those.
///
/// Examples of locations in the report are:
/// 1. cons cell in proper and improper list
/// 2. vector elements
/// 4. byte-vector elements
/// 5. strings
/// 6. variables
#[repr(transparent)]
#[derive(Debug, Clone, PartialEq)]
pub struct Reference<T> {
    inner: Rc<RefCell<T>>,
}

impl<T: Clone> Reference<T> {
    pub fn new(v: T) -> Self {
        Self {
            inner: Rc::new(RefCell::new(v)),
        }
    }

    /// ref_eq is true iff the value that the reference points to is the same
    pub fn ref_eq(lhs: &Self, rhs: &Self) -> bool {
        std::ptr::eq(lhs.inner.as_ptr(), rhs.inner.as_ptr())
    }

    pub fn get_inner(&self) -> T {
        (*self.inner.borrow()).clone()
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

    pub fn with_ref_mut<F, O>(&mut self, mut f: F) -> O
    where
        F: FnMut(&mut T) -> O,
    {
        f(&mut self.inner.borrow_mut())
    }
}

impl<T: Clone> From<T> for Reference<T> {
    fn from(v: T) -> Self {
        Self::new(v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::value::Value;

    #[test]
    pub fn test_ref_value() {
        let v1 = Reference::new(Value::Bool(true));
        let v2 = Reference::new(Value::Bool(true));
        let v3 = v1.clone();

        assert!(
            !Reference::ref_eq(&v1, &v2),
            "different references are not eq"
        );
        assert!(
            Reference::ref_eq(&v1, &v1),
            "references must be eq to themselves"
        );
        assert!(
            Reference::ref_eq(&v1, &v3),
            "cloned references are eq to their clone"
        );
    }
}
