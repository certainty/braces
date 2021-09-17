use crate::vm::value::equality::SchemeEqual;
use std::cell::{Ref, RefCell};
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

/// A Reference is a set-able place. The scheme report specifies that
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

#[repr(transparent)]
struct RefGuard<'a, T> {
    guard: Ref<'a, T>,
}

impl<'b, T> Deref for RefGuard<'b, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard.deref()
    }
}

impl<T: Clone> Reference<T> {
    pub fn new(v: T) -> Self {
        Self {
            inner: Rc::new(RefCell::new(v)),
        }
    }

    pub fn to_owned(&self) -> T {
        self.inner.borrow().clone()
    }

    /// ref_eq is true iff the value that the reference points to is the same
    pub fn ref_eq(lhs: &Self, rhs: &Self) -> bool {
        std::ptr::eq(lhs.inner.as_ptr(), rhs.inner.as_ptr())
    }

    pub fn get_inner_ref(&self) -> impl Deref<Target = T> + '_ {
        RefGuard {
            guard: self.inner.borrow(),
        }
    }

    pub fn set(&mut self, v: T) {
        self.inner.replace(v);
    }
}

impl<T: Clone> From<T> for Reference<T> {
    fn from(v: T) -> Self {
        Self::new(v)
    }
}

impl<T: Display + Clone> Display for Reference<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.get_inner_ref().fmt(f)
    }
}

impl<T: SchemeEqual<T> + Clone> SchemeEqual<Reference<T>> for Reference<T> {
    fn is_eq(&self, other: &Reference<T>) -> bool {
        Reference::<T>::ref_eq(self, other)
    }

    fn is_eqv(&self, other: &Reference<T>) -> bool {
        Reference::<T>::ref_eq(self, other)
    }

    fn is_equal(&self, other: &Reference<T>) -> bool {
        self.get_inner_ref().is_equal(&other.get_inner_ref())
    }
}

/// Explicit modeling of access patterns
///
/// This is used to deal with values which can be places (references) or immutable values.
/// Variables will always be accessed `ByRef`. The same is true for cons cells, i.e. list elements.
/// All other values are usually access `ByVal`.
///
/// We could have modeled in the `Value` type directly, by adding a `Ref` variant
/// or something like that. However this hid important details, that are required to understand
/// the code of the VM instance. This type makes access so implicit, that the reader should
/// always see what she's dealing with and follow the flow more easily.
#[derive(Clone, Debug, PartialEq)]
pub enum Access<T> {
    ByVal(T),
    ByRef(Reference<T>),
}

impl<T: Clone> Access<T> {
    pub fn to_owned(&self) -> T {
        match self {
            Self::ByRef(r) => r.to_owned(),
            Self::ByVal(v) => v.clone(),
        }
    }

    /// consumes `self` and returns inner
    pub fn into_inner(self) -> T {
        match self {
            Self::ByVal(v) => v,
            Self::ByRef(r) => r.get_inner_ref().clone(),
        }
    }

    #[inline]
    pub fn is_by_val(&self) -> bool {
        !self.is_by_ref()
    }

    #[inline]
    pub fn is_mutable(&self) -> bool {
        self.is_by_ref()
    }

    pub fn is_by_ref(&self) -> bool {
        match self {
            Self::ByRef(_) => true,
            _ => false,
        }
    }
}

impl<T: Clone> From<Reference<T>> for Access<T> {
    fn from(r: Reference<T>) -> Self {
        Self::ByRef(r)
    }
}

impl<T: Clone> From<&Reference<T>> for Access<T> {
    fn from(r: &Reference<T>) -> Self {
        Self::ByRef(r.clone())
    }
}

impl<T> From<T> for Access<T> {
    fn from(v: T) -> Self {
        Self::ByVal(v)
    }
}

impl<T: Clone> From<&T> for Access<T> {
    fn from(v: &T) -> Self {
        Self::ByVal(v.clone())
    }
}

impl<T: std::fmt::Display + Clone> std::fmt::Display for Access<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ByVal(v) => v.fmt(f),
            Self::ByRef(r) => f.write_fmt(format_args!("&{}", *r.get_inner_ref())),
        }
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

    #[test]
    pub fn test_ref_deref() {
        let v = Reference::from(true);

        let result = match *v.get_inner_ref() {
            true => true,
            false => false,
        };

        assert!(result)
    }

    #[test]
    pub fn smoke_test_access() {
        let a = Access::from(1);

        assert!(a.is_by_val());
        assert!(!a.is_by_ref());
        assert!(!a.is_mutable());
        assert_eq!(format!("{}", a), "1");

        let b: Access<i32> = Access::from(Reference::from(1));
        assert!(!b.is_by_val());
        assert!(b.is_by_ref());
        assert!(b.is_mutable());
        assert_eq!(format!("{}", b), "&1")
    }
}
