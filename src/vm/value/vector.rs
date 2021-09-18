use super::Value;
use crate::vm::value::access::Reference;
use crate::vm::value::equality::SchemeEqual;
use std::rc::Rc;

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq)]
pub struct Vector(Rc<Vec<Reference<Value>>>);

impl Vector {
    pub fn append(lhs: &Vector, rhs: &Vector) -> Vector {
        let mut new_vec = Vec::with_capacity(lhs.len() + rhs.len());
        let lhs_copy = lhs.0.iter().map(|e| e.copied());
        let rhs_copy = rhs.0.iter().map(|e| e.copied());

        new_vec.extend(lhs_copy);
        new_vec.extend(rhs_copy);

        Vector::from(new_vec)
    }

    pub fn cons(&self, v: Value) -> Self {
        let mut new_vec = Vec::with_capacity(self.0.len() + 1);
        let tail = self.0.iter().cloned();
        new_vec.push(Reference::from(v));
        new_vec.extend(tail);
        Vector::from(new_vec)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn at(&self, idx: usize) -> Option<Reference<Value>> {
        self.0.get(idx).cloned()
    }

    pub fn iter(&self) -> VecIter {
        VecIter {
            inner: self.0.iter(),
        }
    }
}

impl From<Vec<Reference<Value>>> for Vector {
    fn from(elements: Vec<Reference<Value>>) -> Self {
        Vector(Rc::from(elements))
    }
}

impl SchemeEqual<Vector> for Vector {
    fn is_eq(&self, other: &Vector) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn is_eqv(&self, other: &Vector) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn is_equal(&self, other: &Vector) -> bool {
        if self.len() != other.len() {
            return false;
        } else {
            self.iter().zip(other.iter()).all(|(a, b)| a.is_equal(b))
        }
    }
}

pub struct VecIter<'a> {
    inner: std::slice::Iter<'a, Reference<Value>>,
}

impl<'a> Iterator for VecIter<'a> {
    type Item = &'a Reference<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}
