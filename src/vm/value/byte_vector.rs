use crate::vm::value::access::Reference;
use crate::vm::value::equality::SchemeEqual;
use std::rc::Rc;

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq)]
pub struct ByteVector(Rc<Vec<Reference<u8>>>);

impl ByteVector {
    pub fn new() -> Self {
        Self(Rc::from(Vec::new()))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> ByteVecIter {
        ByteVecIter {
            inner: self.0.iter(),
        }
    }
}

pub struct ByteVecIter<'a> {
    inner: std::slice::Iter<'a, Reference<u8>>,
}

impl<'a> Iterator for ByteVecIter<'a> {
    type Item = &'a Reference<u8>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl From<Vec<Reference<u8>>> for ByteVector {
    fn from(elements: Vec<Reference<u8>>) -> Self {
        Self(Rc::from(elements))
    }
}

impl From<Vec<u8>> for ByteVector {
    fn from(elements: Vec<u8>) -> Self {
        Self(Rc::from(
            elements
                .into_iter()
                .map(Reference::from)
                .collect::<Vec<_>>(),
        ))
    }
}

impl SchemeEqual<ByteVector> for ByteVector {
    fn is_eq(&self, other: &ByteVector) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn is_eqv(&self, other: &ByteVector) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn is_equal(&self, other: &ByteVector) -> bool {
        if self.len() != other.len() {
            return false;
        } else {
            self.iter().zip(other.iter()).all(|(a, b)| a == b)
        }
    }
}
