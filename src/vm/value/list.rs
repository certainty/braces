use super::equality::SchemeEqual;
use super::Value;
use crate::vm::value::access::Reference;
use im_rc::Vector;
use std::convert::From;
use std::iter::{FromIterator, IntoIterator};
use std::rc::Rc;

type ElementRepr = Reference<Value>;

#[derive(Debug, PartialEq, Clone)]
pub enum List {
    Nil,
    Cons(Rc<Vector<ElementRepr>>),
}

impl List {
    pub fn nil() -> List {
        Self::Nil
    }

    pub fn singleton(v: Value) -> Self {
        List::from(Vector::from(vec![Reference::from(v)]))
    }

    pub fn copied(&self) -> Self {
        match self {
            Self::Nil => self.clone(),
            Self::Cons(inner) => {
                let elements = inner.iter().map(|e| e.copied()).collect::<Vector<_>>();
                List::from(elements)
            }
        }
    }

    // create a fresh list by concateneting the two supplied lists
    pub fn append(lhs: &List, rhs: &List) -> List {
        match (lhs, rhs) {
            (List::Nil, rhs) => rhs.copied(),
            (lhs, List::Nil) => lhs.copied(),
            (List::Cons(lhs), List::Cons(rhs)) => {
                let mut lhs_copy = lhs.iter().map(|e| e.copied()).collect::<Vector<_>>();
                let rhs_copy = rhs.iter().map(|e| e.copied());

                lhs_copy.extend(rhs_copy);

                List::Cons(Rc::from(lhs_copy))
            }
        }
    }

    pub fn cons(&self, v: Value) -> List {
        match self {
            List::Nil => Self::singleton(v),
            List::Cons(elts) => {
                let mut new_elts = elts.iter().cloned().collect::<Vector<_>>();
                // if it's a reference itself we first copy it value out and then wrap it
                // into a references again
                new_elts.push_front(Reference::from(v));
                List::from(new_elts)
            }
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            Self::Nil => true,
            _ => false,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        match self {
            List::Nil => 0,
            List::Cons(e) => e.len(),
        }
    }

    #[inline]
    pub fn head(&self) -> Option<&Reference<Value>> {
        self.first()
    }

    #[inline]
    pub fn first(&self) -> Option<&Reference<Value>> {
        self.at(0)
    }

    #[inline]
    pub fn second(&self) -> Option<&Reference<Value>> {
        self.at(1)
    }

    #[inline]
    pub fn third(&self) -> Option<&Reference<Value>> {
        self.at(2)
    }

    #[inline]
    pub fn fourth(&self) -> Option<&Reference<Value>> {
        self.at(3)
    }

    pub fn cdr(&self) -> Option<List> {
        match self {
            Self::Nil => None,
            Self::Cons(elements) => Some(Self::from(elements.skip(1))),
        }
    }

    pub fn at(&self, i: usize) -> Option<&Reference<Value>> {
        match self {
            List::Nil => None,
            List::Cons(elts) => elts.get(i),
        }
    }

    pub fn iter(&self) -> Iter {
        match self {
            List::Nil => Iter::empty(),
            List::Cons(e) => Iter::new(e.iter()),
        }
    }
}

pub struct Iter<'a> {
    is_empty: bool,
    inner: Option<im_rc::vector::Iter<'a, Reference<Value>>>,
}

impl<'a> Iter<'a> {
    fn empty() -> Iter<'a> {
        Iter {
            is_empty: true,
            inner: None,
        }
    }

    fn new(inner: im_rc::vector::Iter<'a, Reference<Value>>) -> Iter<'a> {
        Iter {
            is_empty: false,
            inner: Some(inner),
        }
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a Reference<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_empty {
            None
        } else {
            match &mut self.inner {
                Some(inner) => inner.next(),
                _ => None,
            }
        }
    }
}

impl FromIterator<Value> for List {
    fn from_iter<I: IntoIterator<Item = Value>>(iter: I) -> Self {
        let ls: im_rc::vector::Vector<Reference<Value>> =
            iter.into_iter().map(Reference::from).collect();
        List::from(ls)
    }
}

impl From<Vec<Value>> for List {
    fn from(elements: Vec<Value>) -> Self {
        let ls: im_rc::vector::Vector<Reference<Value>> =
            elements.into_iter().map(Reference::from).collect();

        List::from(ls)
    }
}

impl From<Vector<ElementRepr>> for List {
    fn from(elements: Vector<ElementRepr>) -> Self {
        if elements.is_empty() {
            List::Nil
        } else {
            List::Cons(Rc::from(elements))
        }
    }
}

// TODO: fix equality. See r7rs 6.1
impl SchemeEqual<List> for List {
    fn is_eq(&self, other: &List) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Cons(lhs), Self::Cons(rhs)) => Rc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }

    fn is_eqv(&self, other: &List) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Cons(lhs), Self::Cons(rhs)) => Rc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }

    fn is_equal(&self, other: &List) -> bool {
        if self.len() != other.len() {
            return false;
        } else {
            self.iter().zip(other.iter()).all(|(a, b)| a.is_equal(b))
        }
    }
}
