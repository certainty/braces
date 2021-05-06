use super::Value;
use crate::vm::scheme::equality::SchemeEqual;
use im::Vector;
use std::convert::From;
use std::iter::{FromIterator, IntoIterator};

#[derive(Debug, PartialEq, Clone)]
pub enum List {
    Nil,
    Cons(Vector<Value>),
}

impl List {
    pub fn nil() -> List {
        Self::Nil
    }

    pub fn new() -> Self {
        List::Cons(Vector::new())
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
    pub fn head(&self) -> Option<&Value> {
        match self {
            List::Nil => None,
            List::Cons(e) => e.head(),
        }
    }

    #[inline]
    pub fn first(&self) -> Option<&Value> {
        self.head()
    }

    #[inline]
    pub fn second(&self) -> Option<&Value> {
        match self {
            List::Nil => None,
            List::Cons(e) => e.get(1),
        }
    }

    #[inline]
    pub fn third(&self) -> Option<&Value> {
        match self {
            List::Nil => None,
            List::Cons(e) => e.get(2),
        }
    }

    #[inline]
    pub fn fourth(&self) -> Option<&Value> {
        match self {
            List::Nil => None,
            List::Cons(e) => e.get(3),
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
    inner: Option<im::vector::Iter<'a, Value>>,
}

impl<'a> Iter<'a> {
    fn empty() -> Iter<'a> {
        Iter {
            is_empty: true,
            inner: None,
        }
    }

    fn new(inner: im::vector::Iter<'a, Value>) -> Iter<'a> {
        Iter {
            is_empty: false,
            inner: Some(inner),
        }
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a Value;

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
        let ls: im::vector::Vector<Value> = std::iter::FromIterator::from_iter(iter);
        if ls.is_empty() {
            List::Nil
        } else {
            List::Cons(ls)
        }
    }
}

impl IntoIterator for List {
    type Item = Value;
    type IntoIter = im::vector::ConsumingIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            List::Nil => im::vector::Vector::new().into_iter(),
            List::Cons(e) => e.into_iter(),
        }
    }
}

impl From<Vec<Value>> for List {
    fn from(elements: Vec<Value>) -> Self {
        let ls: im::vector::Vector<Value> = elements.into_iter().collect();

        if ls.is_empty() {
            List::Nil
        } else {
            List::Cons(ls)
        }
    }
}

impl SchemeEqual<List> for List {
    fn is_eq(&self, other: &List) -> bool {
        if self.len() != other.len() {
            return false;
        } else {
            self.iter().zip(other.iter()).all(|(a, b)| a.is_eq(b))
        }
    }

    fn is_eqv(&self, other: &List) -> bool {
        if self.len() != other.len() {
            return false;
        } else {
            self.iter().zip(other.iter()).all(|(a, b)| a.is_eqv(b))
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
