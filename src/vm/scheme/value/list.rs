use super::Value;
use im::Vector;
use std::convert::From;
use std::iter::{FromIterator, IntoIterator};

lazy_static! {
    pub static ref NIL: List = List::Nil;
    pub static ref EMPTY: im::vector::Vector<Value> = Vector::new();
}

#[derive(Debug, PartialEq, Clone)]
pub enum List {
    Nil,
    Cons(Vector<Value>),
}

impl List {
    pub fn nil() -> &'static List {
        &NIL
    }

    pub fn new() -> Self {
        List::Cons(Vector::new())
    }

    pub fn is_null(&self) -> bool {
        &self == &Self::nil()
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

    pub fn iter(&self) -> im::vector::Iter<Value> {
        match self {
            List::Nil => EMPTY.iter(),
            List::Cons(e) => e.iter(),
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
