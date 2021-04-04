use super::Value;
use im::Vector;
use std::convert::From;
use std::iter::{FromIterator, IntoIterator};

lazy_static! {
    pub static ref NIL: List = List(Vector::new());
}

#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct List(Vector<Value>);

impl List {
    pub fn nil() -> &'static List {
        &NIL
    }

    pub fn new() -> Self {
        List(Vector::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn head(&self) -> Option<&Value> {
        self.0.head()
    }

    #[inline]
    pub fn first(&self) -> Option<&Value> {
        self.head()
    }

    pub fn second(&self) -> Option<&Value> {
        self.0.get(1)
    }

    pub fn iter(&self) -> im::vector::Iter<Value> {
        self.0.iter()
    }
}

impl FromIterator<Value> for List {
    fn from_iter<I: IntoIterator<Item = Value>>(iter: I) -> Self {
        List(std::iter::FromIterator::from_iter(iter))
    }
}

impl IntoIterator for List {
    type Item = Value;
    type IntoIter = im::vector::ConsumingIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<Vec<Value>> for List {
    fn from(elements: Vec<Value>) -> Self {
        let ls: List = elements.into_iter().collect();
        ls
    }
}
