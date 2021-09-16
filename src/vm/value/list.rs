use super::equality::SchemeEqual;
use super::Value;
use im_rc::Vector;
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

    pub fn cons(&self, v: Value) -> List {
        match self {
            List::Nil => List::Cons(Vector::from(vec![v.to_reference()])),
            List::Cons(elts) => {
                let mut new_elts = elts.clone();
                // if it's a reference itself we first copy it value out and then wrap it
                // into a references again
                new_elts.push_front(v.to_reference());
                List::Cons(new_elts)
            }
        }
    }

    pub fn append(&self, other: &List) -> List {
        match (self, other) {
            (List::Nil, _) => other.clone(),
            (_, List::Nil) => self.clone(),
            (List::Cons(lhs), List::Cons(rhs)) => {
                let mut elts = lhs.clone();
                elts.append(rhs.clone());
                List::Cons(elts)
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
    pub fn head(&self) -> Option<&Value> {
        self.first()
    }

    #[inline]
    pub fn first(&self) -> Option<&Value> {
        self.at(0)
    }

    #[inline]
    pub fn second(&self) -> Option<&Value> {
        self.at(1)
    }

    #[inline]
    pub fn third(&self) -> Option<&Value> {
        self.at(2)
    }

    #[inline]
    pub fn fourth(&self) -> Option<&Value> {
        self.at(3)
    }

    pub fn at(&self, i: usize) -> Option<&Value> {
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
    inner: Option<im_rc::vector::Iter<'a, Value>>,
}

impl<'a> Iter<'a> {
    fn empty() -> Iter<'a> {
        Iter {
            is_empty: true,
            inner: None,
        }
    }

    fn new(inner: im_rc::vector::Iter<'a, Value>) -> Iter<'a> {
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
        let ls: im_rc::vector::Vector<Value> = std::iter::FromIterator::from_iter(iter);
        if ls.is_empty() {
            List::Nil
        } else {
            List::Cons(ls)
        }
    }
}

impl From<Vec<Value>> for List {
    fn from(elements: Vec<Value>) -> Self {
        let ls: im_rc::vector::Vector<Value> =
            elements.into_iter().map(|e| e.to_reference()).collect();

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
