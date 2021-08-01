use crate::vm::value::procedure;
use std::hash::{Hash, Hasher};
use thiserror::Error;
pub mod environment;

// Symbols are a bit special since they might have different origins, which we want to track
// This is done during macro expansion, so compile, time.
// Symbols are the thing that hygiene deals with, so that's why we want to be able to track
// where they came from.

#[derive(Eq, Clone, Debug, PartialEq, Hash)]
pub enum Tag {
    Code,
    Forged,
    Unforgable,
    Renamed(u64),
}

#[derive(Debug, Clone)]
pub struct Symbol {
    tag: Tag,
    value: String,
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        match (&self.tag, &other.tag) {
            (Tag::Unforgable, Tag::Unforgable) => self.value == other.value,
            (Tag::Unforgable, _) => false,
            (_, Tag::Unforgable) => false,
            (Tag::Renamed(u1), Tag::Renamed(u2)) => self.value == other.value && u1 == u2,
            (Tag::Renamed(_), _) => false,
            (_, Tag::Renamed(_)) => false,
            _ => self.value == other.value,
        }
    }
}
impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.tag {
            Tag::Unforgable => {
                self.tag.hash(state);
                self.value.hash(state);
            }
            Tag::Renamed(_) => {
                self.tag.hash(state);
                self.value.hash(state);
            }
            Tag::Code => self.value.hash(state),
            Tag::Forged => self.value.hash(state),
        }
    }
}

impl Symbol {
    pub fn as_str<'a>(&'a self) -> &'a str {
        self.string().as_str()
    }

    pub fn string<'a>(&'a self) -> &'a String {
        &self.value
    }

    pub fn renamed<T: Into<String>>(s: T, c: u64) -> Self {
        Symbol {
            value: s.into(),
            tag: Tag::Renamed(c),
        }
    }

    pub fn unforgeable<T: Into<String>>(s: T) -> Self {
        Symbol {
            value: s.into(),
            tag: Tag::Unforgable,
        }
    }

    pub fn from_code<T: Into<String>>(v: T) -> Self {
        Symbol {
            value: v.into(),
            tag: Tag::Code,
        }
    }

    // a symbol forged at runtime (or compile time)
    pub fn forged<T: Into<String>>(v: T) -> Self {
        Symbol {
            value: v.into(),
            tag: Tag::Forged,
        }
    }
}

impl From<Symbol> for String {
    fn from(s: Symbol) -> String {
        s.string().clone()
    }
}

impl From<&Symbol> for String {
    fn from(s: &Symbol) -> String {
        s.string().clone()
    }
}

#[derive(Debug, Clone)]
pub enum Transformer {
    ExplicitRenaming(procedure::Procedure),
}

#[derive(Error, Debug, Clone)]
pub enum Error {}
