use crate::vm::value::procedure;
use std::hash::{Hash, Hasher};
use thiserror::Error;

// Symbols are a bit special since they might have different origins, which we want to track
// This is done during macro expansion, so compile, time.
// Symbols are the thing that hygiene deals with, so that's why we want to be able to track
// where they came from.

#[derive(Eq, Clone, Debug, PartialEq, Hash)]
pub enum Tag {
    Code,
    Forged,
    Unforgeable,
    Renamed(u64),
}

#[derive(Debug, Clone)]
pub struct Identifier {
    tag: Tag,
    value: String,
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Identifier) -> bool {
        match (&self.tag, &other.tag) {
            (Tag::Unforgeable, Tag::Unforgeable) => self.value == other.value,
            (Tag::Unforgeable, _) => false,
            (_, Tag::Unforgeable) => false,
            (Tag::Renamed(u1), Tag::Renamed(u2)) => self.value == other.value && u1 == u2,
            (Tag::Renamed(_), _) => false,
            (_, Tag::Renamed(_)) => false,
            _ => self.value == other.value,
        }
    }
}
impl Eq for Identifier {}

impl Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.tag {
            Tag::Unforgeable => {
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

impl Identifier {
    pub fn as_str(&self) -> &str {
        self.string().as_str()
    }

    pub fn string(&self) -> &String {
        &self.value
    }

    pub fn renamed<T: Into<String>>(s: T, c: u64) -> Self {
        Self {
            value: s.into(),
            tag: Tag::Renamed(c),
        }
    }

    pub fn unforgeable<T: Into<String>>(s: T) -> Self {
        Self {
            value: s.into(),
            tag: Tag::Unforgeable,
        }
    }

    pub fn from_code<T: Into<String>>(v: T) -> Self {
        Self {
            value: v.into(),
            tag: Tag::Code,
        }
    }

    // a symbol forged at runtime (or compile time)
    pub fn forged<T: Into<String>>(v: T) -> Self {
        Self {
            value: v.into(),
            tag: Tag::Forged,
        }
    }
}

impl From<Identifier> for String {
    fn from(s: Identifier) -> String {
        s.string().clone()
    }
}

impl From<&Identifier> for String {
    fn from(s: &Identifier) -> String {
        s.string().clone()
    }
}

impl From<String> for Identifier {
    fn from(s: String) -> Identifier {
        Identifier::forged(s)
    }
}

#[derive(Debug, Clone)]
pub enum Transformer {
    ExplicitRenaming(procedure::Procedure),
}

#[derive(Error, Debug, Clone)]
pub enum Error {}
