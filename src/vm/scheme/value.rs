#[cfg(test)]
pub mod arbitrary;
pub mod list;
use std::convert::Into;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Bool(bool),
    Symbol(String),
    Char(char),
    String(String),
    ProperList(list::List),
    Unspecified,
}

impl Value {
    pub fn unspecified() -> Value {
        Self::Unspecified
    }

    pub fn boolean(val: bool) -> Value {
        Self::Bool(val)
    }

    pub fn symbol(name: impl Into<String>) -> Value {
        Self::Symbol(name.into())
    }

    pub fn string(s: impl Into<String>) -> Value {
        Self::String(s.into())
    }

    pub fn character(c: char) -> Value {
        Self::Char(c)
    }

    pub fn proper_list(vals: Vec<Value>) -> Value {
        let ls: list::List = vals.into();
        Value::ProperList(ls)
    }
}
