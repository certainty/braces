#[cfg(test)]
pub mod arbitrary;
pub mod list;
use crate::compiler::frontend::parser::sexp::datum::{Datum, Sexp};
use std::convert::Into;

/// Runtime representation of values

#[derive(Debug, Clone, PartialEq)]
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
        if vals.is_empty() {
            Value::ProperList(list::List::Nil)
        } else {
            let ls: list::List = vals.into();
            Value::ProperList(ls)
        }
    }

    pub fn nil() -> Value {
        Value::ProperList(list::List::Nil)
    }
}

impl From<Datum> for Value {
    fn from(d: Datum) -> Self {
        match d.sexp {
            Sexp::Bool(v) => Self::boolean(v),
            Sexp::Symbol(s) => Self::symbol(s),
            Sexp::String(s) => Self::string(s),
            Sexp::List(ls) => Self::proper_list(ls.iter().map(|e| Value::from(e)).collect()),
            Sexp::Char(c) => Self::character(c),
            _ => todo!(),
        }
    }
}

impl From<&Datum> for Value {
    fn from(d: &Datum) -> Self {
        match &d.sexp {
            Sexp::Bool(v) => Self::boolean(*v),
            Sexp::Symbol(s) => Self::symbol(s),
            Sexp::String(s) => Self::string(s),
            Sexp::List(ls) => Self::proper_list(ls.iter().map(|e| Value::from(e)).collect()),
            Sexp::Char(c) => Self::character(*c),
            _ => todo!(),
        }
    }
}
