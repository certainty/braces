#[cfg(test)]
pub mod arbitrary;
pub mod list;
use crate::compiler::frontend::parser::sexp::datum::{Datum, Sexp};
use crate::compiler::utils::string_table;
use crate::compiler::utils::string_table::StringTable;
use lasso::Key;
use std::convert::Into;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Not an interned value")]
    NotInterned,
}

/// Runtime representation of values
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Symbol(Symbol),
    Char(char),
    InternedString(InternedString),
    UninternedString(std::string::String),
    ProperList(list::List),
    Unspecified,
}

#[repr(transparent)]
#[derive(Clone, PartialEq, Hash, Eq)]
pub struct Symbol(string_table::Key);

impl std::fmt::Debug for Symbol {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        formatter.write_fmt(format_args!("sym#{}", self.0.into_usize()))
    }
}

#[repr(transparent)]
#[derive(Clone, PartialEq)]
pub struct InternedString(string_table::Key);

impl std::fmt::Debug for InternedString {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        formatter.write_fmt(format_args!("str#{}", self.0.into_usize()))
    }
}

#[derive(Debug)]
pub struct Factory {
    strings: StringTable,
    symbols: StringTable,
    true_value: Value,
    false_value: Value,
    nil_value: Value,
    unspecified: Value,
}

impl Default for Factory {
    fn default() -> Factory {
        Factory {
            strings: StringTable::default(),
            symbols: StringTable::default(),
            true_value: Value::Bool(true),
            false_value: Value::Bool(false),
            nil_value: Value::ProperList(list::List::Nil),
            unspecified: Value::Unspecified,
        }
    }
}

impl Factory {
    pub fn bool_true<'a>(&'a self) -> &'a Value {
        &self.true_value
    }

    pub fn bool_false<'a>(&'a self) -> &'a Value {
        &self.false_value
    }

    pub fn nil<'a>(&'a self) -> &'a Value {
        &self.nil_value
    }

    pub fn unspecified<'a>(&'a self) -> &'a Value {
        &self.unspecified
    }

    pub fn character(&self, c: char) -> Value {
        Value::Char(c)
    }

    pub fn symbol<T: AsRef<str>>(&mut self, v: T) -> Value {
        let k = self.symbols.get_or_intern(v);
        Value::Symbol(Symbol(k))
    }

    pub fn interned_string<T: AsRef<str>>(&mut self, v: T) -> Value {
        let k = self.strings.get_or_intern(v);
        Value::InternedString(InternedString(k))
    }

    pub fn string<T: Into<std::string::String>>(&mut self, v: T) -> Value {
        Value::UninternedString(v.into())
    }

    pub fn proper_list(&self, vals: Vec<Value>) -> Value {
        if vals.is_empty() {
            Value::ProperList(list::List::Nil)
        } else {
            let ls: list::List = vals.into();
            Value::ProperList(ls)
        }
    }

    pub fn unintern<'a>(&'a self, value: &Value) -> std::result::Result<&'a str, Error> {
        match value {
            Value::InternedString(k) => Ok(self.strings.get(&k.0)),
            Value::Symbol(k) => Ok(self.symbols.get(&k.0)),
            _ => Err(Error::NotInterned),
        }
    }

    pub fn from_datum(&mut self, d: &Datum) -> Value {
        match &d.sexp {
            Sexp::Bool(true) => self.bool_true().clone(),
            Sexp::Bool(false) => self.bool_false().clone(),
            Sexp::Symbol(s) => self.symbol(s),
            Sexp::String(s) => self.string(s),
            Sexp::List(ls) => {
                let elements = ls.iter().map(|e| self.from_datum(e)).collect();
                self.proper_list(elements)
            }
            Sexp::Char(c) => self.character(*c),
            _ => todo!(),
        }
    }

    pub fn absorb(&mut self, other: &Self) {
        if self as *const _ == other as *const _ {
            return ();
        } else {
            self.strings.absorb(&other.strings);
            self.symbols.absorb(&other.symbols);
        }
    }

    pub fn symbol_set(&self) -> Vec<String> {
        self.symbols.string_set()
    }

    pub fn string_set(&self) -> Vec<String> {
        self.strings.string_set()
    }
}
