#[cfg(test)]
pub mod arbitrary;
pub mod lambda;
pub mod list;
use crate::compiler::frontend::parser::sexp::datum::{Datum, Sexp};
use crate::compiler::utils::string_table;
use crate::compiler::utils::string_table::StringTable;
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
    InternedString(string_table::Interned),
    UninternedString(std::string::String),
    ProperList(list::List),
    Procedure(lambda::Procedure),
    Unspecified,
}

#[repr(transparent)]
#[derive(Clone, PartialEq, Hash, Eq)]
pub struct Symbol(string_table::Interned);

impl Symbol {
    pub fn as_str<'a>(&'a self) -> &'a str {
        self.0.as_str()
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        formatter.write_fmt(format_args!("sym#({})", self.as_str()))
    }
}

#[repr(transparent)]
#[derive(Clone, PartialEq)]
pub struct InternedString(string_table::Interned);

impl InternedString {
    pub fn as_str<'a>(&'a self) -> &'a str {
        self.0.as_str()
    }
}

impl std::fmt::Debug for InternedString {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        formatter.write_fmt(format_args!(
            "Interned({} @ {:p})",
            self.as_str(),
            self.as_str()
        ))
    }
}

#[derive(Debug, Clone)]
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

    pub fn symbol<T: Into<std::string::String>>(&mut self, v: T) -> Value {
        let k = self.symbols.get_or_intern(v.into());
        Value::Symbol(Symbol(k))
    }

    pub fn interned_string<T: Into<std::string::String>>(&mut self, v: T) -> Value {
        let k = self.strings.get_or_intern(v.into());
        Value::InternedString(k)
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

    pub fn procedure(&mut self, v: lambda::Procedure) -> Value {
        Value::Procedure(v)
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

    pub fn absorb(&mut self, other: Self) {
        self.strings.absorb(other.strings);
        self.symbols.absorb(other.symbols);
    }

    pub fn interned_symbols(&self) -> Vec<Symbol> {
        self.symbols
            .interned_vec()
            .iter()
            .map(|e| Symbol(e.clone()))
            .collect()
    }

    pub fn interned_strings(&self) -> Vec<InternedString> {
        self.strings
            .interned_vec()
            .iter()
            .map(|e| InternedString(e.clone()))
            .collect()
    }
}
