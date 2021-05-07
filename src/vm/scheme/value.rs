#[cfg(test)]
pub mod arbitrary;
pub mod closure;
pub mod error;
pub mod foreign;
pub mod lambda;
pub mod list;
pub mod procedure;
pub mod string;
pub mod symbol;
use self::{string::InternedString, symbol::Symbol};
use crate::compiler::frontend::parser::sexp::datum::{Datum, Sexp};
use crate::compiler::utils::string_table::StringTable;
use std::convert::Into;
use std::rc::Rc;
use thiserror::Error;

use super::equality::SchemeEqual;

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
    Procedure(Rc<procedure::Procedure>),
    ForeignProcedure(Rc<foreign::Procedure>),
    Unspecified,
    // these are not actually scheme values but rather runtime values that exist during execution
    Closure(closure::Closure),
    UpValue(Rc<Value>),
}

impl Value {
    pub fn is_false(&self) -> bool {
        match self {
            Self::Bool(false) => true,
            _ => false,
        }
    }
}

impl SchemeEqual<Value> for Value {
    fn is_eqv(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
            (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs.is_eqv(rhs),
            (Value::InternedString(lhs), Value::InternedString(rhs)) => lhs.is_eqv(rhs),
            (Value::UninternedString(_), Value::InternedString(_)) => false,
            (Value::UninternedString(_), Value::UninternedString(_)) => false,
            (Value::ProperList(lhs), Value::ProperList(rhs)) => lhs.is_eqv(rhs),
            (Value::Procedure(lhs), Value::Procedure(rhs)) => lhs.is_eqv(rhs),
            (Value::Unspecified, Value::Unspecified) => false,
            _ => false,
        }
    }

    fn is_eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
            (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs.is_eq(rhs),
            (Value::InternedString(lhs), Value::InternedString(rhs)) => lhs.is_eq(rhs),
            (Value::UninternedString(_), Value::InternedString(_)) => false,
            (Value::UninternedString(_), Value::UninternedString(_)) => false,
            (Value::ProperList(lhs), Value::ProperList(rhs)) => lhs.is_eq(rhs),
            (Value::Procedure(lhs), Value::Procedure(rhs)) => lhs.is_eq(rhs),
            (Value::ForeignProcedure(lhs), Value::ForeignProcedure(rhs)) => lhs.is_eq(rhs),
            (Value::Unspecified, Value::Unspecified) => false,
            _ => false,
        }
    }

    fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
            (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs.is_equal(rhs),
            (Value::InternedString(lhs), Value::InternedString(rhs)) => lhs.is_equal(rhs),
            (Value::UninternedString(lhs), Value::InternedString(rhs)) => {
                lhs.as_str() == rhs.as_str()
            }
            (Value::UninternedString(lhs), Value::UninternedString(rhs)) => lhs == rhs,
            (Value::ProperList(lhs), Value::ProperList(rhs)) => lhs.is_equal(rhs),
            (Value::Procedure(lhs), Value::Procedure(rhs)) => lhs.is_equal(rhs),
            (Value::Unspecified, Value::Unspecified) => true,
            _ => false,
        }
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
    pub fn bool_true(&self) -> Value {
        self.true_value.clone()
    }

    pub fn bool_false(&self) -> Value {
        self.false_value.clone()
    }

    pub fn nil(&self) -> Value {
        self.nil_value.clone()
    }

    pub fn unspecified(&self) -> Value {
        self.unspecified.clone()
    }

    pub fn character(&self, c: char) -> Value {
        Value::Char(c)
    }

    pub fn sym<T: Into<std::string::String>>(&mut self, v: T) -> Symbol {
        let k = self.symbols.get_or_intern(v.into());
        Symbol(k)
    }

    pub fn symbol<T: Into<std::string::String>>(&mut self, v: T) -> Value {
        Value::Symbol(self.sym(v))
    }

    pub fn interned_string<T: Into<std::string::String>>(&mut self, v: T) -> Value {
        let k = self.strings.get_or_intern(v.into());
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

    pub fn procedure(&mut self, v: procedure::Procedure) -> Value {
        Value::Procedure(Rc::new(v))
    }

    pub fn foreign_procedure(&mut self, v: foreign::Procedure) -> Value {
        Value::ForeignProcedure(Rc::new(v))
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
            .cloned()
            .map(Symbol)
            .collect()
    }

    pub fn interned_strings(&self) -> Vec<InternedString> {
        self.strings
            .interned_vec()
            .iter()
            .cloned()
            .map(InternedString)
            .collect()
    }
}
