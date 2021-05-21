#[cfg(test)]
pub mod arbitrary;
pub mod closure;
pub mod equality;
pub mod error;
pub mod list;
pub mod number;
pub mod procedure;
pub mod string;
pub mod symbol;
use self::{string::InternedString, symbol::Symbol};
use crate::compiler::frontend::parser::sexp::datum::{self, Datum, Sexp};
use crate::compiler::utils::string_table::StringTable;
use num::BigInt;
use std::cell::Ref;
use std::cell::RefCell;
use std::convert::Into;
use std::rc::Rc;
use thiserror::Error;

use equality::SchemeEqual;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Not an interned value")]
    NotInterned,
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct RefValue {
    inner: Rc<RefCell<Value>>,
}

impl RefValue {
    pub fn new(v: Value) -> Self {
        RefValue {
            inner: Rc::new(RefCell::new(v)),
        }
    }

    pub fn to_value(&self) -> Value {
        self.inner.borrow().clone()
    }

    pub fn as_ref<'a>(&'a self) -> Ref<Value> {
        self.inner.borrow()
    }

    pub fn set(&mut self, v: Value) {
        self.inner.replace(v);
    }
}

impl PartialEq for RefValue {
    fn eq(&self, other: &RefValue) -> bool {
        self.inner.as_ptr() == other.inner.as_ptr()
    }
}

/// Runtime representation of values
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Symbol(Symbol),
    Char(char),
    Number(number::Number),
    InternedString(InternedString),
    UninternedString(std::string::String),
    ProperList(list::List),
    Procedure(procedure::Procedure),
    Closure(closure::Closure),
    Unspecified,
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
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs.is_eqv(rhs),
            (Value::Procedure(lhs), Value::Procedure(rhs)) => lhs.is_eqv(rhs),
            (Value::Unspecified, Value::Unspecified) => false,
            _ => false,
        }
    }

    fn is_eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
            (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs.is_eq(rhs),
            (Value::InternedString(lhs), Value::InternedString(rhs)) => lhs.is_eq(rhs),
            (Value::UninternedString(_), Value::InternedString(_)) => false,
            (Value::UninternedString(_), Value::UninternedString(_)) => false,
            (Value::ProperList(lhs), Value::ProperList(rhs)) => lhs.is_eq(rhs),
            (Value::Procedure(lhs), Value::Procedure(rhs)) => lhs.is_eq(rhs),
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs.is_eq(rhs),
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
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs.is_equal(rhs),
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

    pub fn integer(&self, v: BigInt) -> Value {
        Value::Number(number::Number::Integer(v))
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

    pub fn foreign_procedure(&mut self, v: procedure::foreign::Procedure) -> Value {
        Value::Procedure(procedure::Procedure::foreign(v))
    }

    pub fn native_procedure(&mut self, v: procedure::native::Procedure) -> Value {
        Value::Procedure(procedure::Procedure::native(v))
    }

    pub fn closure(&mut self, v: procedure::native::Procedure) -> Value {
        Value::Closure(closure::Closure::new(v, vec![]))
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
            Sexp::Number(datum::Number::Integer(v)) => self.integer(v.clone()),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_ref_value() {
        let v1 = RefValue::new(Value::Bool(true));
        let v2 = RefValue::new(Value::Bool(true));
        let v3 = v1.clone();

        assert_ne!(v1, v2);
        assert_eq!(v1, v1);
        assert_eq!(v1, v3);
    }

    // TODO: add equality tests (especially for upvalues)

    // TODO: add test for is_false() (especially for up-values)
}
