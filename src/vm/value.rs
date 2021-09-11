#[cfg(test)]
pub mod arbitrary;
pub mod byte_vector;
pub mod closure;
pub mod equality;
pub mod error;
pub mod list;
pub mod number;
pub mod procedure;
pub mod string;
pub mod symbol;
pub mod vector;

use self::{string::InternedString, symbol::Symbol};
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::utils::string_table::StringTable;
use crate::vm::value::number::real::RealNumber;
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

    pub fn as_ref(&self) -> Ref<Value> {
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
    Vector(vector::Vector),
    ByteVector(byte_vector::ByteVector),
    InternedString(InternedString),
    UninternedString(std::string::String),
    ProperList(list::List),
    ImproperList(list::List, Box<Value>),
    Procedure(procedure::Procedure),
    Closure(closure::Closure),
    Syntax(Datum),
    Unspecified,
}

impl Value {
    pub fn is_false(&self) -> bool {
        match self {
            Self::Bool(false) => true,
            _ => false,
        }
    }

    pub fn syntax(v: Datum) -> Self {
        Value::Syntax(v)
    }

    pub fn from_datum(v: &Datum, values: &mut Factory) -> Self {
        match v {
            Datum::Char(c, _) => Self::Char(c.clone()),
            Datum::Symbol(s, _) => values.symbol(s.as_str()),
            Datum::String(s, _) => values.interned_string(s),
            Datum::Number(n, _) => Self::Number(n.clone()),
            Datum::Bool(v, _) => Self::Bool(v.clone()),
            Datum::List(v, _) => {
                let elements: Vec<Value> = v.iter().map(|e| Self::from_datum(e, values)).collect();
                values.proper_list(elements)
            }
            Datum::ImproperList(head, tail, _) => {
                let head_elements: Vec<Value> =
                    head.iter().map(|e| Self::from_datum(e, values)).collect();
                let tail_element = Self::from_datum(tail, values);
                values.improper_list(head_elements, tail_element)
            }

            Datum::Vector(v, _) => {
                let elements: Vec<Value> = v.iter().map(|e| Self::from_datum(e, values)).collect();
                values.vector(elements)
            }

            Datum::ByteVector(v, _) => values.byte_vector(v.clone()),
        }
    }
}

impl SchemeEqual<Value> for Value {
    fn is_eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
            (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs.is_eq(rhs),
            (Value::InternedString(lhs), Value::InternedString(rhs)) => lhs.is_eq(rhs),
            (Value::UninternedString(_), Value::InternedString(_)) => false,
            (Value::UninternedString(_), Value::UninternedString(_)) => false,
            (Value::Vector(lhs), Value::Vector(rhs)) => lhs.is_eq(rhs),
            (Value::ByteVector(lhs), Value::ByteVector(rhs)) => lhs.is_eq(rhs),
            (Value::ProperList(lhs), Value::ProperList(rhs)) => lhs.is_eq(rhs),
            (Value::ImproperList(lhs_head, lhs_tail), Value::ImproperList(rhs_head, rhs_tail)) => {
                lhs_head.is_eq(rhs_head) && lhs_tail.is_eq(rhs_tail)
            }
            (Value::Procedure(lhs), Value::Procedure(rhs)) => lhs.is_eq(rhs),
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs.is_eq(rhs),
            (Value::Unspecified, Value::Unspecified) => false,
            _ => false,
        }
    }

    fn is_eqv(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
            (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs.is_eqv(rhs),
            (Value::InternedString(lhs), Value::InternedString(rhs)) => lhs.is_eqv(rhs),
            (Value::UninternedString(_), Value::InternedString(_)) => false,
            (Value::UninternedString(_), Value::UninternedString(_)) => false,
            (Value::Vector(lhs), Value::Vector(rhs)) => lhs.is_eqv(rhs),
            (Value::ByteVector(lhs), Value::ByteVector(rhs)) => lhs.is_eqv(rhs),
            (Value::ProperList(lhs), Value::ProperList(rhs)) => lhs.is_eqv(rhs),
            (Value::ImproperList(lhs_head, lhs_tail), Value::ImproperList(rhs_head, rhs_tail)) => {
                lhs_head.is_eqv(rhs_head) && lhs_tail.is_eqv(rhs_tail)
            }
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs.is_eqv(rhs),
            (Value::Procedure(lhs), Value::Procedure(rhs)) => lhs.is_eqv(rhs),
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
            (Value::Vector(lhs), Value::Vector(rhs)) => lhs.is_equal(rhs),
            (Value::ByteVector(lhs), Value::ByteVector(rhs)) => lhs.is_equal(rhs),
            (Value::ProperList(lhs), Value::ProperList(rhs)) => lhs.is_equal(rhs),
            (Value::ImproperList(lhs_head, lhs_tail), Value::ImproperList(rhs_head, rhs_tail)) => {
                lhs_head.is_equal(rhs_head) && lhs_tail.is_equal(rhs_tail)
            }
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
    true_value: Value,
    false_value: Value,
    nil_value: Value,
    unspecified: Value,
}

impl Default for Factory {
    fn default() -> Factory {
        Factory {
            strings: StringTable::default(),
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

    pub fn real<N: Into<RealNumber>>(&self, v: N) -> Value {
        Value::Number(number::Number::Real(v.into()))
    }

    pub fn sym<T: Into<std::string::String>>(&mut self, v: T) -> Symbol {
        let k = self.strings.get_or_intern(v.into());
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

    pub fn improper_list(&self, head: Vec<Value>, tail: Value) -> Value {
        Value::ImproperList(head.into(), Box::new(tail))
    }

    pub fn vector(&self, vals: Vec<Value>) -> Value {
        Value::Vector(vals)
    }

    pub fn byte_vector(&self, vals: Vec<u8>) -> Value {
        Value::ByteVector(vals)
    }

    pub fn foreign_procedure(&mut self, v: procedure::foreign::Procedure) -> Value {
        Value::Procedure(procedure::Procedure::foreign(v))
    }

    pub fn native_procedure(&mut self, v: procedure::native::Procedure) -> Value {
        Value::Procedure(procedure::Procedure::native(v))
    }

    pub fn closure(&mut self, v: procedure::native::Procedure) -> Value {
        Value::Closure(closure::Closure::new(v))
    }

    pub fn from_datum(&mut self, d: &Datum) -> Value {
        match d {
            Datum::Bool(true, _) => self.bool_true().clone(),
            Datum::Bool(false, _) => self.bool_false().clone(),
            Datum::Symbol(s, _) => self.symbol(s),
            Datum::String(s, _) => self.string(s),
            Datum::List(ls, _) => {
                let elements = ls.iter().map(|e| self.from_datum(e)).collect();
                self.proper_list(elements)
            }
            Datum::ImproperList(head, tail, _) => {
                let head_values = head.iter().map(|e| self.from_datum(e)).collect::<Vec<_>>();
                Value::ImproperList(
                    list::List::from(head_values),
                    Box::new(self.from_datum(tail)),
                )
            }
            Datum::Char(c, _) => self.character(*c),
            Datum::Number(num, _) => Value::Number(num.clone()),
            Datum::Vector(v, _) => Value::Vector(v.iter().map(|e| self.from_datum(e)).collect()),
            Datum::ByteVector(v, _) => Value::ByteVector(v.clone()),
        }
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
