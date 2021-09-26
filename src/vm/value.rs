use std::convert::Into;
use std::fmt::Formatter;

use thiserror::Error;

use access::Reference;
use equality::SchemeEqual;

use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::utils::string_table::{Interned, StringTable};
use crate::vm::scheme::writer::Writer;
use crate::vm::value::number::real::RealNumber;

use self::symbol::Symbol;
use crate::compiler::frontend::syntax;
use crate::vm::value::byte_vector::ByteVector;
use crate::vm::value::vector::Vector;

pub mod access;
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

#[derive(Error, Debug)]
pub enum Error {
    #[error("Not an interned value")]
    NotInterned,
}

/// Runtime representation of values
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Syntax(Datum),
    Bool(bool),
    Symbol(Symbol),
    UninternedSymbol(syntax::symbol::Symbol),
    Char(char),
    Number(number::Number),
    String(string::String),
    Vector(vector::Vector),
    ByteVector(byte_vector::ByteVector),
    ProperList(list::List),
    ImproperList(list::List, Reference<Value>),
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

    pub fn syntax(v: Datum) -> Self {
        Value::Syntax(v)
    }

    pub fn from_datum(v: &Datum, values: &mut Factory) -> Self {
        match v {
            Datum::Char(c, _) => Self::Char(c.clone()),
            Datum::Symbol(s, _) => Value::UninternedSymbol(s.clone()),
            Datum::String(s, _) => values.string(s),
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
            (Value::String(lhs), Value::String(rhs)) => lhs.is_eq(rhs),
            (Value::Vector(lhs), Value::Vector(rhs)) => lhs.is_eq(rhs),
            (Value::ByteVector(lhs), Value::ByteVector(rhs)) => lhs.is_eq(rhs),
            (Value::ProperList(lhs), Value::ProperList(rhs)) => lhs.is_eq(rhs),
            (Value::ImproperList(lhs_head, lhs_tail), Value::ImproperList(rhs_head, rhs_tail)) => {
                lhs_head.is_eq(rhs_head) && lhs_tail.is_eq(rhs_tail)
            }
            (Value::Procedure(lhs), Value::Procedure(rhs)) => lhs.is_eq(rhs),
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs.is_eq(rhs),
            (Value::Unspecified, Value::Unspecified) => false,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Number(lhs), Value::Number(rhs)) => lhs.is_eq(rhs),
            _ => false,
        }
    }

    fn is_eqv(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
            (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs.is_eqv(rhs),
            (Value::String(lhs), Value::String(rhs)) => lhs.is_eqv(rhs),
            (Value::Vector(lhs), Value::Vector(rhs)) => lhs.is_eqv(rhs),
            (Value::ByteVector(lhs), Value::ByteVector(rhs)) => lhs.is_eqv(rhs),
            (Value::ProperList(lhs), Value::ProperList(rhs)) => lhs.is_eqv(rhs),
            (Value::ImproperList(lhs_head, lhs_tail), Value::ImproperList(rhs_head, rhs_tail)) => {
                lhs_head.is_eqv(rhs_head) && lhs_tail.is_eqv(rhs_tail)
            }
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs.is_eqv(rhs),
            (Value::Procedure(lhs), Value::Procedure(rhs)) => lhs.is_eqv(rhs),
            (Value::Unspecified, Value::Unspecified) => false,
            (Value::Number(lhs), Value::Number(rhs)) => lhs.is_eqv(rhs),
            _ => false,
        }
    }

    fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
            (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs.is_equal(rhs),
            (Value::String(lhs), Value::String(rhs)) => lhs.is_equal(rhs),
            (Value::Vector(lhs), Value::Vector(rhs)) => lhs.is_equal(rhs),
            (Value::ByteVector(lhs), Value::ByteVector(rhs)) => lhs.is_equal(rhs),
            (Value::ProperList(lhs), Value::ProperList(rhs)) => lhs.is_equal(rhs),
            (Value::ImproperList(lhs_head, lhs_tail), Value::ImproperList(rhs_head, rhs_tail)) => {
                lhs_head.is_equal(rhs_head) && lhs_tail.is_equal(rhs_tail)
            }
            (Value::Procedure(lhs), Value::Procedure(rhs)) => lhs.is_equal(rhs),
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs.is_equal(rhs),
            (Value::Unspecified, Value::Unspecified) => true,
            (Value::Number(lhs), Value::Number(rhs)) => lhs.is_equal(rhs),
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

    pub fn string<T: Into<std::string::String>>(&self, v: T) -> Value {
        Value::String(string::String::from(v.into()))
    }

    pub fn proper_list(&self, vals: Vec<Value>) -> Value {
        if vals.is_empty() {
            Value::ProperList(list::List::Nil)
        } else {
            let ls: list::List = list::List::from(vals);
            Value::ProperList(ls)
        }
    }

    pub fn improper_list(&self, head: Vec<Value>, tail: Value) -> Value {
        Value::ImproperList(head.into(), Reference::from(tail))
    }

    pub fn vector(&self, values: Vec<Value>) -> Value {
        let v: Vec<Reference<Value>> = values.into_iter().map(Reference::from).collect();
        Value::Vector(Vector::from(v))
    }

    pub fn byte_vector(&self, vals: Vec<u8>) -> Value {
        Value::ByteVector(ByteVector::from(vals))
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
                    Reference::from(self.from_datum(tail)),
                )
            }
            Datum::Char(c, _) => self.character(*c),
            Datum::Number(num, _) => Value::Number(num.clone()),
            Datum::Vector(v, _) => Value::Vector(Vector::from(
                v.iter()
                    .map(|e| Reference::from(self.from_datum(e)))
                    .collect::<Vec<_>>(),
            )),
            Datum::ByteVector(v, _) => Value::ByteVector(ByteVector::from(v.clone())),
        }
    }

    pub fn interned_strings(&self) -> Vec<Interned> {
        self.strings.interned_vec()
    }
}

// Implementation of display that's useful for logging and printf debugging
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let writer = Writer::new();
        match self {
            Value::ProperList(elements) => {
                let printend_elements = elements
                    .iter()
                    .map(|e| format!("&{}", e))
                    .collect::<Vec<_>>()
                    .join(" ");
                f.write_fmt(format_args!("({})", printend_elements))
            }
            _ => f.write_str(writer.write(&self).as_str()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::vm::value::Factory;

    #[test]
    fn test_display_lists() {
        let values = Factory::default();

        let v = values.proper_list(vec![values.bool_true(), values.bool_false()]);

        assert_eq!(format!("{}", v), "(&#t &#f)")
    }
}
