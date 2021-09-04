pub use parser::*;
pub mod abbreviation;
pub mod boolean;
pub mod byte_vector;
pub mod character;
pub mod list;
pub mod number;
pub mod parser;
pub mod string;
pub mod symbol;
pub mod vector;
pub mod whitespace;

use super::datum::Datum;
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::vm::value::number::real::RealNumber;
use crate::vm::value::number::Number;
use std::fmt::Formatter;

/// Representation of s-expressions for the frontend of the compiler
///
/// This type is used as the external representation of scheme code and data.
/// **Important**:
/// S-expressions are not used as runtime representations of values in the VM.
/// They're optimised to be ergonomic to be used by the different phases of the compiler.
/// So they should be easy to be used by the parser and easy to be used by the expander.
/// This includes both, consumption and construction of new s-expressions.
///
#[derive(Debug, PartialEq, Clone)]
pub enum SExpression {
    Bool(bool),
    Symbol(Symbol),
    String(String),
    Char(char),
    Number(Number),
    List(Vec<Datum>),
    ImproperList(Vec<Datum>, Box<Datum>),
    Vector(Vec<Datum>),
    ByteVector(Vec<u8>),
}

impl SExpression {
    pub fn symbol(val: impl Into<String>) -> Self {
        SExpression::Symbol(Symbol::from_code(val))
    }

    pub fn forged_symbol<T: Into<String>>(val: T) -> Self {
        SExpression::Symbol(Symbol::forged(val))
    }

    pub fn list<I>(elements: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        Self::List(elements.into_iter().map(Into::into).collect())
    }

    pub fn improper_list<I>(elements: I, element: Datum) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        let elts = elements.into_iter().map(Into::into).collect();
        Self::ImproperList(elts, Box::new(element))
    }

    pub fn vector<I>(elements: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        Self::Vector(elements.into_iter().map(Into::into).collect())
    }

    pub fn byte_vector<I>(bytes: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<u8>,
    {
        Self::ByteVector(bytes.into_iter().map(Into::into).collect())
    }

    pub fn is_proper_list(&self) -> bool {
        match self {
            Self::List(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            SExpression::Symbol(_) => true,
            _ => false,
        }
    }
}

impl From<bool> for SExpression {
    fn from(v: bool) -> Self {
        SExpression::Bool(v)
    }
}

impl From<String> for SExpression {
    fn from(v: String) -> Self {
        SExpression::String(v)
    }
}

impl From<char> for SExpression {
    fn from(v: char) -> Self {
        SExpression::Char(v)
    }
}

impl From<Number> for SExpression {
    fn from(v: Number) -> Self {
        SExpression::Number(v)
    }
}

impl From<RealNumber> for SExpression {
    fn from(v: RealNumber) -> Self {
        SExpression::Number(Number::Real(v))
    }
}

impl From<Vec<Datum>> for SExpression {
    fn from(v: Vec<Datum>) -> Self {
        Self::Vector(v)
    }
}

impl From<Vec<u8>> for SExpression {
    fn from(v: Vec<u8>) -> Self {
        Self::ByteVector(v)
    }
}

/// The `Display` instance for s-expressions provides
/// a representation that can be used in debug output and logs.
/// It's not as sophisticated as the `Writer`, so if you want a
/// full faithful, valid, external representation of s-expressions use the `Writer` instead.
impl std::fmt::Display for SExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpression::Bool(v) => f.write_fmt(format_args!("{}", v)),
            SExpression::Symbol(v) => f.write_fmt(format_args!("{}", v.as_str().to_string())),
            SExpression::String(v) => f.write_fmt(format_args!("\"{}\"", v)),
            SExpression::Char(v) => f.write_fmt(format_args!("{}", v.to_string())),
            SExpression::Number(n) => f.write_fmt(format_args!("{}", n.to_string())),
            SExpression::List(inner) => {
                let elements: Vec<_> = inner.iter().map(|e| e.to_string()).collect();
                f.write_fmt(format_args!("( {} )", elements.join(" ")))
            }
            SExpression::ImproperList(head, tail) => {
                let head_elements: Vec<_> = head.iter().map(|e| e.to_string()).collect();
                f.write_fmt(format_args!(
                    "({} . {})",
                    head_elements.join(" "),
                    tail.to_string()
                ))
            }
            SExpression::Vector(inner) => {
                let elements: Vec<_> = inner.iter().map(|e| e.to_string()).collect();
                f.write_fmt(format_args!("#( {} )", elements.join(" ")))
            }
            SExpression::ByteVector(inner) => {
                let elements: Vec<_> = inner.iter().map(|e| format!("{:x?}", e)).collect();
                f.write_fmt(format_args!("u8({})", elements.join(" ")))
            }
        }
    }
}
