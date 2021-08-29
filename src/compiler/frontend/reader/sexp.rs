// Sexp parser and representation
//
// This module provides parsers for scheme sexps.
// The representation that is chosen here differs from the runtime representation of values, to optimise the parsing case.
// However it's still fully possible to convert Value -> Datum and vice versa

pub use parser::*;

pub mod abbreviation;
pub mod boolean;
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
use crate::vm::value::number::Number;

#[derive(Debug, PartialEq, Clone)]
pub enum Sexp {
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

impl Sexp {
    pub fn boolean(val: bool) -> Self {
        Sexp::Bool(val)
    }

    pub fn symbol(val: impl Into<String>) -> Self {
        Sexp::Symbol(Symbol::from_code(val))
    }

    pub fn forged_symbol<T: Into<String>>(val: T) -> Self {
        Sexp::Symbol(Symbol::forged(val))
    }

    pub fn character(val: char) -> Self {
        Sexp::Char(val)
    }

    pub fn string(val: impl Into<String>) -> Self {
        Sexp::String(val.into())
    }

    pub fn number<I: Into<Number>>(num: I) -> Self {
        Self::Number(num.into())
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

    pub fn is_proper_list(&self) -> bool {
        match self {
            Self::List(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Sexp::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn vector<I>(elements: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        Self::Vector(elements.into_iter().map(Into::into).collect())
    }

    pub fn byte_vector(val: impl Into<Vec<u8>>) -> Self {
        Sexp::ByteVector(val.into())
    }
}

impl ToString for Sexp {
    fn to_string(&self) -> String {
        match self {
            Sexp::Bool(v) => v.to_string(),
            Sexp::Symbol(v) => format!("{}", v.as_str().to_string()),
            Sexp::String(v) => format!("{:?}", v),
            Sexp::Char(v) => v.to_string(),
            Sexp::Number(n) => n.to_string(),
            Sexp::List(inner) => {
                let elts: Vec<_> = inner.iter().map(|e| e.to_string()).collect();
                format!("( {} )", elts.join(" "))
            }
            Sexp::ImproperList(head, tail) => {
                let head_elts: Vec<_> = head.iter().map(|e| e.to_string()).collect();
                format!("({} . {})", head_elts.join(" "), tail.to_string())
            }
            Sexp::Vector(inner) => {
                let elts: Vec<_> = inner.iter().map(|e| e.to_string()).collect();
                format!("#( {} )", elts.join(" "))
            }
            Sexp::ByteVector(inner) => {
                let elts: Vec<_> = inner.iter().map(|e| format!("{:x?}", e)).collect();
                format!("u8({})", elts.join(" "))
            }
        }
    }
}
