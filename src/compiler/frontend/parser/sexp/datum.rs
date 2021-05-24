use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use crate::vm::value::number;
use num::BigInt;

#[derive(Debug, PartialEq, Clone)]
pub enum Sexp {
    Bool(bool),
    Symbol(String),
    String(String),
    Char(char),
    Number(number::Number),
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
        Sexp::Symbol(val.into())
    }

    pub fn character(val: char) -> Self {
        Sexp::Char(val)
    }

    pub fn string(val: impl Into<String>) -> Self {
        Sexp::String(val.into())
    }

    pub fn number<I: Into<number::Number>>(num: I) -> Self {
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

#[derive(Debug, PartialEq, Clone)]
pub struct Datum {
    pub location: SourceLocation,
    pub sexp: Sexp,
}

impl Datum {
    pub fn new(sexp: Sexp, location: SourceLocation) -> Self {
        Self { sexp, location }
    }

    pub fn sexp(&self) -> &Sexp {
        &self.sexp
    }

    pub fn location(&self) -> &SourceLocation {
        &self.location
    }
}

impl HasSourceLocation for Datum {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        &self.location
    }
}
