use crate::compiler::source_location::SourceLocation;

#[derive(Debug, PartialEq, Clone)]
pub enum Sexp {
    Bool(bool),
    Symbol(String),
    String(String),
    Char(char),
    List(Vec<Datum>),
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

    pub fn list<I>(elements: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        Self::List(elements.into_iter().map(Into::into).collect())
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
