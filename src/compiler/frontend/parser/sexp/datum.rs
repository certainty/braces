use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use num::BigInt;

#[derive(Clone)]
pub enum Exactness {
    Inexact,
    Exact,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Sign {
    Plus,
    Minus,
}

impl Sign {
    pub fn apply(&self, v: BigInt) -> BigInt {
        match self {
            Self::Plus => v,
            Self::Minus => v * -1,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Real(RealNumber),
    //Complex(num::Complex<BigInt>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum RealNumber {
    Fixnum(BigInt),
    // Should flonum be Rational as Well?
    // Should we even support flonums here? (Rationals are flonums)
    // and on the datum level we don't have machine sized types anyway
    Flonum(f64),
    Rational(num::BigRational),
}

impl RealNumber {
    pub fn sign(&self, s: &Sign) -> Self {
        match (self, s) {
            (Self::Fixnum(n), Sign::Minus) => Self::Fixnum(n * -1),
            (Self::Flonum(n), Sign::Minus) => Self::Flonum(n * -1.0),
            _ => self.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Sexp {
    Bool(bool),
    Symbol(String),
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
        Sexp::Symbol(val.into())
    }

    pub fn character(val: char) -> Self {
        Sexp::Char(val)
    }

    pub fn string(val: impl Into<String>) -> Self {
        Sexp::String(val.into())
    }

    pub fn fixnum(val: BigInt) -> Self {
        Self::real(RealNumber::Fixnum(val))
    }

    pub fn flonum(val: f64) -> Self {
        Self::real(RealNumber::Flonum(val))
    }

    pub fn real(val: RealNumber) -> Self {
        Sexp::Number(Number::Real(val))
    }

    pub fn rational(nom: BigInt, denom: BigInt) -> Self {
        Self::real(RealNumber::Rational(num::BigRational::from((nom, denom))))
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
