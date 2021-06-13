use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use crate::vm::value::number;
use crate::vm::value::Value;

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

    pub fn is_proper_list(&self) -> bool {
        match self {
            Self::List(_) => true,
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

#[derive(Debug, PartialEq, Clone)]
pub struct Datum {
    pub location: SourceLocation,
    pub sexp: Sexp,
}

impl Datum {
    pub fn new(sexp: Sexp, location: SourceLocation) -> Self {
        Self { sexp, location }
    }

    pub fn from_value(v: &Value, location: SourceLocation) -> Option<Self> {
        match v {
            Value::Char(c) => Some(Self::new(Sexp::character(c.clone()), location)),
            Value::Symbol(sym) => Some(Self::new(Sexp::symbol(sym.as_str()), location)),
            Value::InternedString(s) => Some(Self::new(Sexp::string(s.as_str()), location)),
            Value::UninternedString(s) => Some(Self::new(Sexp::string(s), location)),
            Value::Number(n) => Some(Self::new(Sexp::number(n.clone()), location)),
            Value::Bool(b) => Some(Self::new(Sexp::boolean(b.clone()), location)),
            Value::ProperList(ls) => {
                let elts: Option<Vec<_>> = ls
                    .iter()
                    .map(|e| Self::from_value(e, location.clone()))
                    .collect();

                elts.map(|e| Self::new(Sexp::list(e.iter().cloned()), location))
            }
            _ => None,
        }
    }

    pub fn sexp(&self) -> &Sexp {
        &self.sexp
    }

    pub fn location(&self) -> &SourceLocation {
        &self.location
    }

    pub fn is_atom(&self) -> bool {
        match self.sexp() {
            Sexp::Vector(_) => false,
            Sexp::List(_) => false,
            Sexp::ImproperList(_, _) => false,
            Sexp::ByteVector(_) => false,
            _ => true,
        }
    }
}

impl HasSourceLocation for Datum {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        &self.location
    }
}
