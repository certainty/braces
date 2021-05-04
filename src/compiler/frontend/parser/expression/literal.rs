use super::error::Error;
use super::identifier;
use super::identifier::Identifier;
use super::Result;
use super::{BodyExpression, DefinitionExpression, Expression};
use crate::compiler::frontend::parser::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use crate::vm::scheme::value::lambda::Arity;

#[repr(transparent)]
#[derive(Clone, PartialEq, Debug)]
pub struct LiteralExpression(Datum);

impl LiteralExpression {
    pub fn datum<'a>(&'a self) -> &'a Datum {
        &self.0
    }
}

pub fn build(datum: Datum) -> LiteralExpression {
    LiteralExpression(datum)
}

pub fn parse(datum: &Datum) -> Result<Expression> {
    parse_literal(datum).map(Expression::Literal)
}

pub fn parse_literal(datum: &Datum) -> Result<LiteralExpression> {
    match datum.sexp() {
        Sexp::Bool(_) => Ok(build(datum.clone())),
        Sexp::Char(_) => Ok(build(datum.clone())),
        Sexp::String(_) => Ok(build(datum.clone())),
        _ => Error::parse_error("Expected literal", datum.source_location().clone()),
    }
}

impl HasSourceLocation for LiteralExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        self.0.source_location()
    }
}
