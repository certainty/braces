use super::error::Error;
use super::Expression;
use super::Result;
use crate::compiler::frontend::parser::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

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

#[inline]
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::parser::sexp::datum::Sexp;

    // Literals
    // See: r7rs page 12 for all examples of literals we need to support
    // TODO: add support for the other literals once we support them

    #[test]
    fn test_parse_literal_constant() {
        assert_parse_as(
            "#t",
            Expression::constant(make_datum(Sexp::Bool(true), 1, 1)),
        );

        assert_parse_as(
            "\"foo\"",
            Expression::constant(make_datum(Sexp::string("foo"), 1, 1)),
        );
    }
}
