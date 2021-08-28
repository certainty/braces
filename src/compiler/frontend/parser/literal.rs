use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source::{HasSourceLocation, Location};

use super::{Expression, ParseResult};

#[repr(transparent)]
#[derive(Clone, PartialEq, Debug)]
pub struct LiteralExpression(Datum);

impl LiteralExpression {
    pub fn new(datum: Datum) -> Self {
        Self(datum)
    }

    pub fn datum<'a>(&'a self) -> &'a Datum {
        &self.0
    }
}

impl Expression {
    pub fn constant(datum: Datum) -> Expression {
        Expression::Literal(LiteralExpression::new(datum))
    }
}

impl HasSourceLocation for LiteralExpression {
    fn source_location(&self) -> &Location {
        self.0.source_location()
    }
}

impl CoreParser {
    #[inline]
    pub fn parse_literal(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_literal(datum).map(Expression::Literal)
    }

    pub fn do_parse_literal(&mut self, datum: &Datum) -> ParseResult<LiteralExpression> {
        match datum.sexp() {
            Sexp::Bool(_) => ParseResult::accept(LiteralExpression::new(datum.clone())),
            Sexp::Char(_) => ParseResult::accept(LiteralExpression::new(datum.clone())),
            Sexp::String(_) => ParseResult::accept(LiteralExpression::new(datum.clone())),
            Sexp::Number(_) => ParseResult::accept(LiteralExpression::new(datum.clone())),
            Sexp::Vector(_) => ParseResult::accept(LiteralExpression::new(datum.clone())),
            _ => ParseResult::ignore("Expected literal", datum.source_location().clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::parser::tests::*;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;
    use crate::vm::value::number::Number;

    use super::*;

    // Literals
    // See: r7rs page 12 for all examples of literals we need to support
    // TODO: add support for the other literals once we support them

    #[test]
    fn test_parse_literal_constant() {
        assert_parse_as(
            "#t",
            Expression::constant(make_datum(Sexp::Bool(true), 0, 2)),
        );

        assert_parse_as(
            "\"foo\"",
            Expression::constant(make_datum(Sexp::string("foo"), 0, 5)),
        );

        assert_parse_as(
            "123",
            Expression::constant(make_datum(Sexp::number(Number::fixnum(123)), 0, 3)),
        );
    }
}