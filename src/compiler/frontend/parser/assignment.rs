use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::reader::sexp::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

use super::frontend::error::Error;
use super::identifier;
use super::Expression;
use super::ParseResult;
use super::Result;

#[derive(Clone, PartialEq, Debug)]
pub struct SetExpression {
    pub name: identifier::Identifier,
    pub value: Box<Expression>,
    location: Location,
}

impl SetExpression {
    pub fn new(id: identifier::Identifier, expr: Expression, loc: Location) -> Self {
        SetExpression {
            name: id,
            value: Box::new(expr),
            location: loc,
        }
    }
}

impl HasSourceLocation for SetExpression {
    fn source_location(&self) -> &Location {
        &self.location
    }
}

impl Expression {
    pub fn assign(id: identifier::Identifier, expr: Expression, loc: Location) -> Expression {
        Expression::Assign(SetExpression::new(id, expr, loc))
    }
}

/// Parse a set! expression
///
/// Ref: r7rs 7.1.3
///
/// ```grammar
/// <assignment> -> (set! <IDENTIFIER> <expression>)
/// ```

impl CoreParser {
    pub fn parse_set(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_set(datum).map(Expression::Assign).into()
    }

    fn do_parse_set(&mut self, datum: &Datum) -> Result<SetExpression> {
        match self.parse_list(datum)? {
            [_, identifier, expr] => Ok(SetExpression::new(
                self.do_parse_identifier(identifier).res()?,
                self.parse(expr)?,
                datum.source_location().clone(),
            )),

            _other => Err(Error::parse_error(
                "Expected (set! <identifier> <expression>)",
                Detail::new("", datum.source_location().clone()),
                vec![],
            )),
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::compiler::frontend::parser::tests::*;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;

    use super::*;

    #[test]
    fn test_parse_assignment() {
        assert_parse_as(
            "(set! foo #t)",
            Expression::assign(
                identifier::Identifier::synthetic("foo"),
                Expression::constant(make_datum(Sexp::Bool(true), 10, 12)),
                location(0..13),
            ),
        );

        assert_parse_error("(set! foo)");
    }
}