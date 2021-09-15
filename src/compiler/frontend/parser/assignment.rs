use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

use super::frontend::error::Error;
use super::Expression;
use super::ParseResult;
use super::Result;

#[derive(Clone, PartialEq, Debug)]
pub struct SetExpression {
    pub location: Box<Expression>,
    pub value: Box<Expression>,
    source_location: Location,
}

impl SetExpression {
    pub fn new(location: Expression, expr: Expression, loc: Location) -> Self {
        SetExpression {
            location: Box::new(location),
            value: Box::new(expr),
            source_location: loc,
        }
    }
}

impl HasSourceLocation for SetExpression {
    fn source_location(&self) -> &Location {
        &self.source_location
    }
}

impl Expression {
    pub fn assign<L: Into<Location>>(location: Expression, expr: Expression, loc: L) -> Expression {
        Expression::Assign(SetExpression::new(location, expr, loc.into()))
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
                self.parse(identifier)?,
                self.parse(expr)?,
                datum.source_location().clone(),
            )),

            _other => Err(Error::parse_error(
                "Expected (set! <expression> <expression>)",
                Detail::new("", datum.source_location().clone()),
                vec![],
            )),
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::compiler::frontend::parser::tests::*;

    use super::*;

    #[test]
    fn test_parse_assignment() {
        assert_parse_as(
            "(set! foo #t)",
            Expression::assign(
                Expression::identifier("foo".to_string(), 6..9),
                Expression::literal(Datum::boolean(true, 10..12)),
                0..13,
            ),
        );

        assert_parse_error("(set! foo)");
    }
}
