use super::error::Error;
use super::identifier;
use super::identifier::Identifier;
use super::Expression;
use super::Result;
use crate::compiler::frontend::parser::sexp::datum::Datum;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, PartialEq, Debug)]
pub struct SetExpression {
    pub name: Identifier,
    pub value: Box<Expression>,
    location: SourceLocation,
}

impl HasSourceLocation for SetExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        &self.location
    }
}

pub fn build(id: Identifier, expr: Expression, loc: SourceLocation) -> SetExpression {
    SetExpression {
        name: id,
        value: Box::new(expr),
        location: loc,
    }
}

/// Parse a set! expression
///
/// Ref: r7rs 7.1.3
///
/// ```grammar
/// <assignment> -> (set! <IDENTIFIER> <expression>)
/// ```
pub fn parse(datum: &Datum) -> Result<Expression> {
    parse_set(datum).map(Expression::Assign)
}

pub fn parse_set(datum: &Datum) -> Result<SetExpression> {
    match Expression::apply_special(datum) {
        Some(("set!", [identifier, expr])) => Ok(build(
            identifier::parse_identifier(identifier)?,
            Expression::parse_expression(expr)?,
            datum.source_location().clone(),
        )),

        _other => Error::parse_error(
            "Expected (set! <identifier> <expression>)",
            datum.source_location().clone(),
        ),
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::parser::sexp::datum::Sexp;

    #[test]
    fn test_parse_assignment() {
        assert_parse_as(
            "(set! foo #t)",
            Expression::assign(
                Identifier::synthetic("foo"),
                Expression::constant(make_datum(Sexp::Bool(true), 1, 11)),
                location(1, 1),
            ),
        );

        assert_parse_error("(set! foo)");
    }
}
