use super::define;
use super::error::Error;
use super::Expression;
use super::Result;
use crate::compiler::frontend::parser::sexp::datum::Datum;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, PartialEq, Debug)]
pub struct BeginExpression {
    pub first: Box<Expression>,
    pub rest: Vec<Box<Expression>>,
    location: SourceLocation,
}

impl HasSourceLocation for BeginExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        &self.location
    }
}

pub fn build(first: Expression, rest: Vec<Expression>, loc: SourceLocation) -> BeginExpression {
    BeginExpression {
        first: Box::new(first),
        rest: rest.iter().cloned().map(Box::new).collect(),
        location: loc,
    }
}

pub fn parse(datum: &Datum) -> Result<Expression> {
    parse_begin(datum).map(Expression::Begin)
}

pub fn parse_begin(datum: &Datum) -> Result<BeginExpression> {
    match Expression::apply_special(datum) {
        Some(("begin", [first, rest @ ..])) => {
            let parsed_first = parse_command_or_definition(first);
            let parsed_exprs: Result<Vec<Expression>> =
                rest.iter().map(parse_command_or_definition).collect();

            Ok(build(
                parsed_first?,
                parsed_exprs?,
                datum.source_location().clone(),
            ))
        }
        _ => Error::parse_error(
            "Expected (define <command-or-definition+>)",
            datum.source_location().clone(),
        ),
    }
}

pub fn parse_command_or_definition(datum: &Datum) -> Result<Expression> {
    define::parse(datum).or_else(|_| Expression::parse_expression(datum))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::parser::sexp::datum::Sexp;

    #[test]
    fn test_parse_begin() {
        assert_parse_as(
            "(begin #t)",
            Expression::begin(
                Expression::constant(make_datum(Sexp::Bool(true), 1, 8)),
                vec![],
                location(1, 1),
            ),
        )
    }
}
