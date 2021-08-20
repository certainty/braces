use super::error::Error;
use super::{Expression, ParseResult, Parser, Result};
use crate::compiler::frontend::reader::sexp::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

#[derive(Clone, PartialEq, Debug)]
pub struct BeginExpression {
    pub first: Box<Expression>,
    pub rest: Vec<Box<Expression>>,
    location: Location,
}

impl BeginExpression {
    pub fn new(first: Expression, rest: Vec<Expression>, loc: Location) -> BeginExpression {
        BeginExpression {
            first: Box::new(first),
            rest: rest.iter().cloned().map(Box::new).collect(),
            location: loc,
        }
    }
}

impl HasSourceLocation for BeginExpression {
    fn source_location<'a>(&'a self) -> &'a Location {
        &self.location
    }
}

impl Expression {
    pub fn begin(first: Expression, rest: Vec<Expression>, loc: Location) -> Expression {
        Expression::Begin(BeginExpression::new(first, rest, loc))
    }
}

impl Parser {
    pub fn parse_begin(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_begin(datum).map(Expression::Begin).into()
    }

    fn do_parse_begin(&mut self, datum: &Datum, loc: &Location) -> Result<BeginExpression> {
        match self.parse_list(datum)? {
            [_, first, rest @ ..] => {
                let parsed_first = self.parse_command_or_definition(first).res();
                let parsed_exprs: Result<Vec<Expression>> = rest
                    .iter()
                    .map(|d| self.parse_command_or_definition(d))
                    .collect();

                Ok(BeginExpression::new(
                    parsed_first?,
                    parsed_exprs?,
                    loc.clone(),
                ))
            }
            _ => Error::parse_error("Expected (define <command-or-definition+>)", loc.clone()),
        }
    }

    fn parse_command_or_definition(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.parse_define(datum).or(|| self.parse(datum))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::tests::*;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;

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
