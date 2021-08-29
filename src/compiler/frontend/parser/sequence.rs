use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

use super::frontend::error::Error;
use super::{Expression, ParseResult, Result};

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
    fn source_location(&self) -> &Location {
        &self.location
    }
}

impl Expression {
    pub fn begin(first: Expression, rest: Vec<Expression>, loc: Location) -> Expression {
        Expression::Begin(BeginExpression::new(first, rest, loc))
    }
}

impl CoreParser {
    pub fn parse_begin(&mut self, datum: &Datum) -> ParseResult<Expression> {
        log::trace!("parsing begin: {:?} ", datum);
        self.do_parse_begin(datum).map(Expression::Begin).into()
    }

    fn do_parse_begin(&mut self, datum: &Datum) -> Result<BeginExpression> {
        match self.parse_list(datum)? {
            [_, first, rest @ ..] => {
                let parsed_first = self.parse(first);
                let parsed_exprs: Result<Vec<Expression>> =
                    rest.iter().map(|d| self.parse(d)).collect();

                Ok(BeginExpression::new(
                    parsed_first?,
                    parsed_exprs?,
                    datum.source_location().clone(),
                ))
            }
            _ => Err(Error::parse_error(
                "Expected (define <command-or-definition+>)",
                Detail::new("", datum.source_location().clone()),
                vec![],
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::parser::tests::*;
    use crate::compiler::frontend::reader::sexp::SExpression;

    use super::*;

    #[test]
    fn test_parse_begin() {
        assert_parse_as(
            "(begin #t)",
            Expression::begin(
                Expression::constant(make_datum(SExpression::Bool(true), 7, 9)),
                vec![],
                location(0..10),
            ),
        )
    }
}
