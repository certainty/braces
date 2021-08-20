use super::error::Error;
use super::Expression;
use super::Result;
use super::{ParseResult, Parser};
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source::{HasSourceLocation, Location};

#[derive(Clone, Debug, PartialEq)]
pub struct ApplicationExpression {
    pub operator: Box<Expression>,
    pub operands: Vec<Box<Expression>>,
    location: Location,
}

impl ApplicationExpression {
    #[inline]
    pub fn new(
        operator: Expression,
        operands: Vec<Expression>,
        loc: Location,
    ) -> ApplicationExpression {
        ApplicationExpression {
            operator: Box::new(operator),
            operands: operands.iter().cloned().map(Box::new).collect(),
            location: loc,
        }
    }
}

impl HasSourceLocation for ApplicationExpression {
    fn source_location<'a>(&'a self) -> &'a Location {
        &self.location
    }
}

impl Expression {
    pub fn apply(operator: Expression, operands: Vec<Expression>, loc: Location) -> Expression {
        Expression::Apply(ApplicationExpression::new(operator, operands, loc))
    }
}

impl Parser {
    fn parse_apply(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_apply(datum).map(Expression::Apply).into()
    }

    pub fn do_parse_apply(&mut self, datum: &Datum) -> Result<ApplicationExpression> {
        match datum.sexp() {
            Sexp::List(ls) => {
                if let [operator, operands @ ..] = &ls[..] {
                    let operator_expr = Expression::parse(&operator)?;
                    let operands_expr: Result<Vec<Expression>> =
                        operands.iter().map(Expression::parse).collect();

                    Ok(ApplicationExpression::new(
                        operator_expr,
                        operands_expr?,
                        datum.source_location().clone(),
                    ))
                } else {
                    Error::parse_error(
                        "expected (<operator> <operand>*)",
                        datum.source_location().clone(),
                    )
                }
            }

            _ => Error::parse_error(
                "expected (<operator> <operand>*)",
                datum.source_location().clone(),
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::tests::*;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;

    #[test]
    pub fn test_apply() {
        assert_parse_as(
            "(foo #t)",
            Expression::apply(
                Expression::identifier("foo".to_string(), location(1, 2)),
                vec![Expression::constant(make_datum(Sexp::Bool(true), 1, 6))],
                location(1, 1),
            ),
        )
    }
}
