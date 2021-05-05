use super::error::Error;
use super::Expression;
use super::ParseResult;
use super::Result;
use crate::compiler::frontend::parser::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, Debug, PartialEq)]
pub struct ApplicationExpression {
    pub operator: Box<Expression>,
    pub operands: Vec<Box<Expression>>,
    location: SourceLocation,
}

impl HasSourceLocation for ApplicationExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        &self.location
    }
}

#[inline]
pub fn build(
    operator: Expression,
    operands: Vec<Expression>,
    loc: SourceLocation,
) -> ApplicationExpression {
    ApplicationExpression {
        operator: Box::new(operator),
        operands: operands.iter().cloned().map(Box::new).collect(),
        location: loc,
    }
}

#[inline]
pub fn parse(datum: &Datum) -> ParseResult<Expression> {
    parse_apply(datum).map(Expression::Apply)
}

pub fn parse_apply(datum: &Datum) -> ParseResult<ApplicationExpression> {
    do_parse_apply(datum).into()
}

pub fn do_parse_apply(datum: &Datum) -> Result<ApplicationExpression> {
    match datum.sexp() {
        Sexp::List(ls) => {
            if let [operator, operands @ ..] = &ls[..] {
                let operator_expr = Expression::parse(&operator)?;
                let operands_expr: Result<Vec<Expression>> =
                    operands.iter().map(Expression::parse).collect();

                Ok(build(
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::parser::sexp::datum::Sexp;

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
