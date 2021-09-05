use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

use super::frontend::error::Error;
use super::Expression;
use super::ParseResult;
use super::Result;

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
    fn source_location(&self) -> &Location {
        &self.location
    }
}

impl Expression {
    pub fn apply<L: Into<Location>>(
        operator: Expression,
        operands: Vec<Expression>,
        loc: L,
    ) -> Expression {
        Expression::Apply(ApplicationExpression::new(operator, operands, loc.into()))
    }
}

impl CoreParser {
    pub fn parse_apply(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_apply(datum).map(Expression::Apply).into()
    }

    pub fn do_parse_apply(&mut self, datum: &Datum) -> Result<ApplicationExpression> {
        match datum {
            Datum::List(ls, loc) => {
                if let [operator, operands @ ..] = &ls[..] {
                    let operator_expr = self.parse(&operator)?;
                    let operands_expr: Result<Vec<Expression>> =
                        operands.iter().map(|e| self.parse(e)).collect();

                    Ok(ApplicationExpression::new(
                        operator_expr,
                        operands_expr?,
                        loc.clone(),
                    ))
                } else {
                    Err(Error::parse_error(
                        "expected (<operator> <operand>*)",
                        Detail::new("", loc.clone()),
                        vec![],
                    ))
                }
            }

            _ => Err(Error::parse_error(
                "expected (<operator> <operand>*)",
                Detail::new("", datum.source_location().clone()),
                vec![],
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::tests::*;

    #[test]
    pub fn test_apply() {
        assert_parse_as(
            "(foo #t)",
            Expression::apply(
                Expression::identifier("foo".to_string(), 1..4),
                vec![Expression::literal(Datum::boolean(true, 5..7))],
                0..8,
            ),
        )
    }
}
