use super::error::Error;
use super::Expression;
use super::ParseResult;
use super::Result;
use crate::compiler::frontend::parser::sexp::datum::Datum;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, PartialEq, Debug)]
pub struct IfExpression {
    pub test: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Option<Box<Expression>>,
    location: SourceLocation,
}

impl HasSourceLocation for IfExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        &self.location
    }
}

pub fn build(
    test: Expression,
    consequent: Expression,
    alternate: Option<Expression>,
    location: SourceLocation,
) -> IfExpression {
    IfExpression {
        test: Box::new(test),
        consequent: Box::new(consequent),
        alternate: alternate.map(Box::new),
        location,
    }
}

/// Parse an if-expression
///
/// Ref: r7rs 7.1.3
///
/// ```grammar
/// <conditional> -> (if <test> <consequent> <alternate>)
/// <test>        -> <expression>
/// <consequent>  -> <expression>
/// <alternate>   -> <expression> | <empty>
/// ```

#[inline]
pub fn parse(datum: &Datum) -> ParseResult<Expression> {
    parse_if(datum).map(Expression::If)
}

pub fn parse_if(datum: &Datum) -> ParseResult<IfExpression> {
    Expression::parse_apply_special(datum, "if", do_parse_if)
}

pub fn do_parse_if(_op: &str, operands: &[Datum], loc: &SourceLocation) -> Result<IfExpression> {
    match operands {
        [test, consequent, alternate] => {
            let test_expr = Expression::parse(&test)?;
            let consequent_expr = Expression::parse(&consequent)?;
            let alternate_expr = Expression::parse(&alternate)?;

            Ok(build(
                test_expr,
                consequent_expr,
                Some(alternate_expr),
                loc.clone(),
            ))
        }
        [test, consequent] => {
            let test_expr = Expression::parse(&test)?;
            let consequent_expr = Expression::parse(&consequent)?;

            Ok(build(test_expr, consequent_expr, None, loc.clone()))
        }
        _ => Error::parse_error(
            "Expected (if <test> <consequent> <alternate>?)",
            loc.clone(),
        ),
    }
}
