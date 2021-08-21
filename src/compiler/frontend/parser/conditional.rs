use super::frontend::error::Error;
use super::{Expression, ParseResult, Parser, Result};
use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::reader::sexp::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

#[derive(Clone, PartialEq, Debug)]
pub struct IfExpression {
    pub test: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Option<Box<Expression>>,
    location: Location,
}

impl IfExpression {
    pub fn new(
        test: Expression,
        consequent: Expression,
        alternate: Option<Expression>,
        location: Location,
    ) -> Self {
        Self {
            test: Box::new(test),
            consequent: Box::new(consequent),
            alternate: alternate.map(Box::new),
            location,
        }
    }
}

impl HasSourceLocation for IfExpression {
    fn source_location<'a>(&'a self) -> &'a Location {
        &self.location
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

impl Parser {
    #[inline]
    pub fn parse_if(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_if(datum).map(Expression::If).into()
    }

    pub fn do_parse_if(&mut self, datum: &Datum) -> Result<IfExpression> {
        match self.parse_list(datum)? {
            [_if, test, consequent, alternate] => {
                let test_expr = self.do_parse(&test)?;
                let consequent_expr = self.do_parse(&consequent)?;
                let alternate_expr = self.do_parse(&alternate)?;

                Ok(IfExpression::new(
                    test_expr,
                    consequent_expr,
                    Some(alternate_expr),
                    datum.source_location().clone(),
                ))
            }
            [test, consequent] => {
                let test_expr = self.do_parse(&test)?;
                let consequent_expr = self.do_parse(&consequent)?;

                Ok(IfExpression::new(
                    test_expr,
                    consequent_expr,
                    None,
                    datum.source_location().clone(),
                ))
            }
            _ => Err(Error::parse_error(
                "Expected (if <test> <consequent> <alternate>?)",
                Detail::new("", datum.source_location().clone()),
                vec![],
            )),
        }
    }
}
