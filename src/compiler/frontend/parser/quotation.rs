use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::source::HasSourceLocation;

use super::frontend::error::Error;
use super::{Expression, ParseResult, Result};
use crate::compiler::frontend::parser::literal::LiteralExpression;

impl CoreParser {
    #[inline]
    pub fn parse_quote(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_quote(datum).map(Expression::Literal).into()
    }

    pub fn do_parse_quote(&mut self, datum: &Datum) -> Result<LiteralExpression> {
        match self.parse_list(datum)? {
            [_, value] => Ok(LiteralExpression::new(value.clone())),
            _ => Err(Error::parse_error(
                "Expected (quote <datum>) or (quasi-quote <datum>)",
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
    fn test_parse_quote() {
        assert_parse_as(
            "(quote #t)",
            Expression::literal(Datum::boolean(true, 7..9)),
        )
    }

    #[test]
    fn test_parse_literal_quoted_datum() {
        assert_parse_as("'#t", Expression::literal(Datum::boolean(true, 1..3)));

        assert_parse_as("'#\\a", Expression::literal(Datum::character('a', 1..4)));

        assert_parse_as("'foo", Expression::literal(Datum::symbol("foo", 1..4)));
    }
}
