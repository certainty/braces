use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

use super::frontend::error::Error;
use super::{Expression, ParseResult, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum QuotationExpression {
    Quote(Datum),
}

impl QuotationExpression {
    pub fn new(datum: Datum) -> Self {
        QuotationExpression::Quote(datum)
    }

    pub fn datum(&self) -> &Datum {
        match self {
            Self::Quote(d) => d,
        }
    }
}

impl HasSourceLocation for QuotationExpression {
    fn source_location(&self) -> &Location {
        match self {
            Self::Quote(d) => d.source_location(),
        }
    }
}

impl Expression {
    pub fn quoted_value(datum: Datum) -> Expression {
        Expression::Quotation(QuotationExpression::new(datum))
    }
}

/// Create a quotation expression
///
/// Quoted values are special in the sense that they maintain a reference
/// to the quote `Datum`. They're treated as unevaluated expressions.

impl CoreParser {
    #[inline]
    pub fn parse_quote(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_quote(datum).map(Expression::Quotation).into()
    }

    pub fn do_parse_quote(&mut self, datum: &Datum) -> Result<QuotationExpression> {
        match self.parse_list(datum)? {
            [_, value] => Ok(QuotationExpression::new(value.clone())),
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
    use crate::compiler::frontend::parser::tests::*;
    use crate::compiler::frontend::reader::sexp::Sexp;

    use super::*;

    #[test]
    fn test_parse_quote() {
        assert_parse_as(
            "(quote #t)",
            Expression::quoted_value(make_datum(Sexp::Bool(true), 7, 9)),
        )
    }

    #[test]
    fn test_parse_literal_quoted_datum() {
        assert_parse_as(
            "'#t",
            Expression::quoted_value(make_datum(Sexp::Bool(true), 1, 3)),
        );

        assert_parse_as(
            "'#\\a",
            Expression::quoted_value(make_datum(Sexp::character('a'), 1, 4)),
        );

        assert_parse_as(
            "'foo",
            Expression::quoted_value(make_datum(Sexp::symbol("foo"), 1, 4)),
        );
    }
}
