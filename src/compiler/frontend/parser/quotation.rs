use super::error::Error;
use super::{Expression, ParseResult, Parser, Result};
use crate::compiler::frontend::reader::sexp::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

#[derive(Debug, Clone, PartialEq)]
pub enum QuotationExpression {
    Quote(Datum),
}

impl QuotationExpression {
    pub fn new(datum: Datum) -> Self {
        QuotationExpression::Quote(datum)
    }

    pub fn datum<'a>(&'a self) -> &'a Datum {
        match self {
            Self::Quote(d) => d,
        }
    }
}

impl HasSourceLocation for QuotationExpression {
    fn source_location<'a>(&'a self) -> &'a Location {
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

impl Parser {
    #[inline]
    pub fn parse_quote(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_quote(datum).map(Expression::Quotation).into()
    }

    pub fn do_parse_quote(&mut self, datum: &Datum, loc: &Location) -> Result<QuotationExpression> {
        match self.parse_list(datum)? {
            [_, value] => Ok(QuotationExpression::new(value.clone())),
            _ => Error::parse_error(
                "Expected (quote <datum>) or (quasiquite <datum>)",
                loc.clone(),
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
    fn test_parse_quote() {
        assert_parse_as(
            "(quote #t)",
            Expression::quoted_value(make_datum(Sexp::Bool(true), 1, 8)),
        )
    }

    #[test]
    fn test_parse_literal_quoted_datum() {
        assert_parse_as(
            "'#t",
            Expression::quoted_value(make_datum(Sexp::Bool(true), 1, 2)),
        );

        assert_parse_as(
            "'#\\a",
            Expression::quoted_value(make_datum(Sexp::character('a'), 1, 2)),
        );

        assert_parse_as(
            "'foo",
            Expression::quoted_value(make_datum(Sexp::symbol("foo"), 1, 2)),
        );

        assert_parse_error("'");
    }
}
