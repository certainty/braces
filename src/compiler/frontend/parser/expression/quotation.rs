use super::error::Error;
use super::Expression;
use super::Result;
use crate::compiler::frontend::parser::sexp::datum::Datum;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Debug, Clone, PartialEq)]
pub enum QuotationExpression {
    Quote(Datum),
}

impl QuotationExpression {
    pub fn datum<'a>(&'a self) -> &'a Datum {
        match self {
            Self::Quote(d) => d,
        }
    }
}

impl HasSourceLocation for QuotationExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            Self::Quote(d) => d.source_location(),
        }
    }
}

pub fn build_quote(datum: Datum) -> QuotationExpression {
    QuotationExpression::Quote(datum)
}

/// Create a quotation expression
///
/// Quoted values are special in the sense that they maintain a reference
/// to the quote `Datum`. They're treated as unevaluated expressions.

pub fn parse(datum: &Datum) -> Result<Expression> {
    parse_quote(datum).map(Expression::Quotation)
}

pub fn parse_quote(datum: &Datum) -> Result<QuotationExpression> {
    match Expression::apply_special(datum) {
        Some(("quote", [value])) => Ok(build_quote(value.clone())),
        _ => Error::parse_error(
            "Expected (quote <datum>) or (quasiquite <datum>)",
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
