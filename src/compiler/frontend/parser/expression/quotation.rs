use super::error::Error;
use super::Expression;
use super::Result;
use crate::compiler::frontend::parser::sexp::datum::Datum;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

pub fn parse(datum: &Datum) -> Result<Expression> {
    match Expression::apply_special(datum) {
        Some(("quote", operands)) => parse_quote(&operands, &datum.source_location()),
        _ => Error::parse_error(
            "Expected (quote <datum>) or (quasiquite <datum>)",
            datum.source_location().clone(),
        ),
    }
}

fn parse_quote(operands: &[Datum], loc: &SourceLocation) -> Result<Expression> {
    match operands {
        [value] => Ok(Expression::quoted_value(value)),
        _ => Error::parse_error("Too many arguments. Expected (quote <datum>).", loc.clone()),
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
            Expression::quoted_value(&make_datum(Sexp::Bool(true), 1, 8)),
        )
    }
}
