use super::Error;
use super::Expression;
use super::ParseResult;
use super::Result;
use crate::compiler::frontend::parser::syntax::environment::{
    Denotation, Special, SyntacticContext,
};
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::frontend::ParserContext;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Debug, Clone, PartialEq)]
pub enum QuotationExpression {
    Quote(Datum),
    QuasiQuote(QuasiQuotedExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum QuasiQuotedExpression {
    Datum(Datum),
    List(Vec<QuasiQuotedElement>, SourceLocation),
    ImproperList(Vec<QuasiQuotedElement>, QuasiQuotedElement, SourceLocation),
    Vector(Vec<QuasiQuotedElement>, SourceLocation),
}

#[derive(Debug, Clone, PartialEq)]
pub enum QuasiQuotedElement {
    Quote(Datum),
    Unquote(Box<Expression>, SourceLocation),
    UnquoteSplicing(Box<Expression>, SourceLocation),
}

impl HasSourceLocation for QuasiQuotedExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            Self::Vector(_, loc) => loc,
            Self::ImproperList(_, _, loc) => loc,
            Self::List(_, loc) => loc,
            Self::Datum(d) => d.source_location(),
        }
    }
}

impl HasSourceLocation for QuasiQuotedElement {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            Self::Quote(d) => d.source_location(),
            Self::Unquote(_, loc) => loc,
            Self::UnquoteSplicing(_, loc) => loc,
        }
    }
}

impl HasSourceLocation for QuotationExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            Self::Quote(d) => d.source_location(),
            Self::QuasiQuote(e) => e.source_location(),
        }
    }
}

pub fn build_quote(datum: Datum) -> QuotationExpression {
    QuotationExpression::Quote(datum)
}

#[inline]
pub fn parse(datum: &Datum, ctx: &mut ParserContext) -> ParseResult<Expression> {
    match datum.list_slice() {
        Some([operator, operands @ ..]) if operator.is_symbol() => {
            match ctx.denotation_of(operator) {
                Ok(denotation) => match denotation {
                    Denotation::Special(Special::Quote) => parse_quote(&datum, &operands, ctx)
                        .map(Expression::Quotation)
                        .into(),
                    Denotation::Special(Special::QuasiQuote) => {
                        parse_quasi_quote(&datum, &operands, ctx)
                            .map(|e| Expression::Quotation(QuotationExpression::QuasiQuote(e)))
                            .into()
                    }
                    Denotation::Special(Special::Unquote) => Error::parse_error(
                        "Unexpected unquote outside of quasi-quote",
                        datum.source_location().clone(),
                    )
                    .into(),
                    Denotation::Special(Special::UnquoteSplicing) => Error::parse_error(
                        "Unexpected unquote-splicing outside of quasi-quote",
                        datum.source_location().clone(),
                    )
                    .into(),
                    _ => Error::parse_error(
                        "Expected (quote <datum>) or (quasi-quote <datum>)",
                        datum.source_location().clone(),
                    )
                    .into(),
                },
                Err(_) => Error::parse_error(
                    "Expected quoted expression",
                    datum.source_location().clone(),
                )
                .into(),
            }
        }
        _ => ParseResult::ignore("Not a quoted expression", datum.source_location().clone()),
    }
}

pub fn parse_quote(
    datum: &Datum,
    operands: &[Datum],
    _ctx: &mut ParserContext,
) -> Result<QuotationExpression> {
    match operands {
        [datum] => Ok(QuotationExpression::Quote(datum.clone())),
        _ => Error::parse_error("Expected (quote <datum>)", datum.source_location().clone()),
    }
}

pub fn parse_quasi_quote(
    datum: &Datum,
    operands: &[Datum],
    ctx: &mut ParserContext,
) -> Result<QuasiQuotedExpression> {
    match operands {
        [quoted_datum] => match quoted_datum.sexp() {
            // `(1 2 3) -> (quasi-quote (1 2 3))
            Sexp::List(elements) => Ok(QuasiQuotedExpression::List(
                parse_quasi_quoted_elements(elements, ctx)?,
                datum.source_location().clone(),
            )),
            // `(1 2 3 . 4) -> (quasi-quote (1 2 3 . 4))
            Sexp::ImproperList(head, tail) => Ok(QuasiQuotedExpression::ImproperList(
                parse_quasi_quoted_elements(head, ctx)?,
                parse_quasi_quoted_element(tail, ctx)?,
                datum.source_location().clone(),
            )),

            // `#(1 2 3 4) -> (quasi-quote #(1 2 3 4))
            Sexp::Vector(elements) => Ok(QuasiQuotedExpression::Vector(
                parse_quasi_quoted_elements(elements, ctx)?,
                datum.source_location().clone(),
            )),

            // `any-datum
            _ => Ok(QuasiQuotedExpression::Datum(datum.clone())),
        },
        _ => Error::parse_error(
            "Expected (quasi-quote <datum>)",
            datum.source_location().clone(),
        ),
    }
}

#[inline]
fn parse_quasi_quoted_elements(
    elements: &Vec<Datum>,
    ctx: &mut ParserContext,
) -> Result<Vec<QuasiQuotedElement>> {
    elements
        .iter()
        .map(|e| parse_quasi_quoted_element(e, ctx))
        .collect()
}

fn parse_quasi_quoted_element(
    element: &Datum,
    ctx: &mut ParserContext,
) -> Result<QuasiQuotedElement> {
    match element.list_slice() {
        Some([operator, operand]) if operator.is_symbol() => match ctx.denotation_of(operator)? {
            Denotation::Special(Special::QuasiQuote) => Error::parse_error(
                "Can't nest quasi-quote like that",
                element.source_location().clone(),
            ),
            Denotation::Special(Special::Unquote) => Ok(QuasiQuotedElement::Unquote(
                Box::new(Expression::parse(&operand, ctx)?),
                element.source_location().clone(),
            )),
            Denotation::Special(Special::UnquoteSplicing) => {
                Ok(QuasiQuotedElement::UnquoteSplicing(
                    Box::new(Expression::parse(&operand, ctx)?),
                    element.source_location().clone(),
                ))
            }
            _ => Error::parse_error(
                "Unexpected expression in quotation",
                element.source_location().clone(),
            ),
        },
        _ => Ok(QuasiQuotedElement::Quote(element.clone())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
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
