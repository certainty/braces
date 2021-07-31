use super::define;
use super::Error;
use super::Expression;
use super::ParseResult;
use super::Result;
use crate::compiler::frontend::reader::sexp::datum::Datum;
use crate::compiler::frontend::ParserContext;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, PartialEq, Debug)]
pub struct BeginExpression {
    pub first: Box<Expression>,
    pub rest: Vec<Box<Expression>>,
    location: SourceLocation,
}

impl HasSourceLocation for BeginExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        &self.location
    }
}

pub fn build(first: Expression, rest: Vec<Expression>, loc: SourceLocation) -> BeginExpression {
    BeginExpression {
        first: Box::new(first),
        rest: rest.iter().cloned().map(Box::new).collect(),
        location: loc,
    }
}

pub fn parse(datum: &Datum, ctx: &mut ParserContext) -> ParseResult<Expression> {
    parse_begin(datum, ctx).map(Expression::Begin)
}

pub fn parse_begin(datum: &Datum, ctx: &mut ParserContext) -> ParseResult<BeginExpression> {
    Expression::parse_apply_special(datum, "begin", ctx, do_parse_begin)
}

pub fn do_parse_begin(
    _op: &str,
    operands: &[Datum],
    loc: &SourceLocation,
    ctx: &mut ParserContext,
) -> Result<BeginExpression> {
    match operands {
        [first, rest @ ..] => {
            let parsed_first = parse_command_or_definition(first, ctx).res();
            let parsed_exprs: Result<Vec<Expression>> = rest
                .iter()
                .map(|i| parse_command_or_definition(i, ctx))
                .collect();

            Ok(build(parsed_first?, parsed_exprs?, loc.clone()))
        }
        _ => Error::parse_error("Expected (define <command-or-definition+>)", loc.clone()),
    }
}

pub fn parse_command_or_definition(
    datum: &Datum,
    ctx: &mut ParserContext,
) -> ParseResult<Expression> {
    define::parse(datum, ctx).or(|| Expression::parse(datum, ctx).into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;

    #[test]
    fn test_parse_begin() {
        assert_parse_as(
            "(begin #t)",
            Expression::begin(
                Expression::constant(make_datum(Sexp::Bool(true), 1, 8)),
                vec![],
                location(1, 1),
            ),
        )
    }
}
