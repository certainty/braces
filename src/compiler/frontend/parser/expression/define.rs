use super::identifier;
use super::identifier::Identifier;
use super::parse_result::ParseResult;
use super::Error;
use super::Expression;
use super::Result;
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::frontend::ParserContext;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, PartialEq, Debug)]
pub enum DefinitionExpression {
    DefineSimple(Identifier, Box<Expression>, SourceLocation),
}

impl HasSourceLocation for DefinitionExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            DefinitionExpression::DefineSimple(_, _, loc) => loc,
        }
    }
}

pub fn build_simple(id: Identifier, expr: Expression, loc: SourceLocation) -> DefinitionExpression {
    DefinitionExpression::DefineSimple(id, Box::new(expr), loc)
}

/// Parse a define expression
///
/// Ref: r7rs 7.1.6
///
/// ```grammar
/// <definition> ->
///   (define <IDENTIFIER> <expression>)                                         |
///   (define-values <formals> <body>)                                           |
///   (define-record-type <IDENTIFIER> <constructor> <IDENTIFIER> <field spec>*) |
/// ```

pub fn parse(datum: &Datum, ctx: &mut ParserContext) -> ParseResult<Expression> {
    parse_definition(datum, ctx).map(Expression::Define)
}

pub fn parse_definition(
    datum: &Datum,
    ctx: &mut ParserContext,
) -> ParseResult<DefinitionExpression> {
    Expression::parse_apply_special(datum, "define", ctx, do_parse_definition)
}

pub fn do_parse_definition(
    _op: &str,
    operands: &[Datum],
    loc: &SourceLocation,
    ctx: &mut ParserContext,
) -> Result<DefinitionExpression> {
    match operands {
        // (define  <IDENTIFIER> <expression>)
        [identifier, expr] => Ok(DefinitionExpression::DefineSimple(
            identifier::parse_identifier(&identifier).res()?,
            Box::new(Expression::parse(&expr, ctx)?),
            loc.clone(),
        )),
        _ => Error::parse_error("Invalid definitiion", loc.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;

    #[test]
    fn test_parse_define() {
        assert_parse_as(
            "(define x #t)",
            Expression::define(
                Identifier::synthetic("x"),
                Expression::constant(make_datum(Sexp::Bool(true), 1, 11)),
                location(1, 1),
            ),
        )
    }
}
