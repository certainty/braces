use super::body;
use super::error::Error;
use super::identifier;
use super::identifier::Identifier;
use super::Expression;
use super::Result;
use crate::compiler::frontend::parser::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, PartialEq, Debug)]
pub enum DefinitionExpression {
    DefineSimple(Identifier, Box<Expression>, SourceLocation),
    Begin(Vec<Box<DefinitionExpression>>, SourceLocation),
}

impl HasSourceLocation for DefinitionExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            DefinitionExpression::DefineSimple(_, _, loc) => loc,
            DefinitionExpression::Begin(_, loc) => loc,
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
///   (define (<IDENTIFIER> <def formals>) <body>)                               |
///   <syntax definition>                                                        |
///   (define-values <formals> <body>)                                           |
///   (define-record-type <IDENTIFIER> <constructor> <IDENTIFIER> <field spec>*) |
///   (begin <definition>*)
/// ```

pub fn parse(datum: &Datum) -> Result<Expression> {
    parse_definition(datum).map(Expression::Define)
}

pub fn parse_definition(datum: &Datum) -> Result<DefinitionExpression> {
    match Expression::apply_special(datum) {
        Some(("define", [identifier, expr])) => Ok(DefinitionExpression::DefineSimple(
            identifier::parse_identifier(&identifier)?,
            Box::new(Expression::parse_expression(&expr)?),
            datum.source_location().clone(),
        )),
        Some(("begin", rest)) => {
            let exprs: Result<Vec<Box<DefinitionExpression>>> = rest
                .iter()
                .map(parse_definition)
                .map(|e| e.map(Box::new))
                .collect();

            Ok(DefinitionExpression::Begin(
                exprs?,
                datum.source_location().clone(),
            ))
        }
        _ => Error::parse_error("Invalid definition", datum.location.clone()),
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::parser::sexp::datum::Sexp;

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
