use super::identifier;
use super::identifier::Identifier;
use super::parse_result::ParseResult;
use super::Error;
use super::Expression;
use super::Result;
use super::{body, lambda};
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, PartialEq, Debug)]
pub enum DefinitionExpression {
    DefineSimple(Identifier, Box<Expression>, SourceLocation),
    DefineProcedure(Identifier, lambda::LambdaExpression, SourceLocation),
    Begin(Vec<Box<DefinitionExpression>>, SourceLocation),
}

impl HasSourceLocation for DefinitionExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            DefinitionExpression::DefineSimple(_, _, loc) => loc,
            DefinitionExpression::DefineProcedure(_, _, loc) => loc,
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

pub fn parse(datum: &Datum) -> ParseResult<Expression> {
    parse_definition(datum).map(Expression::Define)
}

pub fn parse_definition(datum: &Datum) -> ParseResult<DefinitionExpression> {
    Expression::parse_apply_special(datum, "define", do_parse_definition)
}

pub fn do_parse_definition(
    _op: &str,
    operands: &[Datum],
    loc: &SourceLocation,
) -> Result<DefinitionExpression> {
    match operands {
        // (define (<IDENTIFIER> <def formals>) <body>)
        [definition, exprs @ ..] if definition.sexp().is_proper_list() => {
            parse_procedure_definition(definition, exprs)
        }
        // (define  <IDENTIFIER> <expression>)
        [identifier, expr] => Ok(DefinitionExpression::DefineSimple(
            identifier::parse_identifier(&identifier).res()?,
            Box::new(Expression::parse(&expr)?),
            loc.clone(),
        )),
        rest => {
            let exprs: Result<Vec<Box<DefinitionExpression>>> = rest
                .iter()
                .map(parse_definition)
                .map(|e| e.map(Box::new))
                .collect();

            Ok(DefinitionExpression::Begin(exprs?, loc.clone()))
        }
    }
}

fn parse_procedure_definition(definition: &Datum, body: &[Datum]) -> Result<DefinitionExpression> {
    match definition.sexp() {
        Sexp::List(ls) => match &ls[..] {
            [identifier, def_formals @ ..] => {
                let name = identifier::parse_identifier(&identifier).res()?;
                let label = name.string().clone();
                let formals = lambda::parse_formals(&Datum::new(Sexp::List(def_formals.to_vec()), definition.source_location().clone()))?;
                let body = body::parse(body, definition.source_location())?;

                Ok(DefinitionExpression::DefineProcedure(
                    name,
                    lambda::build(formals, body, Some(label), definition.source_location().clone()),
                    definition.source_location().clone()
                ))
            },
            _ => Error::parse_error("Invalid precedure definition. Expected (define (<IDENTIFIER> <def formals>) <body>)", definition.source_location().clone())
        },
        _ => Error::parse_error(
            "Invalid procedure definition. Expected (define (<IDENTIFIER> <def formals>) <body>)",
            definition.source_location().clone(),
        ),
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
