/*
use super::body;
use super::identifier;
use super::identifier::Identifier;
use super::parse_result::ParseResult;
use super::Error;
use super::Expression;
use super::Result;
use super::{body::BodyExpression, lambda::Formals};
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

pub type BindingSpec = (Identifier, Expression);

#[derive(Clone, PartialEq, Debug)]
pub enum LetExpression {
    Let(Vec<BindingSpec>, Box<BodyExpression>, SourceLocation),
}

impl LetExpression {
    // re-write to equivalent lambda expression
    // TODO: should this be done in an explicit compiler pass?
    pub fn to_lambda(&self) -> Expression {
        match self {
            Self::Let(bindings, body, source) => {
                let formals = Formals::ArgList(bindings.iter().cloned().map(|e| e.0).collect());
                let lambda = Expression::lambda(
                    formals,
                    (**body).clone(),
                    Some(String::from("core#let")),
                    source.clone(),
                );
                let operands = bindings.iter().cloned().map(|e| e.1).collect();
                Expression::apply(lambda, operands, source.clone())
            }
        }
    }
}

impl HasSourceLocation for LetExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            Self::Let(_, _, loc) => loc,
        }
    }
}

#[inline]
pub fn build_let(
    bindings: Vec<BindingSpec>,
    body: BodyExpression,
    loc: SourceLocation,
) -> LetExpression {
    LetExpression::Let(bindings, Box::new(body), loc)
}

/// Parse a let expression
///
/// Ref: r7rs 7.1.3 (derived expression)
///
/// ```grammar
/// <derived expression> ->
///   (let <IDENTIFIER> (<binding spec>*) <body>)
///
/// <binding spec> -> (<IDENTIFIER> <expression>)
/// <body>         -> <definition>* <sequence>
/// <sequence>     -> <command>* <expression>
/// <command>      -> <expression>
///
/// ```

#[inline]
pub fn parse(datum: &Datum) -> ParseResult<Expression> {
    parse_let(datum).map(Expression::Let)
}

pub fn parse_let(datum: &Datum) -> ParseResult<LetExpression> {
    Expression::parse_apply_special(datum, "let", do_parse_let)
}

pub fn do_parse_let(_op: &str, operands: &[Datum], loc: &SourceLocation) -> Result<LetExpression> {
    match operands {
        [binding_spec, body @ ..] => Ok(build_let(
            parse_binding_specs(binding_spec)?,
            body::parse(body, loc)?,
            loc.clone(),
        )),
        _other => Error::parse_error(
            "Expected (let (<bindings>*) body) or (let name (<bindings*>) body)",
            loc.clone(),
        ),
    }
}

/// Parse a let expression
///
/// Ref: r7rs 7.1.3 (derived expression)
///
/// ```grammar
/// <binding spec> -> (<IDENTIFIER> <expression>)
/// ```
fn parse_binding_specs(datum: &Datum) -> Result<Vec<BindingSpec>> {
    match datum.sexp() {
        Sexp::List(ls) => ls.iter().map(parse_binding_spec).collect(),
        other => Error::parse_error(
            &format!("Expected list of binding specs but got {:?}", other),
            datum.location.clone(),
        ),
    }
}

fn parse_binding_spec(datum: &Datum) -> Result<BindingSpec> {
    match datum.sexp() {
        Sexp::List(ls) => match &ls[..] {
            [identifier, expr] => Ok((
                identifier::parse_identifier(identifier).res()?,
                Expression::parse(expr)?,
            )),
            _ => Error::parse_error(
                "Expected list of exactly two elements for binding. (<identifier> <expression>)",
                datum.location.clone(),
            ),
        },
        _ => Error::parse_error(
            "Expected list of exactly two elements for binding. (<identifier> <expression>)",
            datum.location.clone(),
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;

    #[test]
    fn test_parse_let_simple() {
        assert_parse_as(
            "(let ((x #t)) #f)",
            Expression::let_bind(
                vec![(
                    Identifier::synthetic("x"),
                    Expression::constant(make_datum(Sexp::Bool(true), 1, 10)),
                )],
                Expression::constant(make_datum(Sexp::Bool(false), 1, 15)).to_body_expression(),
                location(1, 1),
            ),
        )
    }
}
*/
