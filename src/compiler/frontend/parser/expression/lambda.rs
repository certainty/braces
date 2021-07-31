use super::body;
use super::body::BodyExpression;
use super::identifier;
use super::identifier::Identifier;
use super::parse_result::ParseResult;
use super::Error;
use super::Expression;
use super::Result;
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::frontend::ParserContext;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use crate::vm::value::procedure::Arity;

#[derive(Clone, PartialEq, Debug)]
pub struct LambdaExpression {
    pub formals: Formals,
    pub body: BodyExpression,
    pub label: Option<String>, // in case of form rewrites to lambda we track where this lambda was generated from
    location: SourceLocation,
}

impl HasSourceLocation for LambdaExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        &self.location
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Formals {
    ArgList(Vec<Identifier>),
    RestArg(Identifier),
    VarArg(Vec<Identifier>, Identifier),
}

impl Formals {
    pub fn empty() -> Formals {
        Formals::ArgList(vec![])
    }

    pub fn arity(&self) -> Arity {
        match self {
            Self::ArgList(v) => Arity::Exactly(v.len()),
            Self::RestArg(_) => Arity::Many,
            Self::VarArg(v, _) => Arity::AtLeast(v.len()),
        }
    }

    pub fn identifiers(&self) -> Vec<Identifier> {
        match self {
            Formals::ArgList(ids) => ids.to_vec(),
            Formals::RestArg(id) => vec![id.clone()],
            Formals::VarArg(ids, other) => {
                let mut ret = ids.clone();
                ret.push(other.clone());
                ret
            }
        }
    }
}

pub fn build(
    formals: Formals,
    body: BodyExpression,
    label: Option<String>,
    location: SourceLocation,
) -> LambdaExpression {
    LambdaExpression {
        formals,
        body,
        location,
        label,
    }
}

#[inline]
pub fn parse(datum: &Datum, ctx: &mut ParserContext) -> ParseResult<Expression> {
    parse_lambda(datum, ctx).map(Expression::Lambda)
}

pub fn parse_lambda(datum: &Datum, ctx: &mut ParserContext) -> ParseResult<LambdaExpression> {
    Expression::parse_apply_special(datum, "lambda", ctx, do_parse_lambda)
}

pub fn do_parse_lambda(
    _op: &str,
    operands: &[Datum],
    loc: &SourceLocation,
    ctx: &mut ParserContext,
) -> Result<LambdaExpression> {
    match &operands {
        [formals, body @ ..] => {
            let formals = parse_formals(formals)?;
            let body = body::parse(body, loc, ctx)?;
            Ok(LambdaExpression {
                formals,
                body,
                label: Some(String::from("lambda")),
                location: loc.clone(),
            })
        }
        _ => Error::parse_error("Expected (lambda <formals> <body>)", loc.clone()),
    }
}

pub fn parse_formals(datum: &Datum) -> Result<Formals> {
    match datum.sexp() {
        Sexp::List(ls) => {
            let identifiers: Result<Vec<Identifier>> =
                ls.iter().map(identifier::parse_identifier).collect();
            Ok(Formals::ArgList(identifiers?))
        }
        Sexp::ImproperList(head, tail) => {
            let identifiers: Result<Vec<Identifier>> =
                head.iter().map(identifier::parse_identifier).collect();
            let rest = identifier::parse_identifier(tail).res();

            Ok(Formals::VarArg(identifiers?, rest?))
        }
        _ => Ok(Formals::RestArg(identifier::parse_identifier(datum).res()?)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;

    #[test]
    fn test_parse_lambda() {
        assert_parse_as(
            "(lambda all #t)",
            Expression::lambda(
                Formals::RestArg(Identifier::synthetic("all")),
                Expression::constant(make_datum(Sexp::Bool(true), 1, 13)).to_body_expression(),
                Some(String::from("lambda")),
                location(1, 1),
            ),
        );

        assert_parse_as(
            "(lambda (x y) #t)",
            Expression::lambda(
                Formals::ArgList(vec![Identifier::synthetic("x"), Identifier::synthetic("y")]),
                Expression::constant(make_datum(Sexp::Bool(true), 1, 15)).to_body_expression(),
                Some(String::from("lambda")),
                location(1, 1),
            ),
        );

        assert_parse_as(
            "(lambda () #t)",
            Expression::lambda(
                Formals::ArgList(vec![]),
                Expression::constant(make_datum(Sexp::Bool(true), 1, 12)).to_body_expression(),
                Some(String::from("lambda")),
                location(1, 1),
            ),
        );

        assert_parse_as(
            "(lambda (x y . z) #t)",
            Expression::lambda(
                Formals::VarArg(
                    vec![Identifier::synthetic("x"), Identifier::synthetic("y")],
                    Identifier::synthetic("z"),
                ),
                Expression::constant(make_datum(Sexp::Bool(true), 1, 19)).to_body_expression(),
                Some(String::from("lambda")),
                location(1, 1),
            ),
        );

        assert_parse_error("(lambda #t)");
        assert_parse_error("(lambda (foo . bar . baz) #t)");
    }

    #[test]
    fn test_formals_identifiers() {
        let formals = Formals::VarArg(
            vec![Identifier::synthetic("x"), Identifier::synthetic("y")],
            Identifier::synthetic("z"),
        );

        assert_eq!(
            formals.identifiers(),
            vec![
                Identifier::synthetic("x"),
                Identifier::synthetic("y"),
                Identifier::synthetic("z")
            ]
        )
    }
}
