use super::body::BodyExpression;
use super::error::Error;
use super::identifier::Identifier;
use super::{Expression, ParseResult, Parser, Result};
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source::{HasSourceLocation, Location};
use crate::vm::value::procedure::Arity;

#[derive(Clone, PartialEq, Debug)]
pub struct LambdaExpression {
    pub formals: Formals,
    pub body: BodyExpression,
    pub label: Option<String>, // in case of form rewrites to lambda we track where this lambda was generated from
    location: Location,
}

impl LambdaExpression {
    pub fn new(
        formals: Formals,
        body: BodyExpression,
        label: Option<String>,
        location: Location,
    ) -> Self {
        Self {
            formals,
            body,
            location,
            label,
        }
    }
}

impl HasSourceLocation for LambdaExpression {
    fn source_location<'a>(&'a self) -> &'a Location {
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

impl Expression {
    pub fn lambda(
        formals: Formals,
        body: BodyExpression,
        label: Option<String>,
        loc: Location,
    ) -> Expression {
        Expression::Lambda(LambdaExpression::new(formals, body, label, loc))
    }
}

impl Parser {
    #[inline]
    pub fn parse_lambda(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_lambda(datum).map(Expression::Lambda).into()
    }

    pub fn do_parse_lambda(&mut self, datum: &Datum, loc: &Location) -> Result<LambdaExpression> {
        match self.parse_list(datum)? {
            [formals, body @ ..] => {
                let formals = self.parse_formals(formals)?;
                let body = self.parse_body(body, loc)?;
                Ok(LambdaExpression::new(
                    formals,
                    body,
                    Some(String::from("lambda")),
                    loc.clone(),
                ))
            }
            _ => Error::parse_error("Expected (lambda <formals> <body>)", loc.clone()),
        }
    }

    pub fn parse_formals(&mut self, datum: &Datum) -> Result<Formals> {
        match datum.sexp() {
            Sexp::List(ls) => {
                let identifiers: Result<Vec<Identifier>> =
                    ls.iter().map(|e| self.parse_identifier(d)).collect();
                Ok(Formals::ArgList(identifiers?))
            }
            Sexp::ImproperList(head, tail) => {
                let identifiers: Result<Vec<Identifier>> =
                    head.iter().map(|e| self.parse_identifier(d)).collect();
                let rest = self.parse_identifier(tail).res();

                Ok(Formals::VarArg(identifiers?, rest?))
            }
            _ => Ok(Formals::RestArg(self.parse_identifier(datum).res()?)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::tests::*;
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
