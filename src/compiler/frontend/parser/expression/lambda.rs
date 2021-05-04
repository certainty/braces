use super::error::Error;
use super::identifier::Identifier;
use super::Result;
use super::{BodyExpression, DefinitionExpression, Expression};
use crate::compiler::frontend::parser::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use crate::vm::scheme::value::lambda::Arity;

#[derive(Clone, PartialEq, Debug)]
pub struct LambdaExpression {
    pub formals: Formals,
    pub body: BodyExpression,
    location: SourceLocation,
}

impl LambdaExpression {
    pub fn new(formals: Formals, body: BodyExpression, location: SourceLocation) -> Self {
        Self {
            formals,
            body,
            location,
        }
    }
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

pub fn parse(datum: &Datum) -> Result<LambdaExpression> {
    match Expression::apply_special(datum) {
        Some(("lambda", [formals, body @ ..])) => {
            let formals = parse_formals(formals)?;
            let body = parse_body(body, &datum.source_location())?;
            Ok(LambdaExpression {
                formals,
                body,
                location: datum.source_location().clone(),
            })
        }
        _ => Error::parse_error(
            "Expected (lambda <formals> <body>)",
            datum.source_location().clone(),
        ),
    }
}

fn parse_formals(datum: &Datum) -> Result<Formals> {
    match datum.sexp() {
        Sexp::List(ls) => {
            let identifiers: Result<Vec<Identifier>> =
                ls.iter().map(Expression::parse_identifier).collect();
            Ok(Formals::ArgList(identifiers?))
        }
        Sexp::ImproperList(head, tail) => {
            let identifiers: Result<Vec<Identifier>> =
                head.iter().map(Expression::parse_identifier).collect();
            let rest = Expression::parse_identifier(tail);

            Ok(Formals::VarArg(identifiers?, rest?))
        }
        _ => Ok(Formals::RestArg(Expression::parse_identifier(datum)?)),
    }
}

/// Parse a body
///
/// Ref: r7rs 7.1.3
///
/// ```grammar
/// <body>         -> <definition>* <sequence>
/// <sequence>     -> <command>* <expression>
/// <command>      -> <expression>
/// ```
fn parse_body(datum: &[Datum], loc: &SourceLocation) -> Result<BodyExpression> {
    let mut definitions: Vec<DefinitionExpression> = vec![];
    let mut iter = datum.iter();
    let mut cur = iter.next();

    // parse definitions*
    while cur.is_some() {
        match Expression::parse_definition(cur.unwrap()) {
            Ok(expr) => {
                definitions.push(expr);
                cur = iter.next();
            }
            Err(_) => break,
        }
    }

    // nothing left to parse
    if cur.is_none() {
        return Error::parse_error(
            "Invalid body definition. Expected (<definition>* sequence)",
            loc.clone(),
        );
    }

    //parse the rest as sequence
    let mut sequence = vec![Expression::parse_expression(cur.unwrap())?];
    let rest: Result<Vec<Expression>> = iter.map(Expression::parse_expression).collect();
    sequence.extend(rest?);

    Ok(BodyExpression {
        definitions,
        sequence,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::parser::sexp::datum::Sexp;

    #[test]
    fn test_parse_lambda() {
        assert_parse_as(
            "(lambda all #t)",
            Expression::lambda(
                Formals::RestArg(Identifier::synthetic("all")),
                Expression::constant(&make_datum(Sexp::Bool(true), 1, 13)).to_body_expression(),
                location(1, 1),
            ),
        );

        assert_parse_as(
            "(lambda (x y) #t)",
            Expression::lambda(
                Formals::ArgList(vec![Identifier::synthetic("x"), Identifier::synthetic("y")]),
                Expression::constant(&make_datum(Sexp::Bool(true), 1, 15)).to_body_expression(),
                location(1, 1),
            ),
        );

        assert_parse_as(
            "(lambda () #t)",
            Expression::lambda(
                Formals::ArgList(vec![]),
                Expression::constant(&make_datum(Sexp::Bool(true), 1, 12)).to_body_expression(),
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
                Expression::constant(&make_datum(Sexp::Bool(true), 1, 19)).to_body_expression(),
                location(1, 1),
            ),
        );

        assert_parse_error("(lambda #t)");
        assert_parse_error("(lambda (foo . bar . baz) #t)");
    }
}
