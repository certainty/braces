use super::error::Error;
use super::{identifier::Identifier, lambda};
use super::{Expression, ParseResult, Parser, Result};
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source::{HasSourceLocation, Location};

#[derive(Clone, PartialEq, Debug)]
pub enum DefinitionExpression {
    DefineSimple(Identifier, Box<Expression>, Location),
    DefineProcedure(Identifier, lambda::LambdaExpression, Location),
    Begin(Vec<Box<DefinitionExpression>>, Location),
}

impl DefinitionExpression {
    pub fn new(id: Identifier, expr: Expression, loc: Location) -> DefinitionExpression {
        DefinitionExpression::DefineSimple(id, Box::new(expr), loc)
    }
}

impl HasSourceLocation for DefinitionExpression {
    fn source_location<'a>(&'a self) -> &'a Location {
        match self {
            DefinitionExpression::DefineSimple(_, _, loc) => loc,
            DefinitionExpression::DefineProcedure(_, _, loc) => loc,
            DefinitionExpression::Begin(_, loc) => loc,
        }
    }
}

impl Expression {
    pub fn define(id: Identifier, expr: Expression, loc: Location) -> Expression {
        Expression::Define(DefinitionExpression::new(id, expr, loc))
    }
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

impl Parser {
    pub fn parse_definition(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_definition(datum)
            .map(Expression::Define)
            .into()
    }

    pub fn do_parse_definition(
        &mut self,
        datum: &Datum,
        loc: &Location,
    ) -> Result<DefinitionExpression> {
        match self.parse_list(datum)? {
            // (define (<IDENTIFIER> <def formals>) <body>)
            [definition, exprs @ ..] if definition.sexp().is_proper_list() => {
                self.parse_procedure_definition(definition, exprs)
            }
            // (define  <IDENTIFIER> <expression>)
            [identifier, expr] => Ok(DefinitionExpression::DefineSimple(
                self.parse_identifier(&identifier).res()?,
                Box::new(Expression::parse(&expr)?),
                loc.clone(),
            )),
            rest => {
                let exprs: Result<Vec<Box<DefinitionExpression>>> = rest
                    .iter()
                    .map(|d| self.parse_definition(d))
                    .map(|e| e.map(Box::new))
                    .collect();

                Ok(DefinitionExpression::Begin(exprs?, loc.clone()))
            }
        }
    }

    fn parse_procedure_definition(
        &mut self,
        definition: &Datum,
        body: &[Datum],
    ) -> Result<DefinitionExpression> {
        match definition.sexp() {
        Sexp::List(ls) => match &ls[..] {
            [identifier, def_formals @ ..] => {
                let name = self.parse_identifier(&identifier).res()?;
                let label = name.string().clone();
                let formals = self.parse_formals(&Datum::new(Sexp::List(def_formals.to_vec()), definition.source_location().clone()))?;
                let body = self.parse_body(body, definition.source_location())?;

                Ok(DefinitionExpression::DefineProcedure(
                    name,
                    lambda::LambdaExpression::new(formals, body, Some(label), definition.source_location().clone()),
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::tests::*;
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
