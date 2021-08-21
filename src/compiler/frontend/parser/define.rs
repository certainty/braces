use super::frontend::error::Error;
use super::{identifier::Identifier, lambda};
use super::{Expression, ParseResult, Parser, Result};
use crate::compiler::frontend::error::Detail;
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
    fn source_location(&self) -> &Location {
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

    pub fn do_parse_definition(&mut self, datum: &Datum) -> Result<DefinitionExpression> {
        match self.parse_list(datum)? {
            // (define (<IDENTIFIER> <def formals>) <body>)
            [_, definition, exprs @ ..] if definition.sexp().is_proper_list() => {
                self.parse_procedure_definition(definition, exprs)
            }
            // (define  <IDENTIFIER> <expression>)
            [_, identifier, expr] => Ok(DefinitionExpression::DefineSimple(
                self.do_parse_identifier(&identifier).res()?,
                Box::new(self.do_parse(&expr)?),
                datum.source_location().clone(),
            )),
            rest => {
                let exprs: Result<Vec<Box<DefinitionExpression>>> = rest
                    .iter()
                    .map(|d| self.do_parse_definition(d))
                    .map(|e| e.map(Box::new))
                    .collect();

                Ok(DefinitionExpression::Begin(
                    exprs?,
                    datum.source_location().clone(),
                ))
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
                    let name = self.do_parse_identifier(&identifier).res()?;
                    let label = name.string().clone();
                    let formals = self.parse_formals(&Datum::new(Sexp::List(def_formals.to_vec()), definition.source_location().clone()))?;
                    let body = self.parse_body(body, definition.source_location())?;

                    Ok(DefinitionExpression::DefineProcedure(
                        name,
                        lambda::LambdaExpression::new(formals, body, Some(label), definition.source_location().clone()),
                        definition.source_location().clone()
                    ))
                },
                _ => Err(Error::parse_error("Invalid procedure definition. Expected (define (<IDENTIFIER> <def formals>) <body>)", Detail::new("", definition.source_location().clone()), vec![]))
            },
            _ => Err(Error::parse_error(
                "Invalid procedure definition. Expected (define (<IDENTIFIER> <def formals>) <body>)",
                Detail::new("", definition.source_location().clone()),
                vec![]
            )),
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
                Expression::constant(make_datum(Sexp::Bool(true), 10, 12)),
                location(0..13),
            ),
        )
    }
}
