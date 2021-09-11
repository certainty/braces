use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

use super::frontend::error::Error;
use super::identifier::Identifier;
use super::{Expression, ParseResult, Result};
use crate::compiler::frontend::syntax::environment::{Denotation, Special};

#[derive(Clone, PartialEq, Debug)]
pub enum DefinitionExpression {
    DefineSimple(Identifier, Box<Expression>, Location),
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
        }
    }
}

impl Expression {
    pub fn define<L: Into<Location>>(id: Identifier, expr: Expression, loc: L) -> Expression {
        Expression::Define(DefinitionExpression::new(id, expr, loc.into()))
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

impl CoreParser {
    pub fn parse_definition(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_definition(datum)
            .map(Expression::Define)
            .into()
    }

    pub fn do_parse_definition(&mut self, datum: &Datum) -> Result<DefinitionExpression> {
        match self.parse_list(datum)? {
            [op, identifier, expr] if op.is_symbol() => match self.denotation_of(op)? {
                Denotation::Special(Special::Define) => {
                    let parsed_identifier = self.do_parse_identifier(&identifier).res()?;

                    self.environment
                        .extend(parsed_identifier.symbol().clone(), Denotation::Id);

                    let parsed_expression = match self.parse(&expr)? {
                        Expression::Lambda(lambda) => {
                            Expression::Lambda(lambda.with_label(parsed_identifier.string()))
                        }
                        other => other,
                    };

                    Ok(DefinitionExpression::DefineSimple(
                        parsed_identifier,
                        Box::new(parsed_expression),
                        datum.source_location().clone(),
                    ))
                }
                _ => Err(Error::parse_error(
                    "Not a definition",
                    Detail::new("", datum.source_location().clone()),
                    vec![],
                )),
            },
            _ => Err(Error::parse_error(
                "Invalid definition",
                Detail::new(
                    "expected (define <ID> value)",
                    datum.source_location().clone(),
                ),
                vec![],
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::tests::*;

    #[test]
    fn test_parse_define() {
        assert_parse_as(
            "(define x #t)",
            Expression::define(
                Identifier::synthetic("x"),
                Expression::literal(Datum::boolean(true, 10..12)),
                0..13,
            ),
        )
    }
}
