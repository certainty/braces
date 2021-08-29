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

impl CoreParser {
    pub fn parse_definition(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_definition(datum)
            .map(Expression::Define)
            .into()
    }

    pub fn do_parse_definition(&mut self, datum: &Datum) -> Result<DefinitionExpression> {
        match self.parse_list(datum)? {
            [op, identifier, expr] if op.is_symbol() => match self.denotation_of(op)? {
                Denotation::Special(Special::Define) => Ok(DefinitionExpression::DefineSimple(
                    self.do_parse_identifier(&identifier).res()?,
                    Box::new(self.parse(&expr)?),
                    datum.source_location().clone(),
                )),
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
    use crate::compiler::frontend::parser::tests::*;
    use crate::compiler::frontend::reader::sexp::SExpression;

    use super::*;

    #[test]
    fn test_parse_define() {
        assert_parse_as(
            "(define x #t)",
            Expression::define(
                Identifier::synthetic("x"),
                Expression::constant(make_datum(SExpression::Bool(true), 10, 12)),
                location(0..13),
            ),
        )
    }
}
