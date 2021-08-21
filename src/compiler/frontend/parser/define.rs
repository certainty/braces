use super::frontend::error::Error;
use super::identifier::Identifier;
use super::{Expression, ParseResult, Parser, Result};
use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::reader::sexp::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

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

impl Parser {
    pub fn parse_definition(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_definition(datum)
            .map(Expression::Define)
            .into()
    }

    pub fn do_parse_definition(&mut self, datum: &Datum) -> Result<DefinitionExpression> {
        match self.parse_list(datum)? {
            [_, identifier, expr] => Ok(DefinitionExpression::DefineSimple(
                self.do_parse_identifier(&identifier).res()?,
                Box::new(self.do_parse(&expr)?),
                datum.source_location().clone(),
            )),
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
