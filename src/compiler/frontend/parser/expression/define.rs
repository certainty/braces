use super::identifier;
use super::identifier::Identifier;
use super::Expression;
use super::ParseResult;
use super::Result;
use crate::compiler::frontend::parser::sexp::datum::Datum;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, PartialEq, Debug)]
pub enum DefinitionExpression {
    DefineSimple(Identifier, Box<Expression>, SourceLocation),
    Begin(Vec<Box<DefinitionExpression>>, SourceLocation),
}

impl HasSourceLocation for DefinitionExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            DefinitionExpression::DefineSimple(_, _, loc) => loc,
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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;
    use crate::compiler::frontend::parser::sexp::datum::Sexp;

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
