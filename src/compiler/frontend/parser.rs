pub mod apply;
pub mod assignment;
pub mod body;
pub mod conditional;
pub mod define;
pub mod error;
pub mod expression;
pub mod identifier;
pub mod lambda;
pub mod literal;
pub mod quotation;
pub mod result;
pub mod sequence;

use super::error::Error as FrontendError;
use super::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::representation::{CoreAST, SexpAST};
use crate::compiler::source::{HasSourceLocation, Location};
use error::Error;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Identifier(identifier::Identifier),
    Quotation(quotation::QuotationExpression),
    Literal(literal::LiteralExpression),
    Define(define::DefinitionExpression),
    Lambda(lambda::LambdaExpression),
    Assign(assignment::SetExpression),
    If(conditional::IfExpression),
    Apply(apply::ApplicationExpression),
    Command(Box<Expression>),
    Begin(sequence::BeginExpression),
}

impl HasSourceLocation for Expression {
    fn source_location<'a>(&'a self) -> &'a Location {
        match self {
            Self::Identifier(id) => id.source_location(),
            Self::Literal(exp) => exp.source_location(),
            Self::Quotation(exp) => exp.source_location(),
            Self::Assign(exp) => exp.source_location(),
            Self::Define(def) => def.source_location(),
            Self::If(expr) => expr.source_location(),
            Self::Lambda(proc) => proc.source_location(),
            Self::Apply(exp) => exp.source_location(),
            Self::Command(exp) => exp.source_location(),
            Self::Begin(exp) => exp.source_location(),
        }
    }
}

pub use result::ParseResult;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser {}

impl Parser {
    pub fn parse(&mut self, ast: &SexpAST) -> std::result::Result<CoreAST, FrontendError> {
        ast.iter().map(|d| self.parse(d)).collect()
    }

    /// Parse a single datum into an expression
    ///
    ///
    /// Ref: r7rs 7.1.3
    /// ```grammar
    /// <expression> =>
    ///   <identifier>         |
    ///   <literal>            |
    ///   <procedure call>     |
    ///   <lambda expression>  |
    ///   <conditional>        |
    ///   <assignment>         |
    ///   <derived expression> |
    ///   <macro use>          |
    ///   <macro block>        |
    ///   <includer>           |
    /// ```

    fn parse(&mut self, datum: &Datum) -> Result<Expression> {
        self.parse_identifier(datum)
            .or(|| self.parse_literal(datum))
            .or(|| self.parse_lambda(datum))
            .or(|| self.parse_assignment(datum))
            .or(|| self.parse_quotation(datum))
            .or(|| self.parse_sequence(datum))
            .or(|| self.parse_apply(datum))
            .map_non_applicable(Error::parse_error(
                "Invalid expression",
                datum.source_location().clone(),
            ))
    }

    pub fn parse_list<'a>(&mut self, datum: &'a Datum) -> ParseResult<&'a [Datum]> {
        match datum.sexp() {
            Sexp::List(ls) => Ok(&ls[..]),
            _ => ParseResult::ignore("Expected list", datum.source_location().clone()),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::compiler::frontend::reader::Reader;
    use crate::compiler::source::{BufferSource, Registry, SourceId};
    use std::ops::Range;

    use super::*;

    pub fn assert_parse_as(inp: &str, exp: Expression) -> Result<()> {
        let mut registry = Registry::new();
        let source = registry.add(BufferSource::new(inp, "datum-parser-test"));
        let reader = Reader::new();
        let sexp_ast = reader.parse(source)?;
        let mut parser = Parser::new();
        let core_ast = parser.parse(sexp_ast)?;

        assert_eq!(core_ast.expressions[0], exp);
        Ok(())
    }

    pub fn assert_parse_error(inp: &str) {
        let mut registry = Registry::new();
        let source = registry.add(BufferSource::new(inp, "datum-parser-test"));
        let reader = Reader::new();
        let sexp_ast = reader.parse(source)?;
        let mut parser = Parser::new();
        let parse_result = parser.parse(sexp_ast);
        let message = format!("expected parse error but got {:?}", parse_result);

        assert!(parse_result.is_err(), message)
    }

    pub fn location(span: Range<usize>) -> Location {
        Location::new(SourceId::from(0), span)
    }

    pub fn make_datum(sexp: Sexp, line: usize, col: usize) -> Datum {
        Datum::new(sexp, location(line, col))
    }
}
