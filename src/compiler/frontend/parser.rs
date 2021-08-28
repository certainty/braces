pub use core_parser::CoreParser;
pub use result::ParseResult;

use crate::compiler::frontend;
use crate::compiler::frontend::expander::Expander;
use crate::compiler::representation::{CoreAST, SexpAST};
use crate::compiler::source::{HasSourceLocation, Location};

use super::reader::sexp::datum::{Datum, Sexp};
pub use super::Result;

pub mod apply;
pub mod assignment;
pub mod body;
pub mod conditional;
pub mod core_parser;
pub mod define;
pub mod identifier;
pub mod lambda;
pub mod literal;
pub mod quotation;
pub mod result;
pub mod sequence;

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
    fn source_location(&self) -> &Location {
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

#[derive(Debug)]
pub struct Parser {
    expander: Expander,
    core_parser: CoreParser,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            expander: Expander::new(),
            core_parser: CoreParser::new(),
        }
    }

    pub fn parse(&mut self, ast: &SexpAST) -> Result<CoreAST> {
        let expressions: Result<Vec<Expression>> =
            ast.to_vec().iter().map(|d| self.do_parse(d)).collect();

        Ok(CoreAST::new(expressions?))
    }

    fn do_parse(&mut self, datum: &Datum) -> Result<Expression> {
        let expanded = self.expander.expand(datum)?;
        log::trace!(
            "expanded input {} \n ==== \n {}",
            datum.to_string(),
            expanded.to_string()
        );
        self.core_parser.parse(&expanded)
    }
}

#[cfg(test)]
pub mod tests {
    use std::ops::Range;

    use crate::compiler::frontend::reader::Reader;
    use crate::compiler::source::{BufferSource, Registry, SourceId};

    use super::*;

    pub fn assert_parse_as(inp: &str, exp: Expression) {
        let mut registry = Registry::new();
        let source = registry
            .add(&mut BufferSource::new(inp, "datum-parser-test"))
            .unwrap();
        let reader = Reader::new();
        let sexp_ast = reader.parse(&source).unwrap();
        let mut parser = Parser::new();
        let core_ast = parser.parse(&sexp_ast).unwrap();

        assert_eq!(core_ast.expressions[0], exp);
    }

    pub fn assert_parse_error(inp: &str) {
        let mut registry = Registry::new();
        let source = registry
            .add(&mut BufferSource::new(inp, "datum-parser-test"))
            .unwrap();
        let reader = Reader::new();
        let sexp_ast = reader.parse(&source).unwrap();
        let mut parser = Parser::new();
        let parse_result = parser.parse(&sexp_ast);

        assert!(
            parse_result.is_err(),
            "expected parser error but got something different"
        )
    }

    pub fn location(span: Range<usize>) -> Location {
        Location::new(SourceId::from(0), span)
    }

    pub fn make_datum(sexp: Sexp, line: usize, col: usize) -> Datum {
        Datum::new(sexp, location(line..col))
    }
}
