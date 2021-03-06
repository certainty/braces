pub use core_parser::CoreParser;
pub use result::ParseResult;

use crate::compiler::frontend;
use crate::compiler::frontend::expander::Expander;
use crate::compiler::representation::{CoreAST, DatumAST};
use crate::compiler::source::{HasSourceLocation, Location};

pub use super::Result;
use crate::compiler::frontend::reader::datum::Datum;

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

/// The `Expression` type encodes scheme core forms.
#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    /// A scheme identifier, which is a special symbol
    Identifier(identifier::Identifier),
    /// Any scheme literal value
    Literal(literal::LiteralExpression),
    /// A definition
    Define(define::DefinitionExpression),
    /// A lambda expression
    Lambda(lambda::LambdaExpression),
    /// A set! expression
    Assign(assignment::SetExpression),
    /// An if expression
    If(conditional::IfExpression),
    /// An expression for function application
    Apply(apply::ApplicationExpression),
    /// A begin expression to sequence other expressions
    Begin(sequence::BeginExpression),
}

impl HasSourceLocation for Expression {
    fn source_location(&self) -> &Location {
        match self {
            Self::Identifier(id) => id.source_location(),
            Self::Literal(exp) => exp.source_location(),
            Self::Assign(exp) => exp.source_location(),
            Self::Define(def) => def.source_location(),
            Self::If(expr) => expr.source_location(),
            Self::Lambda(proc) => proc.source_location(),
            Self::Apply(exp) => exp.source_location(),
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

    /// Expand and parse `ast` into the `CoreAST` representation.
    /// This process interleaves macro expansion and parsing of forms.
    ///
    /// ### Example
    /// ```
    /// use braces::compiler::frontend::parser::Parser;
    /// use braces::compiler::representation::DatumAST;
    /// use braces::compiler::frontend::reader::datum::Datum;
    ///
    /// // just a very simple s-expression which will be parsed to a literal
    /// let data = DatumAST::new(vec![Datum::boolean(true, 0..2)]);
    /// let mut parser = Parser::new();
    ///
    /// parser.parse(&data).unwrap();
    /// ```
    pub fn parse(&mut self, ast: &DatumAST) -> Result<CoreAST> {
        let expressions: Result<Vec<Option<_>>> =
            ast.to_vec().iter().map(|d| self.do_parse(d)).collect();

        Ok(CoreAST::new(expressions?.into_iter().flatten().collect()))
    }

    fn do_parse(&mut self, datum: &Datum) -> Result<Option<Expression>> {
        log::trace!("parsing: {}", datum.to_string());
        if let Some(expanded) = self.expander.expand(datum)? {
            log::trace!("expanded: {}", expanded.to_string());
            Ok(Some(self.core_parser.parse(&expanded)?))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::compiler::frontend::reader::Reader;
    use crate::compiler::source::{BufferSource, Registry};

    pub fn assert_parse_as(inp: &str, exp: Expression) {
        let mut registry = Registry::new();
        let source = registry
            .add(&mut BufferSource::new(inp, "datum-parser-test"))
            .unwrap();
        let reader = Reader::new();
        let datum_ast = reader.parse(&source).unwrap();
        let mut parser = Parser::new();
        let core_ast = parser.parse(&datum_ast).unwrap();

        assert_eq!(core_ast.expressions[0], exp);
    }

    pub fn assert_parse_error(inp: &str) {
        let mut registry = Registry::new();
        let source = registry
            .add(&mut BufferSource::new(inp, "datum-parser-test"))
            .unwrap();
        let reader = Reader::new();
        let datum_ast = reader.parse(&source).unwrap();
        let mut parser = Parser::new();
        let parse_result = parser.parse(&datum_ast);

        assert!(
            parse_result.is_err(),
            "expected parser error but got something different"
        )
    }
}
