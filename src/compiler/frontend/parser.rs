pub mod apply;
pub mod assignment;
pub mod body;
pub mod conditional;
pub mod define;
pub mod identifier;
pub mod lambda;
pub mod literal;
pub mod quotation;
pub mod result;
pub mod sequence;

use super::error::Error;
use super::reader::sexp::datum::{Datum, Sexp};
use super::syntax::{self, environment::Denotation};
pub use super::Result;
use crate::compiler::frontend;
use crate::compiler::representation::{CoreAST, SexpAST};
use crate::compiler::source::{HasSourceLocation, Location};

use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::expander::Expander;
use crate::compiler::frontend::syntax::environment::Special;
pub use result::ParseResult;

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
    environment: syntax::environment::SyntaxEnvironment,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            expander: Expander::new(),
            environment: syntax::environment::SyntaxEnvironment::basic(),
        }
    }

    pub fn parse(&mut self, ast: &SexpAST) -> Result<CoreAST> {
        let expressions: Result<Vec<Expression>> =
            ast.to_vec().iter().map(|d| self.do_parse(d)).collect();

        Ok(CoreAST::new(expressions?))
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

    fn do_parse(&mut self, datum: &Datum) -> Result<Expression> {
        //let datum = self.expander.expand(datum)?;
        match datum.sexp() {
            Sexp::List(ls) => match &ls[..] {
                [operator, _operands @ ..] if operator.is_symbol() => {
                    let denotation = self.denotation_of(operator)?;
                    match denotation {
                        Denotation::Special(special) => self.parse_special(special, &datum),
                        Denotation::Global(_) => self.parse_apply(&datum).res(),
                        Denotation::Id(_) => self.parse_apply(&datum).res(),
                        _ => {
                            return Err(Error::bug(&format!(
                                "Unexpected denotation for datum: {:?}",
                                denotation.clone()
                            )))
                        }
                    }
                }
                [_operator, _operands @ ..] => self.parse_apply(&datum).res(),
                _ => Err(Error::parse_error(
                    "Unexpected unquoted list",
                    Detail::new("found unquoted list", datum.source_location().clone()),
                    vec![],
                )),
            },
            sexp if sexp.is_symbol() => self.parse_identifier(&datum).res(),
            _lit => self.parse_literal(&datum).res(),
        }
    }

    fn parse_special(&mut self, special_form: Special, datum: &Datum) -> Result<Expression> {
        match special_form {
            Special::Define => self.parse_definition(&datum).res(),
            Special::Quote => self.parse_quote(&datum).res(),
            Special::QuasiQuote => self.parse_quote(&datum).res(),
            Special::Lambda => self.parse_lambda(&datum).res(),
            Special::Set => self.parse_set(&datum).res(),
            Special::Begin => self.parse_begin(&datum).res(),
            Special::If => self.parse_if(&datum).res(),
            _ => {
                return Err(Error::bug(&format!(
                    "Unexpected special form: {:?}",
                    special_form
                )))
            }
        }
    }

    pub fn denotation_of(&mut self, datum: &Datum) -> Result<Denotation> {
        if let Sexp::Symbol(sym) = datum.sexp() {
            Ok(self.environment.get(&sym.clone().into()))
        } else {
            Err(Error::bug("unexpected symbol to determine denotation"))
        }
    }

    pub fn parse_list<'a>(&mut self, datum: &'a Datum) -> Result<&'a [Datum]> {
        log::trace!("trying to parse list: {:?}", datum);
        match datum.sexp() {
            Sexp::List(ls) => Ok(&ls[..]),
            _ => Err(Error::parse_error(
                "Expected list",
                Detail::new("expected list", datum.source_location().clone()),
                vec![],
            )),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::compiler::frontend::reader::Reader;
    use crate::compiler::source::{BufferSource, Registry, SourceId};
    use std::ops::Range;

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
