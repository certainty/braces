use crate::compiler::frontend::reader::sexp::datum::Datum;
use crate::compiler::source::{HasSourceLocation, Location};

use super::define::DefinitionExpression;
use super::frontend::error::Error;
use super::Expression;
use super::Result;
use crate::compiler::frontend::error::Detail;
use crate::compiler::frontend::parser::core_parser::CoreParser;

#[derive(Clone, PartialEq, Debug)]
pub struct BodyExpression {
    pub definitions: Vec<DefinitionExpression>,
    pub sequence: Vec<Expression>,
}

impl From<&Expression> for BodyExpression {
    fn from(expr: &Expression) -> Self {
        Self {
            definitions: vec![],
            sequence: vec![expr.clone()],
        }
    }
}

impl From<Vec<Expression>> for BodyExpression {
    fn from(exprs: Vec<Expression>) -> Self {
        Self {
            definitions: vec![],
            sequence: exprs,
        }
    }
}

impl From<DefinitionExpression> for BodyExpression {
    fn from(expr: DefinitionExpression) -> Self {
        Self {
            definitions: vec![expr],
            sequence: vec![],
        }
    }
}

impl HasSourceLocation for BodyExpression {
    fn source_location(&self) -> &Location {
        match (self.definitions.first(), self.sequence.first()) {
            (Some(def), _) => def.source_location(),
            (_, Some(seq)) => seq.source_location(),
            _ => panic!("Could not determine source location"),
        }
    }
}

impl Expression {
    pub fn body(sequence: Vec<Expression>) -> BodyExpression {
        BodyExpression::from(sequence)
    }

    /// Create body expression, which is used in expressions introducing new
    /// scopes like <let>, <begin> and <lambda>
    pub fn to_body_expression(&self) -> BodyExpression {
        BodyExpression::from(self)
    }
}

/// Parse a body
///
/// Ref: r7rs 7.1.3
///
/// ```grammar
/// <body>         -> <definition>* <sequence>
/// <sequence>     -> <command>* <expression>
/// <command>      -> <expression>
/// ```

impl CoreParser {
    pub fn parse_body(&mut self, datum: &[Datum], loc: &Location) -> Result<BodyExpression> {
        let mut definitions: Vec<DefinitionExpression> = vec![];
        let mut iter = datum.iter();
        let mut cur = iter.next();

        // parse definitions*
        while cur.is_some() {
            match self.do_parse_definition(cur.unwrap()) {
                Ok(expr) => {
                    // TODO: register binding of internal define in parsing environment
                    definitions.push(expr);
                    cur = iter.next();
                }
                Err(_) => break,
            }
        }

        // nothing left to parse
        if cur.is_none() {
            return Err(Error::parse_error(
                "Invalid body definition. Expected (<definition>* sequence)",
                Detail::new("", loc.clone()),
                vec![],
            ));
        }

        //parse the rest as sequence
        let mut sequence = vec![self.parse(cur.unwrap())?];
        let rest: Result<Vec<Expression>> = iter.map(|e| self.parse(e)).collect();
        sequence.extend(rest?);

        Ok(BodyExpression {
            definitions,
            sequence,
        })
    }
}
