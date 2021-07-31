use super::define;
use super::define::DefinitionExpression;
use super::Error;
use super::Expression;
use super::Result;
use crate::compiler::frontend::ParserContext;
use crate::compiler::source_location::SourceLocation;
use crate::compiler::{frontend::reader::sexp::datum::Datum, source_location::HasSourceLocation};

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
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match (self.definitions.first(), self.sequence.first()) {
            (Some(def), _) => def.source_location(),
            (_, Some(seq)) => seq.source_location(),
            _ => panic!("Could not determine source location"),
        }
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
pub fn parse(
    datum: &[Datum],
    loc: &SourceLocation,
    ctx: &mut ParserContext,
) -> Result<BodyExpression> {
    let mut definitions: Vec<DefinitionExpression> = vec![];
    let mut iter = datum.iter();
    let mut cur = iter.next();

    // parse definitions*
    while cur.is_some() {
        match define::parse_definition(cur.unwrap(), ctx).res() {
            Ok(expr) => {
                definitions.push(expr);
                cur = iter.next();
            }
            Err(_) => break,
        }
    }

    // nothing left to parse
    if cur.is_none() {
        return Error::parse_error(
            "Invalid body definition. Expected (<definition>* sequence)",
            loc.clone(),
        );
    }

    //parse the rest as sequence
    let mut sequence = vec![Expression::parse(cur.unwrap(), ctx)?];
    let rest: Result<Vec<Expression>> = iter.map(|i| Expression::parse(i, ctx)).collect();
    sequence.extend(rest?);

    Ok(BodyExpression {
        definitions,
        sequence,
    })
}
