pub mod apply;
pub mod assignment;
pub mod body;
pub mod conditional;
pub mod define;
pub mod error;
pub mod identifier;
pub mod lambda;
pub mod letexp;
pub mod literal;
pub mod quotation;
pub mod sequence;
use std::iter::FromIterator;

use self::{assignment::SetExpression, conditional::IfExpression, quotation::QuotationExpression};
use crate::compiler::frontend::parser::{
    sexp::datum::{Datum, Sexp},
    Parser,
};
use crate::compiler::source::Source;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use apply::ApplicationExpression;
use body::BodyExpression;
use define::DefinitionExpression;
use error::Error;
use identifier::Identifier;
use lambda::LambdaExpression;
use literal::LiteralExpression;
use sequence::BeginExpression;

type Result<T> = std::result::Result<T, Error>;

pub enum ParseResult<T> {
    Applicable(Result<T>),
    NonApplicable(String, SourceLocation),
}

impl<T> ParseResult<T> {
    pub fn collect_res(results: Vec<ParseResult<T>>) -> Vec<Result<T>> {
        let mut total = vec![];

        for res in results {
            match res {
                ParseResult::Applicable(res) => total.push(res),
                _ => (),
            }
        }

        total
    }

    pub fn accept(v: T) -> ParseResult<T> {
        ParseResult::Applicable(Ok(v))
    }

    pub fn error(e: Error) -> ParseResult<T> {
        ParseResult::Applicable(Err(e))
    }

    pub fn ignore<S: Into<String>>(message: S, location: SourceLocation) -> ParseResult<T> {
        ParseResult::NonApplicable(message.into(), location)
    }

    pub fn or<F: FnOnce() -> ParseResult<T>>(self, op: F) -> ParseResult<T> {
        match self {
            Self::NonApplicable(_, _) => op(),
            other => other,
        }
    }

    pub fn and<F: FnOnce() -> ParseResult<T>>(self, op: F) -> ParseResult<T> {
        match self {
            Self::Applicable(_ignored) => op(),
            other => other,
        }
    }

    pub fn is_err(self) -> bool {
        match self {
            Self::NonApplicable(_, _) => false,
            Self::Applicable(res) => res.is_err(),
        }
    }

    pub fn is_ok(self) -> bool {
        match self {
            Self::NonApplicable(_, _) => false,
            Self::Applicable(res) => res.is_ok(),
        }
    }

    pub fn map<R, F: FnOnce(T) -> R>(self, op: F) -> ParseResult<R> {
        match self {
            Self::Applicable(v) => ParseResult::<R>::Applicable(v.map(op)),
            Self::NonApplicable(m, l) => ParseResult::<R>::NonApplicable(m, l),
        }
    }

    pub fn flat_map<F: FnOnce(T) -> ParseResult<T>>(self, op: F) -> ParseResult<T> {
        match self {
            Self::Applicable(Ok(v)) => op(v),
            other => other,
        }
    }

    pub fn res(self) -> Result<T> {
        match self {
            Self::NonApplicable(message, location) => Error::parse_error(&message, location),
            Self::Applicable(res) => res,
        }
    }

    #[inline]
    pub fn is_applicable(self) -> bool {
        match self {
            Self::Applicable(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_non_applicable(self) -> bool {
        !self.is_applicable()
    }

    pub fn map_non_applicable(self, v: Result<T>) -> Result<T> {
        match self {
            Self::NonApplicable(_, _) => v,
            Self::Applicable(other) => other,
        }
    }
}

impl<T> From<Result<T>> for ParseResult<T> {
    fn from(value: Result<T>) -> Self {
        Self::Applicable(value)
    }
}

impl<T> From<T> for ParseResult<T> {
    fn from(value: T) -> Self {
        Self::Applicable(Ok(value))
    }
}

impl<T> From<ParseResult<T>> for Result<T> {
    fn from(res: ParseResult<T>) -> Self {
        res.res()
    }
}

impl<T> FromIterator<ParseResult<T>> for Result<Vec<T>> {
    fn from_iter<I: IntoIterator<Item = ParseResult<T>>>(iter: I) -> Self {
        iter.into_iter().map(|i| i.res()).collect()
    }
}

impl<T> FromIterator<ParseResult<T>> for Vec<Result<T>> {
    fn from_iter<I: IntoIterator<Item = ParseResult<T>>>(iter: I) -> Self {
        iter.into_iter().map(|i| i.res()).collect()
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier),
    Quotation(QuotationExpression),
    Literal(LiteralExpression),
    Define(DefinitionExpression),
    Lambda(LambdaExpression),
    Assign(SetExpression),
    If(IfExpression),
    Apply(ApplicationExpression),
    Command(Box<Expression>),
    Begin(BeginExpression),
}

impl HasSourceLocation for Expression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
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

impl Expression {
    pub fn parse_program<T: Source>(source: &mut T) -> Result<Vec<Self>> {
        let ast = Parser.parse_datum_sequence(source)?;
        //println!("{:?}", ast);
        ast.iter().map(Self::parse).collect()
    }

    /// Parse a single datum into a an expression.
    ///
    /// It either succeeds or returns an error indicating
    /// what went wrong.
    pub fn parse_one<T: Source>(source: &mut T) -> Result<Self> {
        let ast = Parser.parse_datum(source)?;
        Self::parse(&ast)
    }

    pub fn constant(datum: Datum) -> Expression {
        Expression::Literal(literal::build(datum))
    }

    pub fn quoted_value(datum: Datum) -> Expression {
        Expression::Quotation(quotation::build_quote(datum))
    }

    pub fn assign(id: Identifier, expr: Expression, loc: SourceLocation) -> Expression {
        Expression::Assign(assignment::build(id, expr, loc))
    }

    pub fn lambda(
        formals: lambda::Formals,
        body: BodyExpression,
        label: Option<String>,
        loc: SourceLocation,
    ) -> Expression {
        Expression::Lambda(lambda::build(formals, body, label, loc))
    }

    pub fn define(id: Identifier, expr: Expression, loc: SourceLocation) -> Expression {
        Expression::Define(define::build_simple(id, expr, loc))
    }

    pub fn begin(first: Expression, rest: Vec<Expression>, loc: SourceLocation) -> Expression {
        Expression::Begin(sequence::build(first, rest, loc))
    }

    pub fn identifier(str: String, loc: SourceLocation) -> Expression {
        Expression::Identifier(Identifier::new(str, loc))
    }

    pub fn body(sequence: Vec<Expression>) -> BodyExpression {
        BodyExpression::from(sequence)
    }

    pub fn apply(
        operator: Expression,
        operands: Vec<Expression>,
        loc: SourceLocation,
    ) -> Expression {
        Expression::Apply(apply::build(operator, operands, loc))
    }

    /// Create body expression, which is used in expressions introducing new
    /// scopes like <let>, <begin> and <lambda>
    pub fn to_body_expression(&self) -> BodyExpression {
        BodyExpression::from(self)
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

    fn parse(datum: &Datum) -> Result<Expression> {
        identifier::parse(datum)
            .or(|| literal::parse(datum))
            .or(|| lambda::parse(datum))
            .or(|| assignment::parse(datum))
            .or(|| quotation::parse(datum))
            .or(|| conditional::parse(datum))
            .or(|| Self::parse_derived(datum))
            .or(|| apply::parse(datum))
            .map_non_applicable(Error::parse_error(
                "Invalid expression",
                datum.source_location().clone(),
            ))
    }

    fn parse_derived(datum: &Datum) -> ParseResult<Expression> {
        sequence::parse(datum).or(|| define::parse(datum))
    }

    pub fn parse_apply_special<'a, T, F>(
        datum: &'a Datum,
        operator: &'a str,
        parse: F,
    ) -> ParseResult<T>
    where
        F: FnOnce(&'a str, &'a [Datum], &'a SourceLocation) -> Result<T>,
    {
        match datum.sexp() {
            Sexp::List(ls) => match Self::head_symbol(ls) {
                Some(s) if s == operator => parse(s, &ls[1..], datum.source_location()).into(),
                _ => ParseResult::ignore(
                    "Expected (<special> <operands>*)",
                    datum.source_location().clone(),
                ),
            },
            _ => ParseResult::ignore(
                "Expected (<special> <operands>*)",
                datum.source_location().clone(),
            ),
        }
    }

    fn head_symbol<'a>(ls: &'a Vec<Datum>) -> Option<&'a str> {
        match ls.first().map(|e| e.sexp()) {
            Some(Sexp::Symbol(s)) => Some(s.as_str()),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::{SourceType, StringSource};

    #[test]
    fn test_parse_result_collect_err() {
        let res1: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(12));
        let res3: ParseResult<u32> =
            ParseResult::Applicable(Error::parse_error("couldn't parse", location(0, 1)));
        let res4: ParseResult<u32> = ParseResult::Applicable(Ok(20));
        let total: Result<Vec<u32>> = vec![res1, res2, res3, res4].into_iter().collect();

        assert!(total.is_err(), "expected error")
    }

    #[test]
    fn test_parse_result_collect_ok() {
        let res1: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(12));
        let res4: ParseResult<u32> = ParseResult::Applicable(Ok(20));
        let total: Result<Vec<u32>> = vec![res1, res2, res4].into_iter().collect();

        assert_eq!(total.unwrap(), vec![10, 12, 20])
    }

    #[test]
    fn test_parse_result_predicates() {
        let res: ParseResult<u32> = ParseResult::ignore("test", location(0, 1));
        assert!(res.is_non_applicable(), "Expected non-applicable");

        let res: ParseResult<u32> = ParseResult::ignore("test", location(0, 1));
        assert!(!res.is_applicable(), "Expected non-applicable");

        let res: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert!(res.is_applicable(), "Expected applicable");

        let res: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert!(!res.is_non_applicable(), "Expected applicable");
    }

    #[test]
    fn test_parse_result_and() {
        let res1: ParseResult<u32> = ParseResult::ignore("test", location(0, 1));
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert!(
            res1.and(|| res2).is_non_applicable(),
            "Expected non-applicable"
        );

        let res1: ParseResult<u32> = ParseResult::ignore("test", location(0, 1));
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert!(
            res2.and(|| res1).is_non_applicable(),
            "Expected non-applicable"
        );

        let res1: ParseResult<u32> = ParseResult::Applicable(Ok(5));
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert_eq!(res1.and(|| res2).res().unwrap(), 10)
    }

    #[test]
    fn test_parse_result_or() {
        let res1: ParseResult<u32> = ParseResult::ignore("test", location(0, 1));
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert_eq!(res1.or(|| res2).res().unwrap(), 10);

        let res1: ParseResult<u32> = ParseResult::ignore("test", location(0, 1));
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert_eq!(res1.or(|| res2).res().unwrap(), 10);

        let res1: ParseResult<u32> = ParseResult::ignore("test", location(0, 1));
        let res2: ParseResult<u32> = ParseResult::ignore("test", location(0, 1));

        assert!(
            res1.or(|| res2).is_non_applicable(),
            "Expected non-applicable parse result"
        )
    }

    pub fn assert_parse_as(inp: &str, exp: Expression) {
        let mut source = StringSource::new(inp, "datum-parser-test");
        let parsed_exp = Expression::parse_one(&mut source).unwrap();

        assert_eq!(parsed_exp, exp)
    }

    pub fn assert_parse_error(inp: &str) {
        let mut source = StringSource::new(inp, "datum-parser-test");
        let result = Expression::parse_one(&mut source);
        let message = format!("expected parse error but got {:?}", result);

        assert!(result.is_err(), message)
    }

    pub fn location(line: usize, col: usize) -> SourceLocation {
        SourceLocation::new(
            SourceType::Buffer("datum-parser-test".to_string()),
            line,
            col,
        )
    }

    pub fn make_datum(sexp: Sexp, line: usize, col: usize) -> Datum {
        Datum::new(sexp, location(line, col))
    }
}
