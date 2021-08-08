pub mod apply;
pub mod assignment;
pub mod body;
pub mod conditional;
pub mod define;
pub mod identifier;
pub mod lambda;
pub mod letexp;
pub mod literal;
pub mod parse_result;
pub mod quotation;
pub mod sequence;

use self::{assignment::SetExpression, conditional::IfExpression, quotation::QuotationExpression};
use super::Error;
use super::ParserContext;
use crate::compiler::frontend::parser::syntax::Symbol;
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use apply::ApplicationExpression;
use body::BodyExpression;
use define::DefinitionExpression;
use identifier::Identifier;
use lambda::LambdaExpression;
use literal::LiteralExpression;
use parse_result::ParseResult;
use sequence::BeginExpression;

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, PartialEq, Debug)]
pub struct MacroUseExpression {
    name: Identifier,
    sexps: Vec<Datum>,
    source_location: SourceLocation,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier),
    // TODO: remove?
    Quotation(QuotationExpression),
    Literal(LiteralExpression),
    Define(DefinitionExpression),
    Lambda(LambdaExpression),
    Assign(SetExpression),
    If(IfExpression),
    Apply(ApplicationExpression),
    // TODO: remove?
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

    pub fn conditional(
        test: Expression,
        consequent: Expression,
        alternate: Option<Expression>,
        loc: SourceLocation,
    ) -> Expression {
        Expression::If(conditional::build(test, consequent, alternate, loc))
    }

    pub fn identifier<T: Into<String>>(str: T, loc: SourceLocation) -> Expression {
        Expression::Identifier(Identifier::new(Symbol::forged(str.into()), loc))
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
    //    <definition>         |
    ///   <conditional>        |
    ///   <assignment>         |
    /// ```

    pub fn parse_all(data: Vec<Datum>, ctx: &mut ParserContext) -> Result<Vec<Expression>> {
        data.iter().map(|i| Self::parse(i, ctx)).collect()
    }

    pub fn parse(datum: &Datum, ctx: &mut ParserContext) -> Result<Expression> {
        identifier::parse(datum)
            .or(|| literal::parse(datum))
            .or(|| quotation::parse(datum, ctx))
            .or(|| lambda::parse(datum, ctx))
            .or(|| assignment::parse(datum, ctx))
            .or(|| conditional::parse(datum, ctx))
            .or(|| define::parse(datum, ctx))
            .or(|| sequence::parse(datum, ctx))
            .map_non_applicable(Error::parse_error(
                "Invalid expression",
                datum.source_location().clone(),
            ))
    }

    pub fn parse_lambda(
        datum: &Datum,
        ctx: &mut ParserContext,
    ) -> Result<lambda::LambdaExpression> {
        let expr = Self::parse(datum, ctx)?;
        if let Expression::Lambda(lambda_expr) = expr {
            Ok(lambda_expr)
        } else {
            Error::parse_error(
                "Expected datum to parse as lambda",
                datum.source_location().clone(),
            )
        }
    }

    pub fn parse_apply_special<'a, T, F>(
        datum: &'a Datum,
        operator: &'a str,
        ctx: &mut ParserContext,
        parse: F,
    ) -> ParseResult<T>
    where
        F: FnOnce(&'a str, &'a [Datum], &'a SourceLocation, &mut ParserContext) -> Result<T>,
    {
        match datum.sexp() {
            Sexp::List(ls) => match Self::head_symbol(ls) {
                Some(s) if s == operator => parse(s, &ls[1..], datum.source_location(), ctx).into(),
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
    use crate::compiler::frontend;
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
        let ast = frontend::parse(&mut source).unwrap();

        assert_eq!(ast.singleton().unwrap(), &exp)
    }

    pub fn assert_parse_error(inp: &str) {
        let mut source = StringSource::new(inp, "datum-parser-test");
        let result = frontend::parse(&mut source);
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
