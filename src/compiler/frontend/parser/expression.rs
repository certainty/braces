pub mod apply;
pub mod body;
pub mod conditional;
pub mod define;
pub mod error;
pub mod identifier;
pub mod lambda;
pub mod letexp;
pub mod literal;
pub mod quotation;
pub mod set;
use self::{conditional::IfExpression, quotation::QuotationExpression, set::SetExpression};
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
use letexp::{BindingSpec, LetExpression};
use literal::LiteralExpression;

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier),
    Quotation(QuotationExpression),
    Literal(LiteralExpression),
    Define(DefinitionExpression),
    Lambda(LambdaExpression),
    Assign(SetExpression),
    Let(LetExpression),
    If(IfExpression),
    Apply(ApplicationExpression),
    Command(Box<Expression>, SourceLocation),
    Begin(Box<Expression>, Vec<Box<Expression>>, SourceLocation),
}

impl HasSourceLocation for Expression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            Self::Identifier(id) => id.source_location(),
            Self::Literal(exp) => exp.source_location(),
            Self::Quotation(exp) => exp.source_location(),
            Self::Assign(exp) => exp.source_location(),
            Self::Define(def) => def.source_location(),
            Self::Let(exp) => exp.source_location(),
            Self::If(expr) => expr.source_location(),
            Self::Lambda(proc) => proc.source_location(),
            Self::Apply(exp) => exp.source_location(),
            Self::Command(_, loc) => &loc,
            Self::Begin(_, _, loc) => &loc,
        }
    }
}

impl Expression {
    pub fn parse_program<T: Source>(source: &mut T) -> Result<Vec<Self>> {
        let ast = Parser.parse_datum_sequence(source)?;
        ast.iter().map(Self::parse_expression).collect()
    }

    /// Parse a single datum into a an expression.
    ///
    /// It either succeeds or returns an error indicating
    /// what went wrong.
    pub fn parse_one<T: Source>(source: &mut T) -> Result<Self> {
        let ast = Parser.parse_datum(source)?;
        Self::parse_expression(&ast)
    }

    pub fn constant(datum: Datum) -> Expression {
        Expression::Literal(literal::build(datum))
    }

    pub fn quoted_value(datum: Datum) -> Expression {
        Expression::Quotation(quotation::build_quote(datum))
    }

    pub fn assign(id: Identifier, expr: Expression, loc: SourceLocation) -> Expression {
        Expression::Assign(set::build(id, expr, loc))
    }

    pub fn lambda(
        formals: lambda::Formals,
        body: BodyExpression,
        loc: SourceLocation,
    ) -> Expression {
        Expression::Lambda(lambda::build(formals, body, loc))
    }

    pub fn define(id: Identifier, expr: Expression, loc: SourceLocation) -> Expression {
        Expression::Define(define::build_simple(id, expr, loc))
    }

    pub fn begin(first: Expression, rest: Vec<Expression>, loc: SourceLocation) -> Expression {
        Expression::Begin(
            Box::new(first),
            rest.iter().map(|e| Box::new(e.clone())).collect(),
            loc,
        )
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

    /// Create and expression for core-let
    pub fn let_bind(
        bindings: Vec<BindingSpec>,
        body: BodyExpression,
        loc: SourceLocation,
    ) -> Expression {
        Expression::Let(letexp::build_let(bindings, body, loc))
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
    fn parse_expression(datum: &Datum) -> Result<Expression> {
        //let etest = literal::parse(datum).or_else(|_| quotation::parse(datum));

        match datum.sexp() {
            Sexp::Symbol(s) => Ok(Self::identifier(s.to_string(), datum.location.clone())),
            Sexp::Bool(_) => Ok(Self::constant(datum.clone())),
            Sexp::Char(_) => Ok(Self::constant(datum.clone())),
            Sexp::String(_) => Ok(Self::constant(datum.clone())),
            Sexp::List(ls) => match Self::head_symbol(&ls) {
                Some("set!") => set::parse(datum),
                Some("quote") => quotation::parse(datum),
                Some("if") => conditional::parse(datum),
                Some("let") => letexp::parse(datum),
                Some("lambda") => lambda::parse(datum),
                Some("begin") => Self::parse_begin(&ls, datum.location.clone()),
                Some("define") => define::parse(datum),
                other => apply::parse(datum),
            },
            _ => todo!(),
        }
    }

    fn parse_begin(exprs: &Vec<Datum>, loc: SourceLocation) -> Result<Expression> {
        match &exprs[..] {
            [_, first, rest @ ..] => {
                let parsed_first = Self::parse_command_or_definition(first).map(Box::new);
                let parsed_exprs: Result<Vec<Box<Expression>>> = rest
                    .iter()
                    .map(|e| Self::parse_command_or_definition(e).map(Box::new))
                    .collect();

                Ok(Expression::Begin(parsed_first?, parsed_exprs?, loc))
            }
            _ => Error::parse_error("Expected (define <command-or-definition+>)", loc),
        }
    }

    fn parse_command_or_definition(datum: &Datum) -> Result<Expression> {
        define::parse(datum).or_else(|_| Self::parse_expression(datum))
    }

    fn head_symbol<'a>(ls: &'a Vec<Datum>) -> Option<&'a str> {
        match ls.first().map(|e| e.sexp()) {
            Some(Sexp::Symbol(s)) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn apply_special<'a>(datum: &'a Datum) -> Option<(&'a str, &'a [Datum])> {
        match datum.sexp() {
            Sexp::List(ls) => match Self::head_symbol(ls) {
                Some(s) => Some((&s, &ls[1..])),
                _ => None,
            },
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::{SourceType, StringSource};

    #[test]
    fn test_parse_begin() {
        assert_parse_as(
            "(begin #t)",
            Expression::begin(
                Expression::constant(make_datum(Sexp::Bool(true), 1, 8)),
                vec![],
                location(1, 1),
            ),
        )
    }

    pub fn assert_parse_as(inp: &str, exp: Expression) {
        let mut source = StringSource::new(inp, "datum-parser-test");
        let parsed_exp = Expression::parse_one(&mut source).unwrap();

        assert_eq!(parsed_exp, exp)
    }

    pub fn assert_parse_error(inp: &str) {
        let mut source = StringSource::new(inp, "datum-parser-test");

        assert!(
            Expression::parse_one(&mut source).is_err(),
            "expected parse error"
        )
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
