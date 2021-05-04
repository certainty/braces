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
pub mod sequence;
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
use rustc_hash::FxHashSet;
use sequence::BeginExpression;

type Result<T> = std::result::Result<T, Error>;

lazy_static! {
    pub static ref SPECIAL_OPERATORS: FxHashSet<&'static str> = {
        let mut set = FxHashSet::default();
        set.insert("set!");
        set.insert("lambda");
        set.insert("define");
        set.insert("if");
        set.insert("cond");
        set.insert("let");
        set.insert("let*");
        set.insert("letrec");
        set.insert("letrec");
        set.insert("let-values");
        set.insert("values");
        set.insert("define-values");
        set.insert("begin");
        set
    };
}

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
            Self::Let(exp) => exp.source_location(),
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

    #[inline]
    pub fn is_special_operator(op: &Expression) -> bool {
        if let Expression::Identifier(id) = &op {
            let name: &str = &id.string();
            SPECIAL_OPERATORS.contains(name)
        } else {
            false
        }
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
        identifier::parse(datum)
            .or_else(|_| literal::parse(datum))
            .or_else(|_| lambda::parse(datum))
            .or_else(|_| set::parse(datum))
            .or_else(|_| quotation::parse(datum))
            .or_else(|_| conditional::parse(datum))
            .or_else(|_| letexp::parse(datum))
            .or_else(|_| sequence::parse(datum))
            .or_else(|_| define::parse(datum))
            .or_else(|_| apply::parse(datum))
            .or_else(|e| Err(e))
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
