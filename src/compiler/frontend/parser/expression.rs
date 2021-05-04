pub mod body;
pub mod conditional;
pub mod error;
pub mod identifier;
pub mod lambda;
pub mod quotation;
use self::conditional::IfExpression;
use crate::compiler::frontend::parser::{
    sexp::datum::{Datum, Sexp},
    Parser,
};
use crate::compiler::source::Source;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use body::BodyExpression;
use error::Error;
use identifier::Identifier;

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier),
    Literal(LiteralExpression),
    Assign(Identifier, Box<Expression>, SourceLocation),
    Define(DefinitionExpression),
    Let(LetExpression, SourceLocation),
    If(IfExpression),
    Lambda(lambda::LambdaExpression),
    Apply(Box<Expression>, Vec<Box<Expression>>, SourceLocation),
    Command(Box<Expression>, SourceLocation),
    Begin(Box<Expression>, Vec<Box<Expression>>, SourceLocation),
}

impl HasSourceLocation for Expression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            Self::Identifier(id) => id.source_location(),
            Self::Literal(LiteralExpression::SelfEvaluating(datum)) => &datum.location,
            Self::Literal(LiteralExpression::Quotation(datum)) => &datum.location,
            Self::Assign(_, _expr, loc) => &loc,
            Self::Define(def) => def.source_location(),
            Self::Let(_, loc) => &loc,
            Self::If(expr) => expr.source_location(),
            Self::Lambda(proc) => proc.source_location(),
            Self::Apply(_, _, loc) => &loc,
            Self::Command(_, loc) => &loc,
            Self::Begin(_, _, loc) => &loc,
        }
    }
}

pub type BindingSpec = (Identifier, Expression);

#[derive(Clone, PartialEq, Debug)]
pub enum LetExpression {
    Let(Vec<BindingSpec>, Box<BodyExpression>),
}
#[derive(Clone, PartialEq, Debug)]
pub enum DefinitionExpression {
    DefineSimple(Identifier, Box<Expression>),
    Begin(Vec<Box<DefinitionExpression>>),
}

impl HasSourceLocation for DefinitionExpression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            DefinitionExpression::DefineSimple(_, exp) => exp.source_location(),
            DefinitionExpression::Begin(exprs) => exprs.first().unwrap().source_location(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum LiteralExpression {
    SelfEvaluating(Datum),
    Quotation(Datum),
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

    /// Create a constant expression
    pub fn constant(datum: &Datum) -> Expression {
        Expression::Literal(LiteralExpression::SelfEvaluating(datum.clone()))
    }

    /// Create a quotation expression
    ///
    /// Quoted values are special in the sense that they maintain a reference
    /// to the quote `Datum`. They're treated as unevaluated expressions.
    pub fn quoted_value(datum: &Datum) -> Expression {
        Expression::Literal(LiteralExpression::Quotation(datum.clone()))
    }

    /// Create an assignment expression
    pub fn assign(id: Identifier, expr: Expression, loc: SourceLocation) -> Expression {
        Expression::Assign(id, Box::new(expr), loc)
    }

    pub fn lambda(
        formals: lambda::Formals,
        body: BodyExpression,
        loc: SourceLocation,
    ) -> Expression {
        Expression::Lambda(lambda::LambdaExpression::new(formals, body, loc))
    }

    pub fn define(id: Identifier, expr: Expression, _loc: SourceLocation) -> Expression {
        Expression::Define(DefinitionExpression::DefineSimple(id, Box::new(expr)))
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
        Expression::Apply(
            Box::new(operator),
            operands.iter().map(|e| Box::new(e.clone())).collect(),
            loc,
        )
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
        Expression::Let(LetExpression::Let(bindings, Box::new(body)), loc)
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
        match datum.sexp() {
            Sexp::Symbol(s) => Ok(Self::identifier(s.to_string(), datum.location.clone())),
            Sexp::Bool(_) => Ok(Self::constant(datum)),
            Sexp::Char(_) => Ok(Self::constant(datum)),
            Sexp::String(_) => Ok(Self::constant(datum)),
            Sexp::List(ls) => match Self::head_symbol(&ls) {
                Some("quote") => quotation::parse(datum),
                Some("set!") => Self::parse_assignment(&ls, &datum.location),
                Some("if") => Ok(Expression::If(conditional::parse(datum)?)),
                Some("let") => Self::parse_let(&ls, &datum.location),
                Some("lambda") => Ok(Expression::Lambda(lambda::parse(datum)?)),
                Some("begin") => Self::parse_begin(&ls, datum.location.clone()),
                Some("define") => Ok(Expression::Define(Self::parse_definition(&datum)?)),
                other => Self::parse_apply(&ls, &datum.location),
            },
            _ => todo!(),
        }
    }

    fn parse_apply(ls: &Vec<Datum>, loc: &SourceLocation) -> Result<Expression> {
        match &ls[..] {
            [operator, operands @ ..] => {
                let operatored_expr = Self::parse_expression(&operator);
                let operands_expr: Result<Vec<Expression>> =
                    operands.iter().map(Self::parse_expression).collect();

                Ok(Expression::apply(
                    operatored_expr?,
                    operands_expr?,
                    loc.clone(),
                ))
            }
            _ => Error::parse_error("expected (<operator> <operand>*)", loc.clone()),
        }
    }

    /// Parse a set! expression
    ///
    /// Ref: r7rs 7.1.3
    ///
    /// ```grammar
    /// <assignment> -> (set! <IDENTIFIER> <expression>)
    /// ```
    fn parse_assignment(ls: &Vec<Datum>, loc: &SourceLocation) -> Result<Expression> {
        match &ls[..] {
            [_, identifier, expr] => Ok(Expression::assign(
                identifier::parse(identifier)?,
                Self::parse_expression(expr)?,
                loc.clone(),
            )),
            _other => Error::parse_error("Expected (set! <identifier> <expression>)", loc.clone()),
        }
    }

    /// Parse a let expression
    ///
    /// Ref: r7rs 7.1.3 (derived expression)
    ///
    /// ```grammar
    /// <derived expression> ->
    ///   (let <IDENTIFIER> (<binding spec>*) <body>)
    ///
    /// <binding spec> -> (<IDENTIFIER> <expression>)
    /// <body>         -> <definition>* <sequence>
    /// <sequence>     -> <command>* <expression>
    /// <command>      -> <expression>
    ///
    /// ```
    fn parse_let(ls: &Vec<Datum>, loc: &SourceLocation) -> Result<Expression> {
        match &ls[..] {
            [_, binding_spec, body @ ..] => Ok(Expression::let_bind(
                Self::parse_binding_specs(binding_spec)?,
                Self::parse_body(body, loc)?,
                loc.clone(),
            )),
            [_, _name, _binding_spec, _body @ ..] => {
                Error::parse_error("Named let not yet supported", loc.clone())
            }
            _other => Error::parse_error(
                "Expected (let (<bindings>*) body) or (let name (<bindings*>) body)",
                loc.clone(),
            ),
        }
    }
    /// Parse a let expression
    ///
    /// Ref: r7rs 7.1.3 (derived expression)
    ///
    /// ```grammar
    /// <binding spec> -> (<IDENTIFIER> <expression>)
    /// ```
    fn parse_binding_specs(datum: &Datum) -> Result<Vec<BindingSpec>> {
        match datum.sexp() {
            Sexp::List(ls) => ls.iter().map(Self::parse_binding_spec).collect(),
            _ => Error::parse_error("Expected list of binding specs", datum.location.clone()),
        }
    }

    fn parse_binding_spec(datum: &Datum) -> Result<BindingSpec> {
        match datum.sexp() {
            Sexp::List(ls) => match &ls[..] {
                [identifier, expr] => Ok((identifier::parse(identifier)?, Self::parse_expression(expr)?)),
                _ => Error::parse_error(
                    "Expected list of exactly two elements for binding. (<identifier> <expression>)",
                    datum.location.clone(),
                ),
            },
            _ => Error::parse_error(
                "Expected list of exactly two elements for binding. (<identifier> <expression>)",
                datum.location.clone()
            )
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
    fn parse_body(datum: &[Datum], loc: &SourceLocation) -> Result<BodyExpression> {
        let mut definitions: Vec<DefinitionExpression> = vec![];
        let mut iter = datum.iter();
        let mut cur = iter.next();

        // parse definitions*
        while cur.is_some() {
            match Self::parse_definition(cur.unwrap()) {
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
        let mut sequence = vec![Self::parse_expression(cur.unwrap())?];
        let rest: Result<Vec<Expression>> = iter.map(Self::parse_expression).collect();
        sequence.extend(rest?);

        Ok(BodyExpression {
            definitions,
            sequence,
        })
    }
    /// Parse a define expression
    ///
    /// Ref: r7rs 7.1.6
    ///
    /// ```grammar
    /// <definition> ->
    ///   (define <IDENTIFIER> <expression>)                                         |
    ///   (define (<IDENTIFIER> <def formals>) <body>)                               |
    ///   <syntax definition>                                                        |
    ///   (define-values <formals> <body>)                                           |
    ///   (define-record-type <IDENTIFIER> <constructor> <IDENTIFIER> <field spec>*) |
    ///   (begin <definition>*)
    /// ```
    fn parse_definition(datum: &Datum) -> Result<DefinitionExpression> {
        match datum.sexp() {
            Sexp::List(ls) => match Self::head_symbol(&ls) {
                Some("define") => match &ls[..] {
                    [_, identifier, expr] => Ok(DefinitionExpression::DefineSimple(
                        identifier::parse(&identifier)?,
                        Box::new(Self::parse_expression(&expr)?),
                    )),
                    _ => todo!(),
                },
                Some("begin") => {
                    let exprs: Result<Vec<Box<DefinitionExpression>>> = ls[1..]
                        .iter()
                        .map(Self::parse_definition)
                        .map(|e| e.map(Box::new))
                        .collect();

                    Ok(DefinitionExpression::Begin(exprs?))
                }
                _ => Error::parse_error("Invalid definition", datum.location.clone()),
            },
            _ => Error::parse_error("Expected definition", datum.location.clone()),
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
        match Self::parse_definition(&datum).map(Expression::Define) {
            Ok(expr) => Ok(expr),
            Err(_) => Self::parse_expression(&datum),
        }
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

    // Literals
    // See: r7rs page 12 for all examples of literals we need to support
    // TODO: add support for the other literals once we support them

    #[test]
    fn test_parse_literal_constant() {
        assert_parse_as(
            "#t",
            Expression::constant(&make_datum(Sexp::Bool(true), 1, 1)),
        );

        assert_parse_as(
            "\"foo\"",
            Expression::constant(&make_datum(Sexp::string("foo"), 1, 1)),
        );
    }

    #[test]
    fn test_parse_assignment() {
        assert_parse_as(
            "(set! foo #t)",
            Expression::assign(
                Identifier::synthetic("foo"),
                Expression::constant(&make_datum(Sexp::Bool(true), 1, 11)),
                location(1, 1),
            ),
        );

        assert_parse_error("(set! foo)");
    }

    #[test]
    fn test_parse_let_simple() {
        assert_parse_as(
            "(let ((x #t)) #f)",
            Expression::let_bind(
                vec![(
                    Identifier::synthetic("x"),
                    Expression::constant(&make_datum(Sexp::Bool(true), 1, 10)),
                )],
                Expression::constant(&make_datum(Sexp::Bool(false), 1, 15)).to_body_expression(),
                location(1, 1),
            ),
        )
    }

    #[test]
    fn test_parse_define() {
        assert_parse_as(
            "(define x #t)",
            Expression::define(
                Identifier::synthetic("x"),
                Expression::constant(&make_datum(Sexp::Bool(true), 1, 11)),
                location(1, 1),
            ),
        )
    }

    #[test]
    fn test_parse_begin() {
        assert_parse_as(
            "(begin #t)",
            Expression::begin(
                Expression::constant(&make_datum(Sexp::Bool(true), 1, 8)),
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
