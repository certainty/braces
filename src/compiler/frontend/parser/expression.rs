pub mod error;
use crate::compiler::frontend::parser::sexp::datum::{Datum, Sexp};
use crate::compiler::frontend::parser::Parser;
use crate::compiler::source::Source;
use crate::compiler::source_location::SourceLocation;
use error::Error;

type Result<T> = std::result::Result<T, Error>;

#[repr(transparent)]
#[derive(Clone, PartialEq, Debug)]
pub struct Identifier(String);

impl Identifier {
    pub fn string(&self) -> &String {
        &self.0
    }
}

impl From<Identifier> for String {
    fn from(id: Identifier) -> String {
        id.0
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Identifier {
        Identifier(s.to_string())
    }
}

pub trait HasSourceLocation {
    fn source_location<'a>(&'a self) -> &'a SourceLocation;
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier, SourceLocation),
    Literal(LiteralExpression),
    Assign(Identifier, Box<Expression>, SourceLocation),
    Define(DefinitionExpression, SourceLocation),
    Let(LetExpression, SourceLocation),
    If(IfExpression, SourceLocation),
}

impl HasSourceLocation for Expression {
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        match self {
            Self::Identifier(_, loc) => &loc,
            Self::Literal(LiteralExpression::SelfEvaluating(datum)) => &datum.location,
            Self::Literal(LiteralExpression::Quotation(datum)) => &datum.location,
            Self::Assign(_, expr, loc) => &loc,
            Self::Define(_, loc) => &loc,
            Self::Let(_, loc) => &loc,
            Self::If(_, loc) => &loc,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct IfExpression {
    pub test: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Option<Box<Expression>>,
}

pub type BindingSpec = (Identifier, Expression);

#[derive(Clone, PartialEq, Debug)]
pub enum LetExpression {
    Let(Vec<BindingSpec>, Box<BodyExpression>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct BodyExpression {
    pub definitions: Vec<DefinitionExpression>,
    pub sequence: Vec<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum DefinitionExpression {
    DefineSimple(Identifier, Box<Expression>),
    Begin(Vec<Box<DefinitionExpression>>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum LiteralExpression {
    SelfEvaluating(Datum),
    Quotation(Datum),
}

impl Expression {
    pub fn parse_one<T: Source>(source: &mut T) -> Result<Self> {
        let parser = Parser;
        let ast = parser.parse_datum(source)?;
        Self::parse_expression(&ast)
    }

    pub fn constant(datum: &Datum) -> Expression {
        Expression::Literal(LiteralExpression::SelfEvaluating(datum.clone()))
    }

    pub fn quoted_value(datum: &Datum) -> Expression {
        Expression::Literal(LiteralExpression::Quotation(datum.clone()))
    }

    pub fn assign(id: Identifier, expr: Expression, loc: SourceLocation) -> Expression {
        Expression::Assign(id, Box::new(expr), loc)
    }

    pub fn define(id: Identifier, expr: Expression, loc: SourceLocation) -> Expression {
        Expression::Define(DefinitionExpression::DefineSimple(id, Box::new(expr)), loc)
    }

    pub fn identifier(str: String, loc: SourceLocation) -> Expression {
        Expression::Identifier(Identifier(str), loc)
    }

    pub fn to_body_expression(&self) -> BodyExpression {
        BodyExpression {
            definitions: vec![],
            sequence: vec![self.clone()],
        }
    }

    pub fn conditional(
        test: Expression,
        conseqent: Expression,
        alternate: Option<Expression>,
        location: SourceLocation,
    ) -> Expression {
        Expression::If(
            IfExpression {
                test: Box::new(test),
                consequent: Box::new(conseqent),
                alternate: alternate.map(Box::new),
            },
            location,
        )
    }

    pub fn let_bind(
        bindings: Vec<BindingSpec>,
        body: BodyExpression,
        loc: SourceLocation,
    ) -> Expression {
        Expression::Let(LetExpression::Let(bindings, Box::new(body)), loc)
    }

    /// Parse a single datum into an expression
    fn parse_expression(datum: &Datum) -> Result<Expression> {
        match datum.sexp() {
            Sexp::Symbol(s) => Ok(Self::identifier(s.to_string(), datum.location.clone())),
            Sexp::Bool(_) => Ok(Self::constant(datum)),
            Sexp::Char(_) => Ok(Self::constant(datum)),
            Sexp::String(_) => Ok(Self::constant(datum)),
            Sexp::List(ls) => match Self::head_symbol(&ls) {
                Some("quote") => Self::parse_quoted_datum(&ls, &datum.location),
                Some("set!") => Self::parse_assignment(&ls, &datum.location),
                Some("if") => Self::parse_conditional(&ls, &datum.location),
                Some("let") => Self::parse_let(&ls, &datum.location),
                Some("define") => Ok(Expression::Define(
                    Self::parse_definition(&datum)?,
                    datum.location.clone(),
                )),
                other => {
                    println!("{:?}", other);
                    todo!()
                }
            },

            _ => todo!(),
        }
    }

    fn parse_conditional(ls: &Vec<Datum>, loc: &SourceLocation) -> Result<Expression> {
        match &ls[..] {
            [test, consequent, alternate] => {
                let test_expr = Self::parse_expression(&test)?;
                let consequent_expr = Self::parse_expression(&consequent)?;
                let alternate_expr = Self::parse_expression(&alternate)?;

                Ok(Self::conditional(
                    test_expr,
                    consequent_expr,
                    Some(alternate_expr),
                    loc.clone(),
                ))
            }
            [test, consequent] => {
                let test_expr = Self::parse_expression(&test)?;
                let consequent_expr = Self::parse_expression(&consequent)?;

                Ok(Self::conditional(
                    test_expr,
                    consequent_expr,
                    None,
                    loc.clone(),
                ))
            }
            _ => Error::parse_error(
                "Expected (if <test> <consequent> <alternate>?)",
                loc.clone(),
            ),
        }
    }

    fn parse_assignment(ls: &Vec<Datum>, loc: &SourceLocation) -> Result<Expression> {
        match &ls[..] {
            [_, identifier, expr] => Ok(Expression::assign(
                Self::parse_identifier(identifier)?,
                Self::parse_expression(expr)?,
                loc.clone(),
            )),
            _other => Error::parse_error("Expected (set! <identifier> <expression>)", loc.clone()),
        }
    }

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

    fn parse_binding_specs(datum: &Datum) -> Result<Vec<BindingSpec>> {
        match datum.sexp() {
            Sexp::List(ls) => ls.iter().map(Self::parse_binding_spec).collect(),
            _ => Error::parse_error("Expected list of binding specs", datum.location.clone()),
        }
    }

    fn parse_binding_spec(datum: &Datum) -> Result<BindingSpec> {
        match datum.sexp() {
            Sexp::List(ls) => match &ls[..] {
                [identifier, expr] => Ok((Self::parse_identifier(identifier)?, Self::parse_expression(expr)?)),
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

    fn parse_identifier(datum: &Datum) -> Result<Identifier> {
        let id_expr = Self::parse_expression(datum)?;
        if let Expression::Identifier(id, _) = id_expr {
            Ok(id)
        } else {
            Error::parse_error("Expected identifier", datum.location.clone())
        }
    }

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

    fn parse_definition(datum: &Datum) -> Result<DefinitionExpression> {
        match datum.sexp() {
            Sexp::List(ls) => match Self::head_symbol(&ls) {
                Some("define") => match &ls[..] {
                    [_, identifier, expr] => Ok(DefinitionExpression::DefineSimple(
                        Self::parse_identifier(&identifier)?,
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

    #[inline]
    fn parse_quoted_datum(ls: &Vec<Datum>, loc: &SourceLocation) -> Result<Expression> {
        match &ls[..] {
            [_, value] => Ok(Self::quoted_value(value)),
            _ => Error::parse_error("Too many arguments. Expected (quote <datum>).", loc.clone()),
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
    fn test_parse_literal_quoted_datum() {
        assert_parse_as(
            "'#t",
            Expression::quoted_value(&make_datum(Sexp::Bool(true), 1, 2)),
        );

        assert_parse_as(
            "'#\\a",
            Expression::quoted_value(&make_datum(Sexp::character('a'), 1, 2)),
        );

        assert_parse_as(
            "'foo",
            Expression::quoted_value(&make_datum(Sexp::symbol("foo"), 1, 2)),
        );

        assert_parse_error("'");
    }

    #[test]
    fn test_parse_assignment() {
        assert_parse_as(
            "(set! foo #t)",
            Expression::assign(
                Identifier("foo".to_string()),
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
                    Identifier::from("x"),
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
                Identifier::from("x"),
                Expression::constant(&make_datum(Sexp::Bool(true), 1, 11)),
                location(1, 1),
            ),
        )
    }

    fn assert_parse_as(inp: &str, exp: Expression) {
        let mut source = StringSource::new(inp, "datum-parser-test");
        let parsed_exp = Expression::parse_one(&mut source).unwrap();

        assert_eq!(parsed_exp, exp)
    }

    fn assert_parse_error(inp: &str) {
        let mut source = StringSource::new(inp, "datum-parser-test");

        assert!(
            Expression::parse_one(&mut source).is_err(),
            "expected parse error"
        )
    }

    fn location(line: usize, col: usize) -> SourceLocation {
        SourceLocation::new(
            SourceType::Buffer("datum-parser-test".to_string()),
            line,
            col,
        )
    }

    fn make_datum(sexp: Sexp, line: usize, col: usize) -> Datum {
        Datum::new(sexp, location(line, col))
    }
}
