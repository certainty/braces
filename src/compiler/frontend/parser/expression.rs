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

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier, SourceLocation),
    Literal(LiteralExpression),
    Assign(Identifier, Box<Expression>, SourceLocation),
    Conditional(
        Box<Expression>,
        Box<Expression>,
        Option<Box<Expression>>,
        SourceLocation,
    ),
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

    pub fn identifier(str: String, loc: SourceLocation) -> Expression {
        Expression::Identifier(Identifier(str), loc)
    }

    pub fn conditional(
        test: Expression,
        conseqent: Expression,
        alternate: Option<Expression>,
        location: SourceLocation,
    ) -> Expression {
        Expression::Conditional(
            Box::new(test),
            Box::new(conseqent),
            alternate.map(Box::new),
            location,
        )
    }

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
            [_, identifier, expr] => {
                if let Expression::Identifier(id, _) = Self::parse_expression(identifier)? {
                    Ok(Expression::assign(
                        id,
                        Self::parse_expression(expr)?,
                        loc.clone(),
                    ))
                } else {
                    Error::parse_error(
                        "Can only set! variables. (set! <identifier> <expression>)",
                        loc.clone(),
                    )
                }
            }
            _ => Error::parse_error("Expected (set! <identifier> <expression>)", loc.clone()),
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

    fn assert_parse_as(inp: &str, exp: Expression) {
        let mut source = StringSource::new(inp, "datum-parser-test");
        let parsed_exp = Expression::parse_one(&mut source).unwrap();

        assert_eq!(exp, parsed_exp)
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
