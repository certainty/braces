use super::error::Error;
use crate::compiler::frontend::parser::datum;
use crate::compiler::frontend::parser::datum::Datum;
use crate::compiler::frontend::parser::Parser;
use crate::compiler::source::{Source, SourceType};
use crate::compiler::source_location::SourceLocation;
use crate::vm::scheme::value::list;
use crate::vm::scheme::value::Value;

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Literal(LiteralExpression, SourceLocation),
}

#[derive(Clone, PartialEq, Debug)]
pub enum LiteralExpression {
    SelfEvaluating(Value),
    Quotation(Value),
}

impl Expression {
    pub fn parse_one<T: Source>(source: &mut T) -> Result<Self> {
        let parser = Parser;
        let ast = parser.parse_datum(source)?;
        Self::parse_expression(ast)
    }

    pub fn constant(value: Value, location: SourceLocation) -> Expression {
        Expression::Literal(LiteralExpression::SelfEvaluating(value), location)
    }

    pub fn quoted_value(value: Value, location: SourceLocation) -> Expression {
        Expression::Literal(LiteralExpression::Quotation(value), location)
    }

    fn parse_expression(datum: Datum) -> Result<Expression> {
        match &datum.value {
            val @ Value::Bool(_) => Ok(Self::constant(val.clone(), datum.location.clone())),
            val @ Value::Char(_) => Ok(Self::constant(val.clone(), datum.location.clone())),
            val @ Value::String(_) => Ok(Self::constant(val.clone(), datum.location.clone())),
            Value::ProperList(ls) => match &ls.head() {
                Some(Value::Symbol(sym)) => match sym.as_str() {
                    "quote" => Self::parse_quoted_datum(&ls, &datum),
                    _ => todo!(),
                },
                None => Error::parse_error(
                    "Unexpected empty list literal. Did you intend to quote it?",
                    datum.location.clone(),
                ),
                _ => todo!(),
            },

            _ => todo!(),
        }
    }

    #[inline]
    fn parse_quoted_datum(ls: &list::List, datum: &Datum) -> Result<Expression> {
        match (ls.len(), ls.second()) {
            (2, Some(value)) => Ok(Self::quoted_value(value.clone(), datum.location.clone())),
            _ => Error::parse_error(
                "Too many arguments. Expected (quote <datum>).",
                datum.location.clone(),
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::{Source, StringSource};

    // Literals
    // See: r7rs page 12 for all examples of literals we need to support
    // TODO: add support for the other literals once we support them

    #[test]
    fn test_parse_literal_constant() {
        assert_parse_as(
            "#t",
            Expression::constant(Value::Bool(true), location(1, 1)),
        );

        assert_parse_as(
            "\"foo\"",
            Expression::constant(Value::string("foo"), location(1, 1)),
        );
    }

    #[test]
    fn test_parse_literal_quoted_datum() {
        assert_parse_as(
            "'#t",
            Expression::quoted_value(Value::Bool(true), location(1, 1)),
        );

        assert_parse_as(
            "'#\\a",
            Expression::quoted_value(Value::character('a'), location(1, 1)),
        );

        assert_parse_as(
            "'foo",
            Expression::quoted_value(Value::symbol("foo"), location(1, 1)),
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
}
