use super::error::Error;
use crate::compiler::frontend::parser::datum::Datum;
use crate::compiler::source::Source;
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
    pub fn parse_one<T: Source>(source: &mut T) -> Result<Option<Self>> {
        let datum_ast = Datum::parse(source)?;

        match datum_ast {
            Some(ast) => Ok(Some(Self::parse_expression(ast)?)),
            None => Ok(None),
        }
    }

    pub fn constant(value: Value, location: SourceLocation) -> Expression {
        Expression::Literal(LiteralExpression::SelfEvaluating(value), location)
    }

    pub fn quoted_value(value: Value, location: SourceLocation) -> Expression {
        Expression::Literal(LiteralExpression::Quotation(value), location)
    }

    fn parse_expression(datum: Datum) -> Result<Expression> {
        match &datum.value {
            val @ Value::Bool(_) => Ok(Self::constant(val.clone(), datum.source_location.clone())),
            Value::ProperList(ls) => match &ls.head() {
                Some(Value::Symbol(sym)) => match sym.as_str() {
                    "quote" => Self::parse_quoted_datum(&ls, &datum),
                    _ => todo!(),
                },
                None => Error::parse_error(
                    "Unexpected empty list literal. Did you intend to quote it?",
                    datum.source_location.clone(),
                ),
                _ => todo!(),
            },

            _ => todo!(),
        }
    }

    #[inline]
    fn parse_quoted_datum(ls: &list::List, datum: &Datum) -> Result<Expression> {
        match (ls.len(), ls.second()) {
            (2, Some(value)) => Ok(Self::quoted_value(
                value.clone(),
                datum.source_location.clone(),
            )),
            _ => Error::parse_error(
                "Too many arguments. Expected (quote <datum>).",
                datum.source_location.clone(),
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
        let mut source = src("#t");
        let source_type = source.source_type();

        assert_eq!(
            Expression::parse_one(&mut source).unwrap(),
            Some(Expression::constant(
                Value::Bool(true),
                SourceLocation::new(source_type, 1, 1)
            ))
        )
    }

    #[test]
    fn test_parse_literal_quoted_datum() {
        let mut source = src("'#t");
        let source_type = source.source_type();

        assert_eq!(
            Expression::parse_one(&mut source).unwrap(),
            Some(Expression::quoted_value(
                Value::Bool(true),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );

        source = src("'foo");
        assert_eq!(
            Expression::parse_one(&mut source).unwrap(),
            Some(Expression::quoted_value(
                Value::symbol("foo"),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );

        source = src("'");
        assert!(
            Expression::parse_one(&mut source).is_err(),
            "expected error on single quote"
        );
    }

    fn src(inp: &str) -> impl Source {
        StringSource::new(inp, "datum-parser-test")
    }
}
