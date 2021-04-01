use super::error::Error;
use crate::compiler::frontend::parser::datum::Datum;
use crate::compiler::source::Source;
use crate::compiler::source_location::SourceLocation;
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
    pub fn parse<T: Source>(source: &mut T) -> Result<Option<Self>> {
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
        match datum.value {
            val @ Value::Bool(_) => Ok(Self::constant(val, datum.source_location.clone())),
            Value::ProperList(elts) if elts.len() == 2 => {
                let mut iter = elts.iter();
                let head = iter.next().unwrap();
                let value = iter.next().unwrap();

                match &**head {
                    Value::Symbol(sym) if sym == "quote" => Ok(Self::quoted_value(
                        *value.clone(),
                        datum.source_location.clone(),
                    )),
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::{Source, StringSource};

    #[test]
    fn test_parse_literal_boolean() {
        let mut source = src("#t");
        let source_type = source.source_type();

        assert_eq!(
            Expression::parse(&mut source).unwrap(),
            Some(Expression::constant(
                Value::Bool(true),
                SourceLocation::new(source_type, 1, 1)
            ))
        )
    }

    fn src(inp: &str) -> impl Source {
        StringSource::new(inp, "datum-parser-test")
    }
}
