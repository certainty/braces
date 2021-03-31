use super::error::Error;
use crate::compiler::frontend::parser::datum::Datum;
use crate::compiler::source::Source;
use crate::compiler::source_location::SourceLocation;

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Literal(LiteralExpression, SourceLocation),
}

#[derive(Clone, PartialEq, Debug)]
pub enum LiteralExpression {
    SelfEvaluating(SelfEvaluatingExpression),
    Quotation(Datum),
}

#[derive(Clone, PartialEq, Debug)]
pub enum SelfEvaluatingExpression {
    Boolean(bool),
}

impl Expression {
    pub fn parse<T: Source>(source: &mut T) -> Result<Option<Self>> {
        let datum_ast = Datum::parse(source)?;

        match datum_ast {
            Some(ast) => Ok(Some(Self::parse_expression(ast)?)),
            None => Ok(None),
        }
    }

    pub fn boolean(value: bool, location: SourceLocation) -> Expression {
        Expression::Literal(
            LiteralExpression::SelfEvaluating(SelfEvaluatingExpression::Boolean(value)),
            location,
        )
    }

    fn parse_expression(datum: Datum) -> Result<Expression> {
        match datum {
            Datum::Boolean(val, loc) => Ok(Self::boolean(val, loc.clone())),
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
            Some(Expression::boolean(
                true,
                SourceLocation::new(source_type, 1, 1)
            ))
        )
    }

    fn src(inp: &str) -> impl Source {
        StringSource::new(inp, "datum-parser-test")
    }
}
