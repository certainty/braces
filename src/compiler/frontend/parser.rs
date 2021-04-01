pub mod datum;
pub mod error;
pub mod expression;
use crate::compiler::source::Source;
use error::Error;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser;

impl Parser {
    pub fn parse<T: Source>(source: &mut T) -> Result<Option<expression::Expression>> {
        expression::Expression::parse(source)
    }
}

#[cfg(test)]
mod tests {
    use super::expression::Expression;
    use super::*;
    use crate::compiler::source::{Source, StringSource};
    use crate::compiler::source_location::SourceLocation;
    use crate::vm::scheme::value::Value;

    #[test]
    fn test_parse_literal() {
        let mut source = src("#true");
        let source_type = source.source_type();

        assert_eq!(
            Parser::parse(&mut source).unwrap(),
            Some(Expression::constant(
                Value::Bool(true),
                SourceLocation::new(source_type, 1, 1)
            ))
        )
    }

    fn src(inp: &str) -> impl Source {
        StringSource::new(inp, "expression-parser-test")
    }
}
