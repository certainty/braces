use super::error::Error;
use crate::compiler::source::{Source, SourceType};
use crate::compiler::source_location::SourceLocation;
use crate::vm::scheme::value::Value;
use pest::iterators::{Pair, Pairs};
use pest::Parser;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub struct Datum {
    pub value: Value,
    pub source_location: SourceLocation,
}

impl Datum {
    // parses the next available Datum from Source
    pub fn parse(source: &mut impl Source) -> Result<Option<Datum>> {
        let source_type = source.source_type();
        let mut buffer = String::new();
        source.read_to_string(&mut buffer)?;

        match DataParser::parse(Rule::datum, &buffer) {
            Ok(pair) => {
                let mut ast = Self::to_ast_seq(pair, &source_type)?;
                Ok(ast.pop())
            }
            Err(e) => Error::syntax_error(&format!("{}", e), source_type.clone()),
        }
    }

    pub fn boolean(value: bool, source_location: SourceLocation) -> Datum {
        Datum {
            value: Value::Bool(value),
            source_location,
        }
    }

    pub fn symbol(value: &str, source_location: SourceLocation) -> Datum {
        Datum {
            value: Value::Symbol(value.to_string()),
            source_location,
        }
    }

    fn to_ast_seq(pairs: Pairs<Rule>, source_type: &SourceType) -> Result<Vec<Datum>> {
        pairs.map(|p| Self::to_ast(&p, source_type)).collect()
    }

    fn to_ast(pair: &Pair<Rule>, source_type: &SourceType) -> Result<Datum> {
        let loc = Self::create_location(&pair, source_type);

        match pair.as_rule() {
            Rule::BOOL_TRUE => Ok(Self::boolean(true, loc)),
            Rule::BOOL_FALSE => Ok(Self::boolean(false, loc)),
            Rule::IDENTIFIER => Ok(Self::symbol(pair.as_str(), loc)),
            Rule::PECULIAR_IDENTIFIER => Ok(Self::symbol(pair.as_str(), loc)),
            _ => Error::syntax_error("Unsupported external representation", source_type.clone()),
        }
    }

    fn create_location(pair: &Pair<Rule>, source_type: &SourceType) -> SourceLocation {
        let span = pair.as_span();
        let start = span.start_pos();
        let (line, col) = start.line_col();

        SourceLocation::new(source_type.clone(), line, col)
    }
}

#[derive(Parser)]
#[grammar = "compiler/frontend/parser/datum.pest"]
struct DataParser;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::{Source, StringSource};

    #[test]
    fn test_read_bool_true() {
        let mut source = src("#t");
        let source_type = source.source_type();

        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::boolean(true, SourceLocation::new(source_type, 1, 1)))
        );

        source = src("#true");
        let source_type = source.source_type();
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::boolean(true, SourceLocation::new(source_type, 1, 1)))
        );
    }

    #[test]

    fn test_read_symbol() {
        let mut source = src("<=?");
        let source_type = source.source_type();
        let symbols = vec![
            "<=?",
            "->string",
            "a34kTMNs",
            "lambda",
            "list->vector",
            "q",
            "V17a",
            "the-word-recursion-has-many-meanings",
        ];

        for sym in symbols.iter() {
            source = src(sym);

            assert_eq!(
                Datum::parse(&mut source).unwrap(),
                Some(Datum::symbol(
                    sym,
                    SourceLocation::new(source_type.clone(), 1, 1)
                ))
            );
        }
    }

    #[test]
    fn test_read_symbol_peculiar() {
        let mut source = src("...");
        let source_type = source.source_type();
        let symbols = vec!["...", "+soup+", "+"];

        for sym in symbols.iter() {
            source = src(sym);

            assert_eq!(
                Datum::parse(&mut source).unwrap(),
                Some(Datum::symbol(
                    sym,
                    SourceLocation::new(source_type.clone(), 1, 1)
                ))
            );
        }
    }

    fn src(inp: &str) -> impl Source {
        StringSource::new(inp, "datum-parser-test")
    }
}
