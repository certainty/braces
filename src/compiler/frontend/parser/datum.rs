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
    pub fn new(value: Value, source_location: SourceLocation) -> Self {
        Self {
            value,
            source_location,
        }
    }

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

    fn to_ast_seq(pairs: Pairs<Rule>, source_type: &SourceType) -> Result<Vec<Datum>> {
        pairs.map(|p| Self::to_ast(p, source_type)).collect()
    }

    fn to_ast(pair: Pair<Rule>, source_type: &SourceType) -> Result<Datum> {
        let loc = Self::create_location(&pair, source_type);

        match pair.as_rule() {
            Rule::BOOL_TRUE => Ok(Datum::new(Value::Bool(true), loc)),
            Rule::BOOL_FALSE => Ok(Datum::new(Value::Bool(false), loc)),
            Rule::IDENTIFIER => Ok(Datum::new(Value::Symbol(pair.as_str().to_string()), loc)),
            Rule::PECULIAR_IDENTIFIER => {
                Ok(Datum::new(Value::Symbol(pair.as_str().to_string()), loc))
            }
            Rule::DELIMITED_IDENTIFIER => {
                let s = pair.as_str();
                Ok(Datum::new(
                    Value::Symbol(s[1..s.len() - 1].to_string()),
                    loc,
                ))
            }
            Rule::abbreviation => {
                let parts: Vec<Pair<Rule>> = pair.into_inner().collect();
                match &parts[..] {
                    [prefix, datum] => {
                        let other_datum = Datum::to_ast(datum.clone(), source_type)?;
                        match prefix.as_rule() {
                            Rule::abbrev_quote => Ok(Datum::new(
                                Value::proper_list(vec![
                                    Value::symbol("quote"),
                                    other_datum.value.clone(),
                                ]),
                                loc,
                            )),
                            _ => todo!(),
                        }
                    }
                    _ => {
                        Error::syntax_error("Expected (abbrev-prefix <datum>)", source_type.clone())
                    }
                }
            }
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
            Some(Datum::new(
                Value::boolean(true),
                SourceLocation::new(source_type, 1, 1)
            ))
        );

        source = src("#true");
        let source_type = source.source_type();
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::boolean(true),
                SourceLocation::new(source_type, 1, 1)
            ))
        );
    }

    #[test]
    fn test_read_quotation() {
        let mut source = src("'#f");
        let source_type = source.source_type();

        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::proper_list(vec![Value::symbol("quote"), Value::boolean(false)]),
                SourceLocation::new(source_type, 1, 1)
            ))
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
                Some(Datum::new(
                    Value::symbol(sym),
                    SourceLocation::new(source_type.clone(), 1, 1)
                ))
            );
        }
    }

    #[test]
    fn test_read_symbol_peculiar() {
        let mut source = src("");
        let source_type = source.source_type();
        let symbols = vec!["...", "+soup+", "+"];

        for sym in symbols.iter() {
            source = src(sym);

            assert_eq!(
                Datum::parse(&mut source).unwrap(),
                Some(Datum::new(
                    Value::symbol(sym),
                    SourceLocation::new(source_type.clone(), 1, 1)
                ))
            );
        }
    }

    #[test]
    fn test_read_symbol_delimited() {
        let mut source = src("");
        let source_type = source.source_type();

        source = src("|two words|");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::symbol("two words"),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );

        source = src("|two\x20;words|");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::symbol("two\x20;words"),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );
    }

    fn src(inp: &str) -> impl Source {
        StringSource::new(inp, "datum-parser-test")
    }
}
