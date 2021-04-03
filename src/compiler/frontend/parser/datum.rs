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

        match DataParser::parse(Rule::datum_single, &buffer) {
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
            Rule::DELIMITED_IDENTIFIER => Self::parse_delimited_identifier(pair.as_str(), loc),
            Rule::STRING => Self::parse_string(pair.as_str(), loc, &source_type),
            Rule::NAMED_CHAR_LITERAL => {
                Self::parse_named_character(pair.as_str(), loc, source_type)
            }
            Rule::HEX_CHAR_LITERAL => Self::parse_hex_character(pair.as_str(), loc, source_type),
            Rule::CHAR_LITERAL => Self::parse_character(pair.as_str(), loc, source_type),
            Rule::abbreviation => Self::parse_abbreviation(pair, loc, &source_type),
            _ => Error::syntax_error("Unsupported external representation", source_type.clone()),
        }
    }

    #[inline]
    fn parse_delimited_identifier(s: &str, loc: SourceLocation) -> Result<Datum> {
        Ok(Datum::new(
            Value::Symbol(s[1..s.len() - 1].to_string()),
            loc,
        ))
    }

    #[inline]
    fn parse_abbreviation(
        pair: Pair<Rule>,
        loc: SourceLocation,
        source_type: &SourceType,
    ) -> Result<Datum> {
        let parts: Vec<Pair<Rule>> = pair.into_inner().collect();

        match &parts[..] {
            [prefix, datum] => {
                let other_datum = Datum::to_ast(datum.clone(), source_type)?;
                match prefix.as_rule() {
                    Rule::abbrev_quote => Ok(Datum::new(
                        Value::proper_list(vec![Value::symbol("quote"), other_datum.value.clone()]),
                        loc,
                    )),
                    _ => todo!(),
                }
            }
            _ => Error::syntax_error("Expected (abbrev-prefix <datum>)", source_type.clone()),
        }
    }

    #[inline]
    fn parse_named_character(
        str: &str,
        loc: SourceLocation,
        source_type: &SourceType,
    ) -> Result<Datum> {
        let character = match str.get(2..) {
            Some("space") => Ok(' '),
            Some("newline") => Ok('\n'),
            Some("return") => Ok('\n'),
            Some("tab") => Ok('\t'),
            Some("alarm") => Ok('\u{0007}'),
            Some("null") => Ok('\u{0000}'),
            Some("backspace") => Ok('\u{0008}'),
            Some("delete") => Ok('\u{0018}'),
            Some("escape") => Ok('\u{001b}'),
            Some(unknown) => Error::syntax_error(
                &format!("Unknown character literal `{}`", unknown),
                source_type.clone(),
            ),
            None => Error::syntax_error(
                "Missing character name. Expected named character literal",
                source_type.clone(),
            ),
        };

        Ok(Datum::new(Value::character(character?), loc))
    }

    #[inline]
    fn parse_hex_character(
        str: &str,
        loc: SourceLocation,
        _source_type: &SourceType,
    ) -> Result<Datum> {
        if let Some(c) = Self::hex_to_char(str.trim_start_matches("#\\x")) {
            Ok(Datum::new(Value::character(c), loc))
        } else {
            Error::parse_error("Couldn't parse hex character literal", loc)
        }
    }

    #[inline]
    fn parse_character(str: &str, loc: SourceLocation, _source_type: &SourceType) -> Result<Datum> {
        Ok(Datum::new(
            Value::character(str.chars().last().unwrap()),
            loc,
        ))
    }

    #[inline]
    fn parse_string(str: &str, loc: SourceLocation, _source_type: &SourceType) -> Result<Datum> {
        let mut result = String::new();
        let mut iter = str.trim_matches(|c| c == '"').chars();

        loop {
            match iter.next() {
                Some('\\') => match iter.next() {
                    Some('n') => result.push('\n'),
                    Some('r') => result.push('\r'),
                    Some('b') => result.push('\u{0007}'),
                    Some('t') => result.push('\t'),
                    Some('x') => {
                        let mut hex_value = String::new();
                        loop {
                            match iter.next() {
                                Some(';') => break,
                                Some(digit) => hex_value.push(digit),
                                None => return Error::parse_error("Unexpected end of string", loc),
                            }
                        }
                        if let Some(c) = Self::hex_to_char(&hex_value) {
                            result.push(c);
                        } else {
                            return Error::parse_error("Invalid hex escape", loc);
                        }
                    }
                    Some('"') => result.push('"'),
                    Some('\\') => result.push('\\'),
                    Some(' ') => continue, // handle intraline ws
                    Some('\t') => continue,
                    Some('\n') => continue,
                    Some('\r') => continue,
                    _ => return Error::parse_error("Invalid escape character", loc),
                },
                Some(c) => result.push(c),
                None => break,
            }
        }

        Ok(Datum::new(Value::string(&result), loc))
    }

    #[inline]
    fn hex_to_char(s: &str) -> Option<char> {
        if let Ok(v) = u32::from_str_radix(s, 16).map(std::char::from_u32) {
            v
        } else {
            None
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

    #[test]
    fn test_read_character_named() {
        let mut source = src("");
        let source_type = source.source_type();

        source = src("#\\alarm");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::character('\u{0007}'),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );
    }

    #[test]
    fn test_read_character_hex() {
        let mut source = src("");
        let source_type = source.source_type();

        source = src("#\\x7");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::character('\u{0007}'),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );

        source = src("#\\xtrash");
        assert!(
            Datum::parse(&mut source).is_err(),
            "expected not to parse trash hex literals"
        );
    }

    #[test]
    fn test_read_character() {
        let mut source = src("");
        let source_type = source.source_type();

        source = src("#\\c");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::character('c'),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );

        source = src("#\\☆");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::character('☆'),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );
    }

    #[test]
    fn test_read_string() {
        let mut source = src("");
        let source_type = source.source_type();

        source = src("\"this is my string\"");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::string("this is my string"),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );

        source = src("\"this is my ☆ string ☆\"");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::string("this is my ☆ string ☆"),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );

        source = src("\"string with \\n and \\t \"");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::string("string with \n and \t "),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );

        source = src("\"string with \\xa; and \\t \"");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::string("string with \n and \t "),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );

        source = src("\"string with \\\n and the\\\n next line\"");
        assert_eq!(
            Datum::parse(&mut source).unwrap(),
            Some(Datum::new(
                Value::string("string with  and the next line"),
                SourceLocation::new(source_type.clone(), 1, 1)
            ))
        );
    }

    fn src(inp: &str) -> impl Source {
        StringSource::new(inp, "datum-parser-test")
    }
}
