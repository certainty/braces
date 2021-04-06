use crate::compiler::source::{Source, SourceType};
use crate::compiler::source_location::SourceLocation;
use crate::vm::scheme::value::Value;
use nom::branch::alt;
use nom::bytes::streaming::{is_not, tag, take_while_m_n};
use nom::character::streaming::{char, multispace1};
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::error::{ErrorKind, FromExternalError, ParseError};
use nom::multi::fold_many0;
use nom::sequence::{delimited, preceded};
use nom::IResult;
use nom_locate::LocatedSpan;
use std::convert::From;

#[derive(Debug)]
pub enum Error {
    IoError(std::io::Error),
    FixMe,
    Nom(ErrorKind),
}

impl<I> ParseError<I> for Error {
    fn from_error_kind(_: I, kind: ErrorKind) -> Self {
        Error::Nom(kind)
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}

impl<I> FromExternalError<I, std::num::ParseIntError> for Error {
    fn from_external_error(_: I, _: nom::error::ErrorKind, _: std::num::ParseIntError) -> Self {
        todo!()
    }
}

impl<I> FromExternalError<I, std::io::Error> for Error {
    fn from_external_error(_: I, _: nom::error::ErrorKind, e: std::io::Error) -> Self {
        Error::IoError(e)
    }
}

type Result<'a, T> = IResult<Span<'a>, T, Error>;

type Span<'a> = LocatedSpan<&'a str, SourceType>;

#[derive(Debug, PartialEq)]
struct Datum<'a> {
    pub location: Option<Span<'a>>,
    value: Value,
}

/// R7RS
impl<'a> Datum<'a> {
    fn with_location(value: Value, location: &Span<'a>) -> Self {
        Datum {
            location: Some(location.to_owned()),
            value,
        }
    }
}

/// r7rs compliant parser
struct Parser {}

impl Parser {
    pub fn parse<'a, T: Source>(&self, source: &'a mut T) -> std::result::Result<Datum, Error> {
        let source_type = source.source_type();

        match source.as_str() {
            Err(e) => Err(Error::IoError(e)),
            Ok(buffer) => {
                let input = Span::new_extra(buffer, source_type);
                match self.parse_datum(input) {
                    Ok(v) => Ok(v.1),
                    Err(e) => Err(Error::FixMe),
                }
            }
        }
    }

    fn parse_datum<'a>(&self, input: Span<'a>) -> Result<'a, Datum> {
        todo!()
    }

    fn parse_char<'a>(&self, input: Span<'a>) -> Result<'a, Datum> {
        todo!()
    }

    fn parse_char_literal<'a>(&self, input: Span<'a>) -> Result<'a, char> {
        todo!()
    }

    fn parse_named_char_literal<'a>(&self, input: Span<'a>) -> Result<'a, char> {
        value('\n', tag("newline"))(input)
    }

    fn parse_hex_char_literal<'a>(&self, input: Span<'a>) -> Result<'a, char> {
        todo!()
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::{Source, StringSource};

    #[test]
    fn test_read_character() {
        let parser = Parser::default();
        let mut source = src("");
        let source_type = source.source_type();

        source = src("#\\c");
        let mut datum = parser.parse(&mut source).unwrap();

        assert_eq!(datum.value, Value::character('c'));

        source = src("#\\☆");
        datum = parser.parse(&mut source).unwrap();
        assert_eq!(datum.value, Value::character('☆'));
    }

    fn src(inp: &str) -> impl Source {
        StringSource::new(inp, "datum-parser-test")
    }
}
