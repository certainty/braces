use crate::compiler::source::{Source, SourceType};
use crate::compiler::source_location::SourceLocation;
use crate::vm::scheme::value::Value;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while_m_n};
use nom::character::complete::{anychar, char, multispace1};
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::error::{context, ContextError, ErrorKind, FromExternalError, ParseError};
use nom::multi::fold_many0;
use nom::sequence::{delimited, preceded};
use nom::Err;
use nom::IResult;
use nom_locate::{position, LocatedSpan};
use std::convert::From;
use thiserror::Error;

#[derive(Debug, PartialEq)]
pub struct Datum {
    pub location: Option<SourceLocation>,
    value: Value,
}

impl Datum {
    fn from_input<'a>(value: Value, input: &Input<'a>) -> Self {
        let loc = SourceLocation::new(
            input.extra.clone(),
            input.location_line() as usize,
            input.get_column(),
        );
        Datum {
            location: Some(loc),
            value,
        }
    }
}

/// Parser definition

type Input<'a> = LocatedSpan<&'a str, SourceType>;
type ParseResult<'a, T> = IResult<Input<'a>, T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IoError")]
    IoError(#[from] std::io::Error),
    #[error("ParseError")]
    Nom(ErrorKind),
    #[error("Input was incomplete")]
    Incomplete,
}

impl<I> ContextError<I> for Error {}

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

pub fn parse<'a, T: Source>(source: &'a mut T) -> std::result::Result<Datum, Error> {
    let source_type = source.source_type();
    let source_str = source.as_str()?;
    let input = Input::new_extra(source_str, source_type);
    match parse_datum(input) {
        Ok(result) => Ok(result.1),
        Err(nom::Err::Error(e)) => Err(e),
        Err(nom::Err::Failure(e)) => Err(e),
        Err(nom::Err::Incomplete(_)) => Err(Error::Incomplete),
    }
}

fn parse_datum<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    parse_character(input)
}

fn parse_character<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let variant_parser = alt((parse_hex_char_literal, parse_named_char_literal, anychar));
    let (s, c) = context("character", preceded(tag("#\\"), variant_parser))(input)?;

    let datum = Datum::from_input(Value::character(c), &s);
    Ok((s, datum))
}

#[inline]
fn parse_named_char_literal<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    alt((
        value(' ', tag("space")),
        value('\n', tag("newline")),
        value('\r', tag("return")),
        value('\t', tag("tab")),
        value('\u{7}', tag("alarm")),
        value('\u{0}', tag("null")),
        value('\u{8}', tag("backspace")),
        value('\u{18}', tag("delete")),
        value('\u{1b}', tag("escape")),
    ))(input)
}

#[inline]
fn parse_hex_char_literal<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    let (s, _) = char('x')(input)?;
    parse_hex_literal(s)
}

// parse a sequence of 3 bytes hex encoded
#[inline]
fn parse_hex_literal<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
    let parse_u32 = map_res(parse_hex, move |hex: Input<'a>| {
        u32::from_str_radix(hex.fragment(), 16)
    });

    map_opt(parse_u32, |value| std::char::from_u32(value))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::{Source, StringSource};

    #[test]
    fn test_read_char_hex_literal() {
        let datum = test_parse("#\\x43").unwrap();
        assert_eq!(datum.value, Value::character('C'));

        assert!(test_parse("\\xtrash").is_err(), "expected parse error");
    }

    #[test]
    fn test_read_char_named_literal() {
        let datum = test_parse("#\\alarm").unwrap();
        assert_eq!(datum.value, Value::character('\u{7}'));
    }

    #[test]
    fn test_read_char_literal() {
        let mut datum = test_parse("#\\a").unwrap();
        assert_eq!(datum.value, Value::character('a'));

        datum = test_parse("#\\☆").unwrap();
        assert_eq!(datum.value, Value::character('☆'));
    }

    fn test_parse(inp: &str) -> std::result::Result<Datum, Error> {
        parse(&mut StringSource::new(inp, "datum-parser-test"))
    }
}
