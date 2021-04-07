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

// impl<I> ContextError<I> for Error {}

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

// impl<I> FromExternalError<I, std::io::Error> for Error {
//     fn from_external_error(_: I, _: nom::error::ErrorKind, e: std::io::Error) -> Self {
//         Error::IoError(e)
//     }
// }

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
    let variant_parser = parse_hex_literal;
    let (s, c) = preceded(tag("#\\"), variant_parser)(input)?;
    let datum = Datum::from_input(Value::character(c), &s);

    Ok((s, datum))
}

// parse a sequence of 3 bytes hex encoded
#[inline]
fn parse_hex_literal<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
    let parse_u32 = map_res(parse_hex, move |hex: Input<'a>| {
        u32::from_str_radix(hex.fragment(), 16)
    });

    map_opt(preceded(char('x'), parse_u32), |value| {
        std::char::from_u32(value)
    })(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::{Source, StringSource};

    #[test]
    fn test_read_char_hex_literal() {
        let mut source = src("#\\x43");
        let datum = parse(&mut source).unwrap();

        assert_eq!(datum.value, Value::character('C'));
    }

    fn src(inp: &str) -> impl Source {
        StringSource::new(inp, "datum-parser-test")
    }
}
