mod abbreviation;
mod boolean;
mod character;
pub mod datum;
pub mod error;
mod list;
mod number;
mod string;
pub mod symbol;
mod whitespace;

use super::Error;
use crate::compiler::source::{Source, SourceType};
use crate::compiler::source_location::SourceLocation;
use datum::Datum;
use datum::Sexp;
use nom::branch::alt;
use nom::combinator::value;
use nom::error::{context, VerboseError};
use nom::multi::many1;
use nom::sequence::preceded;
use nom::IResult;
use nom_locate::{position, LocatedSpan};

// See https://matklad.github.io/2018/06/06/modern-parser-generator.html for error recovery strategies

// TODO: Better error reporting strategy
// Every parser that returns a datum is a good candidate to be on a boundary
// for better errors. We should create a meaningful error if these parser fail
// and give enough context to be useful for the user. This requires a custom error
// type and not `VerboseError`

/// Parser definition
pub(crate) type Input<'a> = LocatedSpan<&'a str, SourceType>;
type ParseResult<'a, T> = IResult<Input<'a>, T, VerboseError<Input<'a>>>;

pub type Result<T> = std::result::Result<T, Error>;

pub fn parse<'a, T: Source>(source: &'a mut T) -> Result<Datum> {
    let source_type = source.source_type();
    let mut source_str = String::new();
    source.read_to_string(&mut source_str)?;
    let input = Input::new_extra(&source_str, source_type);
    let (_, datum) = parse_datum(input)?;

    Ok(datum)
}

pub fn parse_sequence<'a, T: Source>(source: &'a mut T) -> Result<Vec<Datum>> {
    let source_type = source.source_type();
    let mut source_str = String::new();
    source.read_to_string(&mut source_str)?;
    let input = Input::new_extra(&source_str, source_type);
    let (_rest, datum) = context("program", many1(parse_datum))(input)?;

    Ok(datum)
}

fn parse_datum<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let datum = context("datum", alt((parse_simple_datum, parse_compound_datum)));
    preceded(whitespace::parse_inter_token_space, datum)(input)
}

#[inline]
fn parse_simple_datum<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    context(
        "simple datum",
        alt((
            context("number", number::parse),
            context("character", character::parse),
            context("boolean", boolean::parse),
            context("symbol", symbol::parse),
            context("string", string::parse),
        )),
    )(input)
}

#[inline]
fn parse_compound_datum<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    context(
        "compund datum",
        alt((
            context("improper list", list::parse_improper_list),
            context("list", list::parse_proper_list),
            context("abbreviation", abbreviation::parse),
        )),
    )(input)
}

// Helper to create datum from a parser
pub fn map_datum<'a, O1, F, G>(
    mut first: F,
    mut second: G,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Datum>
where
    F: FnMut(Input<'a>) -> ParseResult<'a, O1>,
    G: FnMut(O1) -> Sexp,
{
    move |input: Input<'a>| {
        let (s, p) = position(input)?;
        let (s, v) = first(s)?;
        let value = second(v);
        let datum = Datum::new(value, location(p));
        Ok((s, datum))
    }
}

fn location<'a>(input: Input<'a>) -> SourceLocation {
    input
        .extra
        .location(input.location_line() as usize, input.get_column())
}

#[inline]
pub fn unit<'a, O, F>(parser: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, ()>
where
    F: FnMut(Input<'a>) -> ParseResult<'a, O>,
{
    value((), parser)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::StringSource;

    pub fn assert_parse_as(inp: &str, expected: Sexp) {
        let datum = parse(&mut StringSource::new(inp, "datum-parser-test")).unwrap();

        assert_eq!(datum.sexp, expected)
    }

    pub fn assert_parse_ok(inp: &str) {
        let mut source = StringSource::new(inp, "datum-parser-test");
        let parsed = parse(&mut source);

        assert!(parsed.is_ok(), "expected to parse successfully")
    }

    pub fn assert_parse_error(inp: &str) {
        let mut source = StringSource::new(inp, "datum-parser-test");
        let parsed = parse(&mut source);

        assert!(parsed.is_err(), "expected  parse error")
    }

    pub fn location(line: usize, col: usize) -> SourceLocation {
        SourceType::Buffer("datum-parser-test".to_string()).location(line, col)
    }

    pub fn make_datum(sexp: Sexp, line: usize, col: usize) -> Datum {
        Datum::new(sexp, location(line, col))
    }
}
