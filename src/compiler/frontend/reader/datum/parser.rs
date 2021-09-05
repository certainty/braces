use nom::branch::alt;
use nom::combinator::value;
use nom::error::{context, Error};
use nom::sequence::preceded;
use nom::IResult;
use nom_locate::{position, LocatedSpan};

use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::source::{Location, SourceId};

use super::{
    abbreviation, boolean, byte_vector, character, list, number, string, symbol, vector, whitespace,
};

pub(crate) type Input<'a> = LocatedSpan<&'a str, SourceId>;

pub type ParseResult<'a, T> = IResult<Input<'a>, T, Error<Input<'a>>>;

// Parse a single datum from input
pub fn parse_datum(input: Input) -> ParseResult<Datum> {
    let datum = context("datum", alt((parse_simple_datum, parse_compound_datum)));
    preceded(whitespace::parse_inter_token_space, datum)(input)
}

#[inline]
fn parse_simple_datum(input: Input) -> ParseResult<Datum> {
    context(
        "simple datum",
        alt((
            context("number", number::parse),
            context("character", character::parse),
            context("boolean", boolean::parse),
            context("symbol", symbol::parse),
            context("string", string::parse),
            context("byte-vector", byte_vector::parse),
        )),
    )(input)
}

#[inline]
fn parse_compound_datum(input: Input) -> ParseResult<Datum> {
    context(
        "compound datum",
        alt((
            context("vector", vector::parse_vector),
            context("improper list", list::parse_improper_list),
            context("list", list::parse_proper_list),
            context("abbreviation", abbreviation::parse),
        )),
    )(input)
}

// Helper to create datum from a parser
pub fn with_location<'a, O1, F, G>(
    mut parser: F,
    mut datum_builder: G,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, Datum>
where
    F: FnMut(Input<'a>) -> ParseResult<'a, O1>,
    G: FnMut(O1, Location) -> Datum,
{
    move |input: Input<'a>| {
        let (s, p) = position(input)?;
        let (s2, v) = parser(s)?;
        let (s3, p2) = position(s2)?;
        let loc = input
            .extra
            .location(p.location_offset()..p2.location_offset());
        let datum = datum_builder(v, loc);
        Ok((s3, datum))
    }
}

#[inline]
pub fn unit<'a, O, F>(parser: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, ()>
where
    F: FnMut(Input<'a>) -> ParseResult<'a, O>,
{
    value((), parser)
}
