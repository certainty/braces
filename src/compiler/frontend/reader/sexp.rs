// Sexp parser and representation
//
// This module provides parsers for scheme sexps.
// The representation that is chosen here differs from the runtime representation of values, to optimise the parsing case.
// However it's still fully possible to convert Value -> Datum and vice versa

pub mod abbreviation;
pub mod boolean;
pub mod character;
pub mod datum;
pub mod list;
pub mod number;
pub mod string;
pub mod symbol;
pub mod whitespace;

use crate::compiler::source::{Location, SourceId};
use datum::{Datum, Sexp};
use nom::branch::alt;
use nom::combinator::value;
use nom::error::{context, Error};
use nom::sequence::preceded;
use nom::IResult;
use nom_locate::{position, LocatedSpan};

pub(crate) type Input<'a> = LocatedSpan<&'a str, SourceId>;

type ParseResult<'a, T> = IResult<Input<'a>, T, Error<Input<'a>>>;

// Parse a single datum from input
pub fn parse_datum<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
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

fn location<'a>(input: Input<'a>) -> Location {
    input
        .extra
        .location(input.location_offset()..input.fragment().len())
}

#[inline]
pub fn unit<'a, O, F>(parser: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, ()>
where
    F: FnMut(Input<'a>) -> ParseResult<'a, O>,
{
    value((), parser)
}
