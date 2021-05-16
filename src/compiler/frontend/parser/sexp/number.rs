use super::datum::Datum;
use super::datum::Sexp;
use super::error::ReadError;
use crate::compiler::source::{Source, SourceType};
use crate::compiler::source_location::SourceLocation;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while_m_n};
use nom::character::complete::{anychar, char, line_ending, one_of};
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::error::{context, ErrorKind, ParseError, VerboseError};
use nom::multi::many_till;
use nom::multi::{fold_many0, many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;
use nom_locate::{position, LocatedSpan};

use super::Input;
use super::ParseResult;
use super::Result;

// Parse numbers
// R7RS 7.1.2
//
// <number> ->
//   <num 2> |
//   <num 8> |
//   <num 10> |
//   <num 16> |

pub fn parse_number<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    todo!()
}

#[cfg(test)]
mod tests {}
