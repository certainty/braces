use crate::compiler::source::{Source, SourceType};
use crate::compiler::source_location::SourceLocation;
use crate::vm::scheme::value::Value;
use nom::branch::alt;
use nom::bytes::streaming::{is_not, tag, take_while_m_n};
use nom::character::streaming::{char, multispace1};
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::error::{FromExternalError, ParseError};
use nom::multi::fold_many0;
use nom::sequence::{delimited, preceded};
use nom::IResult;
use nom_locate::LocatedSpan;

struct Error {}

type Result<'a, T> = IResult<Span<'a>, T, Error>;

type Span<'a> = LocatedSpan<&'a str>;

struct Datum<'a> {
    pub position: Span<'a>,
    value: Value,
}

/// R7RS
impl<'a> Datum<'a> {
    fn with_span(value: Value, span: &Span<'a>) -> Self {
        Datum {
            position: *span,
            value,
        }
    }
}

/// r7rs compliant parser
struct Parser {}

impl Parser {
    fn parse_char<'a>() -> Result<'a, Value> {
        todo!()
    }

    fn parse_char_literal<'a>() -> Result<'a, char> {
        todo!()
    }

    fn parse_named_char_literal<'a>(input: Span<'a>) -> Result<'a, char> {
        value('\n', tag("newline"))(input)
    }

    fn parse_hex_char_literal<'a>() -> Result<'a, char> {
        todo!()
    }
}
