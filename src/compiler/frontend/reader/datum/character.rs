use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while_m_n;
use nom::character::complete::{anychar, char};
use nom::combinator::{map_opt, map_res, value};
use nom::error::context;
use nom::sequence::{delimited, preceded};

use super::{with_location, Input, ParseResult};
use crate::compiler::frontend::reader::datum::Datum;

/// Character parser
pub fn parse(input: Input) -> ParseResult<Datum> {
    let char_literal = preceded(
        tag("#\\"),
        alt((parse_hex_char_literal, parse_named_char_literal, anychar)),
    );

    with_location(char_literal, Datum::character)(input)
}

#[inline]
fn parse_hex_char_literal(input: Input) -> ParseResult<char> {
    preceded(char('x'), parse_hex_literal)(input)
}

fn parse_named_char_literal(input: Input) -> ParseResult<char> {
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

// parse a sequence of 3 bytes hex encoded
fn parse_hex_literal<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
    let parse_u32 = map_res(parse_hex, move |hex: Input<'a>| {
        u32::from_str_radix(hex.fragment(), 16)
    });

    map_opt(parse_u32, |value| std::char::from_u32(value))(input)
}

// shared for strings and symbols
pub fn parse_inline_hex_escape(input: Input) -> ParseResult<char> {
    context(
        "inline hex escape",
        delimited(tag("\\x"), parse_hex_literal, char(';')),
    )(input)
}

pub fn parse_mnemonic_escape(input: Input) -> ParseResult<char> {
    context(
        "mnemonic escape",
        preceded(
            char('\\'),
            alt((
                value('\n', char('n')),
                value('\r', char('r')),
                value('\u{7}', char('b')),
                value('\t', char('t')),
            )),
        ),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::reader::tests::*;

    #[test]
    fn test_read_char_hex_literal() {
        assert_parse_as("#\\x43", Datum::character('C', 0..5));
    }

    #[test]
    fn test_read_char_named_literal() {
        assert_parse_as("#\\alarm", Datum::character('\u{7}', 0..7));
    }

    #[test]
    fn test_read_char_literal() {
        assert_parse_as("#\\a", Datum::character('a', 0..3));

        assert_parse_as("#\\☆", Datum::character('☆', 0..5));
    }
}
