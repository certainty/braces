use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::character::complete::char;
use nom::combinator::{map, value};
use nom::error::{context, ErrorKind, ParseError};
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, preceded, terminated};

use super::{
    character::{parse_inline_hex_escape, parse_mnemonic_escape},
    whitespace::{consume_line_ending, parse_intra_line_ws},
};

use super::{map_datum, Input, ParseResult};
use crate::compiler::frontend::reader::{datum::Datum, sexp::SExpression};

//////////////////////////////
// String parser
//////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringElement<'a> {
    Literal(&'a str),
    EscapedChar(char),
    Continuation,
}

pub fn parse(input: Input) -> ParseResult<Datum> {
    let string_elements = fold_many0(
        parse_string_element,
        String::new(),
        |mut string, element| {
            match element {
                StringElement::Literal(s) => string.push_str(s),
                StringElement::EscapedChar(c) => string.push(c),
                StringElement::Continuation => string.push(' '),
            }
            string
        },
    );

    let string_literal = delimited(char('"'), string_elements, char('"'));

    map_datum(string_literal, SExpression::String)(input)
}

fn parse_string_element(input: Input) -> ParseResult<StringElement> {
    alt((
        map(parse_mnemonic_escape, StringElement::EscapedChar),
        map(parse_string_escape, StringElement::EscapedChar),
        value(StringElement::Continuation, parse_string_continuation),
        map(parse_inline_hex_escape, StringElement::EscapedChar),
        map(parse_string_literal, StringElement::Literal),
    ))(input)
}

fn parse_string_escape<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    context(
        "escaped character",
        preceded(
            char('\\'),
            alt((value('"', char('"')), value('\\', char('\\')))),
        ),
    )(input)
}

#[inline]
fn parse_string_continuation<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    let line_continuation = terminated(
        terminated(many0(parse_intra_line_ws), consume_line_ending),
        parse_intra_line_ws,
    );

    let (s, _) = preceded(char('\\'), line_continuation)(input)?;
    Ok((s, ()))
}

#[inline]
fn parse_string_literal(input: Input) -> ParseResult<&str> {
    let (s, v) = is_not("\\\"")(input)?;

    if v.fragment().is_empty() {
        Err(nom::Err::Error(nom::error::Error::from_error_kind(
            s,
            ErrorKind::Verify,
        )))
    } else {
        Ok((s, v.fragment()))
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::reader::tests::*;

    use super::*;

    #[test]
    fn test_read_string() {
        assert_parse_as(
            "\"this is my string\"",
            Datum::string("this is my string", 0..19),
        );

        assert_parse_as(
            "\"this is my ☆ string ☆\"",
            Datum::string("this is my ☆ string ☆", 0..27),
        );

        assert_parse_as(
            "\"string with \\n and \\t \"",
            Datum::string("string with \n and \t ", 0..24),
        );

        assert_parse_as(
            "\"string with \\xa; and \\t \"",
            Datum::string("string with \n and \t ", 0..26),
        );

        assert_parse_as(
            "\"string with \\\n and the\\\n next line\"",
            Datum::string("string with  and the next line", 0..36),
        );
    }

    #[test]
    fn test_read_string_bugs() {
        assert_parse_as("\"\"", Datum::string("", 0..2));
        assert_parse_as(r#""\\7""#, Datum::string("\\7", 0..5));
    }
}
