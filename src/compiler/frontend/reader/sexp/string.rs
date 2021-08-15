use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::character::complete::char;
use nom::combinator::{map, value};
use nom::error::{context, ErrorKind, ParseError, VerboseError};
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, preceded, terminated};

use crate::compiler::frontend::reader::{
    character::{parse_inline_hex_escape, parse_mnemonic_escape},
    Input,
    map_datum,
    ParseResult,
    whitespace::{consume_line_ending, parse_intra_line_ws}
};

use super::datum::{Datum, Sexp};

//////////////////////////////
// String parser
//////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringElement<'a> {
    Literal(&'a str),
    EscapedChar(char),
    Continuation,
}

pub fn parse<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
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

    map_datum(string_literal, Sexp::String)(input)
}

fn parse_string_element<'a>(input: Input<'a>) -> ParseResult<'a, StringElement<'a>> {
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
fn parse_string_literal<'a>(input: Input<'a>) -> ParseResult<'a, &'a str> {
    let (s, v) = is_not("\\\"")(input)?;

    if v.fragment().is_empty() {
        Err(nom::Err::Error(VerboseError::from_error_kind(
            s,
            ErrorKind::Verify,
        )))
    } else {
        Ok((s, v.fragment()))
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::parser::sexp::tests::*;

    use super::*;

    #[test]
    fn test_read_string() {
        assert_parse_as("\"this is my string\"", Sexp::string("this is my string"));

        assert_parse_as(
            "\"this is my ☆ string ☆\"",
            Sexp::string("this is my ☆ string ☆"),
        );

        assert_parse_as(
            "\"string with \\n and \\t \"",
            Sexp::string("string with \n and \t "),
        );

        assert_parse_as(
            "\"string with \\xa; and \\t \"",
            Sexp::string("string with \n and \t "),
        );

        assert_parse_as(
            "\"string with \\\n and the\\\n next line\"",
            Sexp::string("string with  and the next line"),
        );
    }

    #[test]
    fn test_read_string_bugs() {
        assert_parse_as("\"\"", Sexp::string(""));
        assert_parse_as(r#""\\7""#, Sexp::string("\\7"));
    }
}
