use crate::compiler::source::{Source, SourceType};
use crate::compiler::source_location::SourceLocation;
use crate::vm::scheme::value::Value;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while_m_n};
use nom::character::complete::{anychar, char, multispace1, none_of};
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::error::{
    context, ContextError, ErrorKind, FromExternalError, ParseError, VerboseError, VerboseErrorKind,
};
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, preceded, terminated};
use nom::Err;
use nom::IResult;
use nom_locate::{position, LocatedSpan};
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
type ParseResult<'a, T> = IResult<Input<'a>, T, VerboseError<Input<'a>>>;

#[derive(Debug, Error)]
pub enum Error<'a> {
    #[error("IoError")]
    IoError(#[from] std::io::Error),
    #[error("ParseError")]
    ParseError(VerboseError<Input<'a>>),
    #[error("Nom Error")]
    Nom(ErrorKind),
    #[error("Input was incomplete")]
    Incomplete,
}

impl<'a, I> ContextError<I> for Error<'a> {}

impl<'a, I> ParseError<I> for Error<'a> {
    fn from_error_kind(_: I, kind: ErrorKind) -> Self {
        Error::Nom(kind)
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}

impl<'a, I> FromExternalError<I, std::num::ParseIntError> for Error<'a> {
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
        Err(nom::Err::Error(e)) => {
            println!("{:#?}", e);
            Err(Error::ParseError(e))
        }
        Err(nom::Err::Failure(e)) => {
            println!("{:#?}", e);
            Err(Error::ParseError(e))
        }
        Err(nom::Err::Incomplete(_)) => Err(Error::Incomplete),
    }
}

fn parse_datum<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    context(
        "datum",
        alt((
            context("boolean", parse_boolean),
            context("character", parse_character),
            context("string", parse_string),
        )),
    )(input)
}

// Boolean parser
fn parse_boolean<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let (s, v) = alt((
        value(true, tag("#t")),
        value(true, tag("#true")),
        value(false, tag("#f")),
        value(false, tag("#false")),
    ))(input)?;

    let datum = Datum::from_input(Value::boolean(v), &s);
    Ok((s, datum))
}

// Character parser
fn parse_character<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let variant_parser = alt((parse_hex_char_literal, parse_named_char_literal, anychar));
    let (s, c) = preceded(tag("#\\"), variant_parser)(input)?;

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
    preceded(char('x'), parse_hex_char_literal)(input)
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

//////////////////////////////
// String parser
//////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringElement<'a> {
    Literal(&'a str),
    EscapedChar(char),
    Continuation,
}

fn parse_string<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let build_string = fold_many0(
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

    let (s, string) = delimited(char('"'), build_string, char('"'))(input)?;
    let datum = Datum::from_input(Value::string(string), &s);

    Ok((s, datum))
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

fn parse_mnemonic_escape<'a>(input: Input<'a>) -> ParseResult<'a, char> {
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

#[inline]
fn parse_inline_hex_escape<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    context(
        "inline hex escape",
        delimited(tag("\\x"), parse_hex_literal, char(';')),
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

fn consume_line_ending<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    value((), alt((tag("\r\n"), tag("\n"), tag("\r"))))(input)
}

fn parse_intra_line_ws<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    value((), alt((char(' '), char(' '))))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::StringSource;

    #[test]
    fn test_read_boolean_literal() {
        assert_parse_as("#t", Value::boolean(true));
        assert_parse_as("#true", Value::boolean(true));

        assert_parse_as("#f", Value::boolean(false));
        assert_parse_as("#false", Value::boolean(false));
    }

    #[test]
    fn test_read_char_hex_literal() {
        assert_parse_as("#\\x43", Value::character('C'));
    }

    #[test]
    fn test_read_char_named_literal() {
        assert_parse_as("#\\alarm", Value::character('\u{7}'));
    }

    #[test]
    fn test_read_char_literal() {
        assert_parse_as("#\\a", Value::character('a'));

        assert_parse_as("#\\☆", Value::character('☆'));
    }

    #[test]
    fn test_read_string() {
        assert_parse_as("\"this is my string\"", Value::string("this is my string"));

        assert_parse_as(
            "\"this is my ☆ string ☆\"",
            Value::string("this is my ☆ string ☆"),
        );

        assert_parse_as(
            "\"string with \\n and \\t \"",
            Value::string("string with \n and \t "),
        );

        assert_parse_as(
            "\"string with \\xa; and \\t \"",
            Value::string("string with \n and \t "),
        );

        assert_parse_as(
            "\"string with \\\n and the\\\n next line\"",
            Value::string("string with  and the next line"),
        );
    }

    #[test]
    fn test_read_string_bugs() {
        assert_parse_as("\"\"", Value::string(""));
        assert_parse_ok("\"–)ꍽ[\u{83}\u{2}\u{94}\u{10}\u{1e}(\u{9f}\u{94}\t^+\u{fff5}\u{2003}JX}]\u{9f}VL%®\u{81}{e@8\u{2}\u{9c}{\u{83}\u{1b}\\7/O^7x\u{19}v¤ᣋ\u{9a}^~§\u{83}02x!)\u{3b19f}f}\u{8d}>5\u{8c}{}\u{52bf9}\u{1f}徒1\u{c73ef}骍<\u{1a}v^t\u{95}\u{92}6l쏊b\u{10fffe}\\015\u{0}¯8\u{8}\"");
    }

    fn assert_parse_as(inp: &str, expected: Value) {
        let datum = parse(&mut StringSource::new(inp, "datum-parser-test")).unwrap();

        assert_eq!(datum.value, expected)
    }

    fn assert_parse_ok(inp: &str) {
        let parsed = parse(&mut StringSource::new(inp, "datum-parser-test")).unwrap();

        assert!(false, "expected to parse successfully")
    }
}
