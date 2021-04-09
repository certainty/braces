pub mod datum;
pub mod error;

use crate::compiler::source::{Source, SourceType};
use crate::compiler::source_location::SourceLocation;
use crate::vm::scheme::value::Value;
use datum::Datum;
use error::ReadError;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while_m_n};
use nom::character::complete::{anychar, char, line_ending, one_of};
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::error::{context, ErrorKind, ParseError, VerboseError};
use nom::multi::many_till;
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;
use nom_locate::{position, LocatedSpan};

/// Parser definition
pub(crate) type Input<'a> = LocatedSpan<&'a str, SourceType>;
type ParseResult<'a, T> = IResult<Input<'a>, T, VerboseError<Input<'a>>>;

pub type Result<T> = std::result::Result<T, ReadError>;

pub fn parse<'a, T: Source>(source: &'a mut T) -> Result<Datum> {
    let source_type = source.source_type();
    let source_str = source.as_str()?;
    let input = Input::new_extra(source_str, source_type);
    let (_, datum) = parse_datum(input)?;

    Ok(datum)
}

fn parse_datum<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let datum = context("datum", alt((parse_simple_datum, parse_compound_datum)));

    preceded(parse_inter_token_space, datum)(input)
}

#[inline]
fn parse_simple_datum<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    context(
        "simple datum",
        alt((
            context("character", parse_character),
            context("boolean", parse_boolean),
            context("symbol", parse_symbol),
            context("string", parse_string),
        )),
    )(input)
}

#[inline]
fn parse_compound_datum<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    context(
        "compund datum",
        alt((
            context("list", parse_proper_list),
            context("abbreviation", parse_abbreviation),
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
    G: FnMut(O1) -> Value,
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
    SourceLocation::new(
        input.extra.clone(),
        input.location_line() as usize,
        input.get_column(),
    )
}

#[inline]
pub fn unit<'a, O, F>(parser: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, ()>
where
    F: FnMut(Input<'a>) -> ParseResult<'a, O>,
{
    value((), parser)
}

////////////////////////
// Boolean parser
////////////////////////

fn parse_boolean<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let bool_literal = alt((
        value(true, tag("#t")),
        value(true, tag("#true")),
        value(false, tag("#f")),
        value(false, tag("#false")),
    ));

    map_datum(bool_literal, Value::boolean)(input)
}

////////////////////////
// Character parser
////////////////////////

fn parse_character<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let char_literal = preceded(
        tag("#\\"),
        alt((parse_hex_char_literal, parse_named_char_literal, anychar)),
    );

    map_datum(char_literal, Value::character)(input)
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
    preceded(char('x'), parse_hex_literal)(input)
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

    map_datum(string_literal, Value::String)(input)
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

//////////////////////////////////////////
// Identifier / Symbol
/////////////////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SymbolElement<'a> {
    Literal(&'a str),
    EscapedChar(char),
}

fn parse_symbol<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let symbol_literal = alt((
        parse_identifier,
        parse_delimited_identifier,
        parse_peculiar_identifier,
    ));

    map_datum(symbol_literal, Value::symbol)(input)
}

#[inline]
fn parse_peculiar_identifier<'a>(input: Input<'a>) -> ParseResult<'a, String> {
    let explicit_sign_str = map(parse_explicit_sign, String::from);

    alt((
        parse_peculiar_with_sign,
        parse_peculiar_with_sign_dot,
        parse_peculiar_with_dot,
        explicit_sign_str,
    ))(input)
}

#[inline]
fn parse_peculiar_with_sign<'a>(input: Input<'a>) -> ParseResult<'a, String> {
    let (s, (sign, sign_sub, subseq)) = tuple((
        parse_explicit_sign,
        parse_sign_subsequent,
        many0(parse_sign_subsequent),
    ))(input)?;

    let mut symbol = String::from(sign);
    symbol.push(sign_sub);
    symbol.extend(subseq.iter());

    Ok((s, symbol))
}

#[inline]
fn parse_peculiar_with_dot<'a>(input: Input<'a>) -> ParseResult<'a, String> {
    let (s, (dot, dot_subseq, subseq)) =
        tuple((char('.'), parse_dot_subsequent, many0(parse_subsequent)))(input)?;

    let mut symbol = String::from(dot);
    symbol.push(dot_subseq);
    symbol.extend(subseq.iter());

    Ok((s, symbol))
}

#[inline]
fn parse_peculiar_with_sign_dot<'a>(input: Input<'a>) -> ParseResult<'a, String> {
    let (s, (sign, sign_sub, dot_subseq)) =
        tuple((parse_explicit_sign, char('.'), parse_dot_subsequent))(input)?;

    let mut symbol = String::from(sign);
    symbol.push(sign_sub);
    symbol.push(dot_subseq);

    Ok((s, symbol))
}

#[inline]
fn parse_dot_subsequent<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    alt((parse_sign_subsequent, char('.')))(input)
}

#[inline]
fn parse_sign_subsequent<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    alt((parse_initial, parse_explicit_sign, char('@')))(input)
}

#[inline]
fn parse_delimited_identifier<'a>(input: Input<'a>) -> ParseResult<'a, String> {
    let symbol_elements = fold_many0(
        parse_symbol_element,
        String::new(),
        |mut string, element| {
            match element {
                SymbolElement::Literal(s) => string.push_str(s),
                SymbolElement::EscapedChar(c) => string.push(c),
            }
            string
        },
    );

    delimited(char('|'), symbol_elements, char('|'))(input)
}

#[inline]
fn parse_symbol_element<'a>(input: Input<'a>) -> ParseResult<'a, SymbolElement<'a>> {
    let parse_symbol_escape = value('|', tag("\\|"));

    alt((
        map(parse_mnemonic_escape, SymbolElement::EscapedChar),
        map(parse_inline_hex_escape, SymbolElement::EscapedChar),
        map(parse_symbol_escape, SymbolElement::EscapedChar),
        map(parse_symbol_literal, SymbolElement::Literal),
    ))(input)
}

#[inline]
fn parse_symbol_literal<'a>(input: Input<'a>) -> ParseResult<'a, &'a str> {
    let (s, v) = is_not("|\\")(input)?;

    Ok((s, v.fragment()))
}

fn parse_identifier<'a>(input: Input<'a>) -> ParseResult<'a, String> {
    let mut identifier = String::new();
    let (s, (init, subseq)) = pair(parse_initial, many0(parse_subsequent))(input)?;

    identifier.push(init);
    identifier.extend(subseq.iter());

    Ok((s, identifier))
}

#[inline]
fn parse_initial<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    let letter = verify(anychar, |c| c.is_alphabetic());
    let special_initial = one_of("!$%&*/:<=>?^_~");

    alt((letter, special_initial))(input)
}

#[inline]
fn parse_subsequent<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    let digit = verify(anychar, |c| c.is_digit(10));
    let special_subsequent = alt((parse_explicit_sign, char('.'), char('@')));

    alt((parse_initial, digit, special_subsequent))(input)
}

#[inline]
fn parse_explicit_sign<'a>(input: Input<'a>) -> ParseResult<'a, char> {
    alt((char('+'), char('-')))(input)
}

/////////////////////////////
// proper list
/////////////////////////////

#[inline]
fn parse_proper_list<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let list_elements = delimited(
        parse_inter_token_space,
        parse_datum,
        parse_inter_token_space,
    );
    let list = delimited(char('('), many0(list_elements), char(')'));

    map_datum(list, |elts| {
        Value::proper_list(elts.iter().map(|e| e.value.clone()).collect())
    })(input)
}

////////////////////////////
// abbreviation
////////////////////////////

#[inline]
fn parse_abbreviation<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let abbrev = pair(parse_abbrev_prefix, parse_datum);

    map_datum(abbrev, |(abbr, datum)| {
        Value::proper_list(vec![abbr.value, datum.value])
    })(input)
}

#[inline]
fn parse_abbrev_prefix<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let abbrev = alt((
        value(Value::symbol("quote"), char('\'')),
        value(Value::symbol("quasi-quote"), char('`')),
        value(Value::symbol("unquote-splicing"), tag(",@")),
        value(Value::symbol("unquote"), char(',')),
    ));

    map_datum(abbrev, |v| v)(input)
}

#[inline]
fn consume_line_ending<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    unit(line_ending)(input)
}

#[inline]
fn parse_intra_line_ws<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    unit(alt((char(' '), char(' '))))(input)
}

#[inline]
fn parse_inter_token_space<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    let atmosphere = alt((parse_white_space, parse_comment, parse_directive));
    unit(many0(atmosphere))(input)
}

#[inline]
fn parse_white_space<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    alt((parse_intra_line_ws, consume_line_ending))(input)
}

#[inline]
fn parse_comment<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    context(
        "comment",
        unit(alt((
            parse_line_comment,
            parse_nested_comment,
            parse_inline_comment,
        ))),
    )(input)
}

#[inline]
fn parse_nested_comment<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    let comment_text = many0(anychar);
    let nested_comment = delimited(tag("#|"), comment_text, tag("|#"));
    context("nested comment", unit(nested_comment))(input)
}

#[inline]
fn parse_line_comment<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    unit(preceded(char(';'), many_till(anychar, line_ending)))(input)
}

#[inline]
fn parse_inline_comment<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    unit(preceded(
        tag("#;"),
        preceded(parse_inter_token_space, parse_datum),
    ))(input)
}

#[inline]
fn parse_directive<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    unit(alt((tag("#!fold-case"), tag("#!no-fold-case"))))(input)
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
        assert_parse_as(r#""\\7""#, Value::string("\\7"));
    }

    #[test]
    fn test_read_symbol() {
        let symbols = vec![
            "<=?",
            "->string",
            "a34kTMNs",
            "lambda",
            "list->vector",
            "q",
            "V17a",
            "the-word-recursion-has-many-meanings",
        ];

        for sym in symbols.iter() {
            assert_parse_as(sym, Value::symbol(*sym))
        }
    }

    #[test]
    fn test_read_symbol_delimited() {
        assert_parse_as("||", Value::symbol(""));

        assert_parse_as("|two words|", Value::symbol("two words"));
        assert_parse_as(r#"|two\x20;words|"#, Value::symbol("two words"));
        assert_parse_as(r#"|two\|words|"#, Value::symbol("two|words"));

        assert_parse_as(
            r#"|test with \| escaped vertical lines|"#,
            Value::symbol("test with | escaped vertical lines"),
        );
        assert_parse_as(
            r#"|:(\x80;\xfff6;]&\x5c;"|"#,
            Value::symbol(":(\u{0080}\u{fff6}]&\\\""),
        );
    }

    #[test]
    fn test_read_abbrev() {
        assert_parse_as(
            "'foo",
            Value::proper_list(vec![Value::symbol("quote"), Value::symbol("foo")]),
        );
        assert_parse_as(
            ",foo",
            Value::proper_list(vec![Value::symbol("unquote"), Value::symbol("foo")]),
        );

        assert_parse_as(
            "`foo",
            Value::proper_list(vec![Value::symbol("quasi-quote"), Value::symbol("foo")]),
        );

        assert_parse_as(
            ",@foo",
            Value::proper_list(vec![
                Value::symbol("unquote-splicing"),
                Value::symbol("foo"),
            ]),
        );
    }

    #[test]
    fn test_read_proper_list() {
        assert_parse_as(
            "(#t    #f)",
            Value::proper_list(vec![Value::boolean(true), Value::boolean(false)]),
        );

        assert_parse_as("()", Value::proper_list(vec![]));

        assert_parse_as(
            "((foo #t))",
            Value::proper_list(vec![Value::proper_list(vec![
                Value::symbol("foo"),
                Value::boolean(true),
            ])]),
        );
    }

    #[test]
    fn test_read_comments() {
        assert_parse_as(";foo bar\n #t", Value::boolean(true));
        assert_parse_as(
            "(#t \n #; foo\n #f)",
            Value::proper_list(vec![Value::boolean(true), Value::boolean(false)]),
        );

        //TODO: fix me
        // assert_parse_as(
        //     "#| this is a nested comment\n\n\n followed by more comments\n|# #t",
        //     Value::boolean(true),
        // );
    }

    fn assert_parse_as(inp: &str, expected: Value) {
        let datum = parse(&mut StringSource::new(inp, "datum-parser-test")).unwrap();

        assert_eq!(datum.value, expected)
    }

    fn assert_parse_ok(inp: &str) {
        let mut source = StringSource::new(inp, "datum-parser-test");
        let parsed = parse(&mut source);

        assert!(parsed.is_ok(), "expected to parse successfully")
    }
}