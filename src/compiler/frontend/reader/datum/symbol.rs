use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{anychar, char, one_of};
use nom::combinator::{map, value, verify};
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, pair, tuple};

use super::{
    character::{parse_inline_hex_escape, parse_mnemonic_escape},
    with_location, Input, ParseResult,
};

use crate::compiler::frontend::reader::datum::Datum;

//////////////////////////////////////////
// Identifier / Symbol
/////////////////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SymbolElement<'a> {
    Literal(&'a str),
    EscapedChar(char),
}

pub fn parse(input: Input) -> ParseResult<Datum> {
    let symbol_literal = alt((
        parse_identifier,
        parse_delimited_identifier,
        parse_peculiar_identifier,
    ));

    with_location(symbol_literal, Datum::symbol)(input)
}

fn parse_peculiar_identifier(input: Input) -> ParseResult<String> {
    let explicit_sign_str = map(parse_explicit_sign, String::from);

    alt((
        parse_peculiar_with_sign,
        parse_peculiar_with_sign_dot,
        parse_peculiar_with_dot,
        explicit_sign_str,
    ))(input)
}

fn parse_peculiar_with_sign(input: Input) -> ParseResult<String> {
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

fn parse_peculiar_with_dot(input: Input) -> ParseResult<String> {
    let (s, (dot, dot_subseq, subseq)) =
        tuple((char('.'), parse_dot_subsequent, many0(parse_subsequent)))(input)?;

    let mut symbol = String::from(dot);
    symbol.push(dot_subseq);
    symbol.extend(subseq.iter());

    Ok((s, symbol))
}

fn parse_peculiar_with_sign_dot(input: Input) -> ParseResult<String> {
    let (s, (sign, sign_sub, dot_subseq)) =
        tuple((parse_explicit_sign, char('.'), parse_dot_subsequent))(input)?;

    let mut symbol = String::from(sign);
    symbol.push(sign_sub);
    symbol.push(dot_subseq);

    Ok((s, symbol))
}

#[inline]
fn parse_dot_subsequent(input: Input) -> ParseResult<char> {
    alt((parse_sign_subsequent, char('.')))(input)
}

#[inline]
fn parse_sign_subsequent(input: Input) -> ParseResult<char> {
    alt((parse_initial, parse_explicit_sign, char('@')))(input)
}

fn parse_delimited_identifier(input: Input) -> ParseResult<String> {
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

fn parse_symbol_element(input: Input) -> ParseResult<SymbolElement> {
    let parse_symbol_escape = value('|', tag("\\|"));

    alt((
        map(parse_mnemonic_escape, SymbolElement::EscapedChar),
        map(parse_inline_hex_escape, SymbolElement::EscapedChar),
        map(parse_symbol_escape, SymbolElement::EscapedChar),
        map(parse_symbol_literal, SymbolElement::Literal),
    ))(input)
}

fn parse_symbol_literal(input: Input) -> ParseResult<&str> {
    let (s, v) = is_not("|\\")(input)?;

    Ok((s, v.fragment()))
}

fn parse_identifier(input: Input) -> ParseResult<String> {
    let mut identifier = String::new();
    let (s, (init, subseq)) = pair(parse_initial, many0(parse_subsequent))(input)?;

    identifier.push(init);
    identifier.extend(subseq.iter());

    Ok((s, identifier))
}

pub const SYMBOL_SPECIAL_INITIAL: &str = "!$%&*/:<=>?^_~";

fn parse_initial(input: Input) -> ParseResult<char> {
    let letter = verify(anychar, |c| c.is_alphabetic());
    let special_initial = one_of(SYMBOL_SPECIAL_INITIAL);

    alt((letter, special_initial))(input)
}

fn parse_subsequent(input: Input) -> ParseResult<char> {
    let digit = verify(anychar, |c| c.is_digit(10));
    let special_subsequent = alt((parse_explicit_sign, char('.'), char('@')));

    alt((parse_initial, digit, special_subsequent))(input)
}

#[inline]
fn parse_explicit_sign(input: Input) -> ParseResult<char> {
    alt((char('+'), char('-')))(input)
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::reader::tests::*;

    use super::*;

    #[test]
    fn test_read_symbol() {
        let symbols = vec![
            ("<=?", 0..3),
            ("->string", 0..8),
            ("a34kTMNs", 0..8),
            ("lambda", 0..6),
            ("list->vector", 0..12),
            ("q", 0..1),
            ("V17a", 0..4),
            ("the-word-recursion-has-many-meanings", 0..36),
        ];

        for sym in symbols.iter() {
            assert_parse_as(sym.0, Datum::symbol(sym.0, sym.1.clone()))
        }
    }

    #[test]
    fn test_read_symbol_delimited() {
        assert_parse_as("||", Datum::symbol("", 0..2));

        assert_parse_as("|two words|", Datum::symbol("two words", 0..11));
        assert_parse_as(r#"|two\x20;words|"#, Datum::symbol("two words", 0..15));
        assert_parse_as(r#"|two\|words|"#, Datum::symbol("two|words", 0..12));

        assert_parse_as(
            r#"|test with \| escaped vertical lines|"#,
            Datum::symbol("test with | escaped vertical lines", 0..37),
        );
        assert_parse_as(
            r#"|:(\x80;\xfff6;]&\x5c;"|"#,
            Datum::symbol(":(\u{0080}\u{fff6}]&\\\"", 0..24),
        );
    }
}
