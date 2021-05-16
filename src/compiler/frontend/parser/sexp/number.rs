use super::datum::Datum;
use super::datum::Sexp;
use super::map_datum;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1};
use nom::combinator::{map, map_res, opt, value};
use nom::multi::many1;
use nom::sequence::tuple;

use super::Input;
use super::ParseResult;

// Parse numbers
// R7RS 7.1.2
//
// <number> ->
//   <num 2> |
//   <num 8> |
//   <num 10> |
//   <num 16> |

pub fn parse<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let (s, pref) = parse_prefix(input)?;
    let (s, sign) = parse_sign(s)?;

    map_datum(parse_integer(&pref, sign), Sexp::integer)(s)
}

fn parse_integer<'a>(
    prefix: &Prefix,
    sign: Option<Sign>,
) -> impl FnMut(Input<'a>) -> ParseResult<'a, i64> {
    let number_parser = if prefix.radix == 2 {
        parse_integer_2
    } else {
        parse_integer_10
    };

    map(number_parser, move |n| {
        if sign == Some(Sign::Minus) {
            n * -1
        } else {
            n
        }
    })
}

fn parse_integer_10<'a>(input: Input<'a>) -> ParseResult<'a, i64> {
    map_res(digit1, move |d: Input<'a>| {
        i64::from_str_radix(d.fragment(), 10)
    })(input)
}

fn parse_integer_2<'a>(input: Input<'a>) -> ParseResult<'a, i64> {
    let binary_digits = many1(alt((char('0'), char('1'))));

    map_res(binary_digits, move |digits| {
        let s: String = digits.into_iter().collect();
        i64::from_str_radix(&s, 2)
    })(input)
}

pub struct Prefix {
    radix: u8,
    exactness: Option<Exactness>,
}

#[derive(Clone)]
pub enum Exactness {
    Inexact,
    Exact,
}

#[derive(Clone, PartialEq)]
pub enum Sign {
    Plus,
    Minus,
}

fn parse_prefix<'a>(input: Input<'a>) -> ParseResult<'a, Prefix> {
    alt((
        (map(tuple((parse_exactness, parse_radix)), |res| Prefix {
            radix: res.1,
            exactness: res.0,
        })),
        (map(tuple((parse_radix, parse_exactness)), |res| Prefix {
            radix: res.0,
            exactness: res.1,
        })),
    ))(input)
}

fn parse_radix<'a>(input: Input<'a>) -> ParseResult<'a, u8> {
    alt((
        value(2, tag("#b")),
        value(8, tag("#o")),
        value(16, tag("#x")),
        value(10, opt(tag("#d"))),
    ))(input)
}

fn parse_exactness<'a>(input: Input<'a>) -> ParseResult<'a, Option<Exactness>> {
    opt(alt((
        value(Exactness::Inexact, tag("#i")),
        value(Exactness::Exact, tag("#e")),
    )))(input)
}

pub fn parse_sign<'a>(input: Input<'a>) -> ParseResult<'a, Option<Sign>> {
    opt(alt((
        value(Sign::Plus, char('+')),
        value(Sign::Minus, char('-')),
    )))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::sexp::tests::*;

    #[test]
    fn parse_integer_10() {
        assert_parse_as("0", Sexp::integer(0));
        assert_parse_as("10", Sexp::integer(10));
        assert_parse_as("#d10", Sexp::integer(10));
        assert_parse_as("#e#d10", Sexp::integer(10));
        assert_parse_as("23434", Sexp::integer(23434));
        assert_parse_as("-23434", Sexp::integer(-23434));
    }

    #[test]
    fn parse_integer_2() {
        assert_parse_as("#b0", Sexp::integer(0));
        assert_parse_as("#b01011", Sexp::integer(11));
    }
}
