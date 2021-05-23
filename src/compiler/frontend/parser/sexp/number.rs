use super::datum::Sexp;
use super::datum::{Datum, RealNumber};
use super::datum::{Exactness, Sign};
use super::map_datum;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::one_of;
use nom::character::complete::{char, digit1};
use nom::combinator::{map, map_res, opt, value};
use nom::multi::{many0, many1};
use nom::sequence::tuple;
use num::BigInt;

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
//
// <num R> -> <prefix R> <complex R>
//
// We derive the parse slightly differently.
// <complex R> expands into productions for reals (rational, integer, decimal)
// which we represent as parsers (and in fact use in the complex parser). However
// we never construct only complex numbers if the number parser succeeds.
// Instead we create either: Complex, FixNum, Flownum or Rational

pub fn parse<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let (s, pref) = parse_prefix(input)?;

    map_datum(parse_real(pref.radix), Sexp::real)(s)
}

//
// <complex R> ->
//    <real R>               |
//    <real R> @ <real R>    |
//    <real R> + <ureal R> i |
//    <real R> - <ureal R> i |
//    <real R> + i           |
//    <real R> - i           |
//    <real R> <infnan> i    |
//    + <ureal R> i          |
//    - <ureal R> i          |
//    <infnan> i             |
//    + i                    |
//    - i                    |
//
//

//
// <real R>   -> <sign> <ureal R> | infnan
// <ureal R> -> <uinteger R> | <uninteger R> / <uninteger R> | <decimal R>
//
//
fn parse_real<'a>(radix: u8) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    move |input| alt((parse_signed_real(radix), parse_inf_nan))(input)
}

fn parse_signed_real<'a>(radix: u8) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    let ureal = alt((parse_uinteger(radix), parse_decimal(radix)));
    let signed_ureal = tuple((parse_sign, ureal));

    map(signed_ureal, |result: (Sign, RealNumber)| {
        result.1.sign(&result.0)
    })
}

fn parse_inf_nan<'a>(input: Input<'a>) -> ParseResult<'a, RealNumber> {
    alt((
        value(RealNumber::Flonum(num::Float::nan()), tag("+nan.0")),
        value(RealNumber::Flonum(num::Float::nan()), tag("-nan.0")),
        value(RealNumber::Flonum(num::Float::infinity()), tag("+inf.0")),
        value(
            RealNumber::Flonum(num::Float::neg_infinity()),
            tag("-inf.0"),
        ),
    ))(input)
}

// <uinteger R> -> <digit R>+
fn parse_uinteger<'a>(radix: u8) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    parse_digits(radix)
}

// decimal ->
//   <uniteger 10> <suffix> |
//   . <digit 10>+ <suffix> |
//   <digit 10>+ . <digit 10>* suffix
fn parse_decimal<'a>(radix: u8) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    // TODO: put into own parsers that ma the result correctly
    let uint_suffix = tuple((parse_uinteger(10), parse_suffix));
    let dot_digits = tuple((char('.'), many1(_parse_digits(10)), parse_suffix));
    let digit_dot_digits = tuple((
        many1(_parse_digits(10)),
        char('.'),
        many0(_parse_digits(10)),
        parse_suffix,
    ));
}

fn parse_decimal_no_dot<'a>(input: Input<'a>) -> ParseResult<'a, f64> {
    let (pref, suff) = tuple((parse_uinteger(10), parse_suffix))(input)?;
    //let number = f64::from_str(format!("{}e{}", pref, suff))
}

fn parse_digits<'a>(radix: u8) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    map_res(many1(_parse_digits(radix)), move |digits| {
        let s: String = digits.into_iter().collect();
        match BigInt::parse_bytes(s.as_bytes(), radix as u32) {
            None => Err(anyhow!("Can't parse integer with base {}", radix)),
            Some(v) => Ok(RealNumber::Fixnum(v)),
        }
    })
}

fn _parse_digits<'a>(radix: u8) -> impl FnMut(Input<'a>) -> ParseResult<'a, char> {
    let digits = match radix {
        2 => "01",
        8 => "01234567",
        16 => "012345679abcdef",
        _ => "0123456789",
    };

    one_of(digits)
}
// returns the exponent if present
fn parse_suffix<'a>(input: Input<'a>) -> ParseResult<'a, Option<i64>> {
    let (s, result) = opt(tuple((char('e'), parse_sign, many1(_parse_digits(10)))))(input)?;

    match result {
        Some((_, sign, digits)) => {
            let s: String = digits.into_iter().collect();
            let exp = s.parse::<i64>().or(Err(anyhow!("Can't parse number")))?;
            let signed_exp = match sign {
                Sign::Minus => exp * -1,
                _ => exp,
            };
            Ok((s, Some(signed_exp)))
        }
        None => Ok((s, None)),
    }
}

struct Prefix {
    radix: u8,
    exactness: Option<Exactness>,
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

pub fn parse_sign<'a>(input: Input<'a>) -> ParseResult<'a, Sign> {
    let (s, sign) = opt(alt((
        value(Sign::Plus, char('+')),
        value(Sign::Minus, char('-')),
    )))(input)?;

    Ok((s, sign.unwrap_or(Sign::Plus)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::sexp::tests::*;

    #[test]
    fn parse_integer_10() {
        assert_parse_as("0", Sexp::integer(BigInt::from(0)));
        assert_parse_as("10", Sexp::integer(BigInt::from(10)));
        assert_parse_as("#d10", Sexp::integer(BigInt::from(10)));
        assert_parse_as("#e#d10", Sexp::integer(BigInt::from(10)));
        assert_parse_as("23434", Sexp::integer(BigInt::from(23434)));
        assert_parse_as("-23434", Sexp::integer(BigInt::from(-23434)));
    }

    #[test]
    fn parse_integer_2() {
        assert_parse_as("#b0", Sexp::integer(BigInt::from(0)));
        assert_parse_as("#b01011", Sexp::integer(BigInt::from(11)));
    }

    #[test]
    fn parse_integer_8() {
        assert_parse_as("#o777", Sexp::integer(BigInt::from(511)));
        assert_parse_as("#o0775", Sexp::integer(BigInt::from(509)));

        assert_parse_error("#o8989")
    }
}
