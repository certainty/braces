use super::datum::Datum;
use super::datum::Sexp;
use super::datum::{Exactness, Sign};
use super::map_datum;
use crate::vm::value::number::{Fixnum, Flonum, Number, RealNumber};
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

    map_datum(parse_real(pref.radix), Sexp::number)(s)
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
    let ureal = alt((parse_decimal, parse_rational(radix), parse_uinteger(radix)));
    let signed_ureal = tuple((parse_sign, ureal));

    map(signed_ureal, |result: (Sign, RealNumber)| {
        result.1.sign(&result.0)
    })
}

fn parse_inf_nan<'a>(input: Input<'a>) -> ParseResult<'a, RealNumber> {
    alt((
        value(RealNumber::Flonum(Flonum::nan()), tag("+nan.0")),
        value(RealNumber::Flonum(Flonum::nan()), tag("-nan.0")),
        value(RealNumber::Flonum(Flonum::infinity()), tag("+inf.0")),
        value(RealNumber::Flonum(Flonum::neg_infinity()), tag("-inf.0")),
    ))(input)
}

// <uinteger R> -> <digit R>+
fn parse_uinteger<'a>(radix: u8) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    map(parse_digits(radix), RealNumber::Fixnum)
}

fn _parse_u64<'a, P>(digits: P) -> impl FnMut(Input<'a>) -> ParseResult<'a, u64>
where
    P: FnMut(Input<'a>) -> ParseResult<'a, Vec<char>>,
{
    map(digits, move |num| {
        if num.len() == 0 {
            0
        } else {
            let nums: String = num.into_iter().collect();
            nums.parse::<u64>().expect("Parsing u64 can't fail")
        }
    })
}

// <rational R> -> <uinteger R> / <uinteger R>

fn parse_rational<'a>(radix: u8) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    map(
        tuple((parse_digits(radix), char('/'), parse_digits(radix))),
        |(numer, _, denom)| RealNumber::Rational(num::BigRational::from((numer, denom))),
    )
}

// decimal ->
//   <uniteger 10> <suffix> |
//   . <digit 10>+ <suffix> |
//   <digit 10>+ . <digit 10>* suffix

fn parse_decimal<'a>(input: Input<'a>) -> ParseResult<'a, RealNumber> {
    map(alt((parse_decimal_short, parse_decimal_full)), |num| {
        RealNumber::Flonum(num)
    })(input)
}

fn parse_decimal_full<'a>(input: Input<'a>) -> ParseResult<'a, f64> {
    let (s, (pref, _, decimal_places, exp)) = tuple((
        _parse_u64(many1(_parse_digits(10))),
        char('.'),
        _parse_u64(many0(_parse_digits(10))),
        parse_suffix,
    ))(input)?;

    let decimal = format!("{}.{}", pref, decimal_places)
        .parse::<f64>()
        .expect("Parse f64 can't fail");

    Ok((s, apply_exponent(decimal, exp)))
}

fn parse_decimal_short<'a>(input: Input<'a>) -> ParseResult<'a, f64> {
    let (s, (_, decimal_places, exp)) = tuple((
        char('.'),
        _parse_u64(many1(_parse_digits(10))),
        parse_suffix,
    ))(input)?;

    let decimal = format!("0.{}", decimal_places)
        .parse::<f64>()
        .expect("Parse f64 can't fail");

    Ok((s, apply_exponent(decimal, exp)))
}

fn apply_exponent(num: f64, exp: Option<i32>) -> f64 {
    match exp {
        Some(e) => (num as f64) * (f64::powi(10.0, e) as f64),
        _ => (num as f64),
    }
}

fn parse_digits<'a>(radix: u8) -> impl FnMut(Input<'a>) -> ParseResult<'a, BigInt> {
    map(many1(_parse_digits(radix)), move |digits| {
        let digits: String = digits.into_iter().collect();
        BigInt::parse_bytes(digits.as_bytes(), radix as u32)
            .expect("BigInt parse digits can't fail")
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
fn parse_suffix<'a>(input: Input<'a>) -> ParseResult<'a, Option<i32>> {
    let (s, result) = opt(tuple((char('e'), parse_sign, many1(_parse_digits(10)))))(input)?;

    match result {
        Some((_, sign, digits)) => {
            let number: String = digits.into_iter().collect();
            let exp = number.parse::<i32>().expect("parse digit's can't fail");
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
    use num::BigRational;

    #[test]
    fn parse_integer_10() {
        assert_parse_as("0", Sexp::fixnum(BigInt::from(0)));
        assert_parse_as("10", Sexp::fixnum(BigInt::from(10)));
        assert_parse_as("#d10", Sexp::fixnum(BigInt::from(10)));
        assert_parse_as("#e#d10", Sexp::fixnum(BigInt::from(10)));
        assert_parse_as("23434", Sexp::fixnum(BigInt::from(23434)));
        assert_parse_as("-23434", Sexp::fixnum(BigInt::from(-23434)));
    }

    #[test]
    fn parse_integer_2() {
        assert_parse_as("#b0", Sexp::fixnum(BigInt::from(0)));
        assert_parse_as("#b01011", Sexp::fixnum(BigInt::from(11)));
    }

    #[test]
    fn parse_integer_8() {
        assert_parse_as("#o777", Sexp::fixnum(BigInt::from(511)));
        assert_parse_as("#o0775", Sexp::fixnum(BigInt::from(509)));

        //assert_parse_error("#o8989")
    }

    #[test]
    fn parse_decimal_short() {
        assert_parse_as(".3", Sexp::flonum(0.3));
        assert_parse_as(".3e1", Sexp::flonum(3.0));
        assert_parse_ok(".3e-1")
    }

    #[test]
    fn parse_decimal() {
        assert_parse_as("135.3", Sexp::flonum(135.3));
        assert_parse_as("-135.3", Sexp::flonum(-135.3));
        assert_parse_as("1.3e2", Sexp::flonum(130.0));
        assert_parse_ok("1.3e-1")
    }

    #[test]
    fn parse_rational() {
        assert_parse_as("3/4", Sexp::rational(BigInt::from(3), BigInt::from(4)));
        assert_parse_as(
            "#b111/100",
            Sexp::rational(BigInt::from(7), BigInt::from(4)),
        );
    }
}
