use az::CheckedAs;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::character::complete::one_of;
use nom::combinator::{map, opt, value};
use nom::error::{Error, ErrorKind, ParseError};
use nom::multi::{many0, many1};
use nom::sequence::tuple;

use super::{Input, ParseResult};
use crate::compiler::frontend::reader::datum::Datum;

use crate::vm::value::number::rational::Rational;
use crate::vm::value::number::{flonum::Flonum, real::RealNumber, Exactness, SchemeNumber, Sign};
use nom_locate::position;

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

pub fn parse(input: Input) -> ParseResult<Datum> {
    // We need to track the location ourselves
    //
    // at this point the position has advanced already by the prefix parser
    // and map_datum can't capture the right position for us. It will always
    // pointer to after the prefix
    let (s, start) = position(input)?;
    let (s, pref) = parse_prefix(s)?;
    let (s, n) = parse_real(pref)(s)?;
    let (s, end) = position(s)?;
    let loc = s
        .extra
        .location(start.location_offset()..end.location_offset());

    Ok((s, Datum::number(n, loc)))
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
fn parse_real<'a>(prefix: Prefix) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    move |input| alt((parse_signed_real(prefix.clone()), parse_inf_nan))(input)
}

fn parse_signed_real<'a>(prefix: Prefix) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    let ureal = alt((
        parse_decimal,
        parse_rational(prefix.clone()),
        parse_uinteger(prefix.clone()),
    ));
    let mut signed_ureal = tuple((parse_sign, ureal));

    move |input| {
        let (s, (sign, num)) = signed_ureal(input)?;
        if let Some(num) = apply_exactness(num.sign(sign), &prefix.exactness) {
            Ok((s, num))
        } else {
            Err(nom::Err::Error(Error::from_error_kind(
                s,
                ErrorKind::Verify,
            )))
        }
    }
}

fn apply_exactness<'a>(num: RealNumber, exactness: &Option<Exactness>) -> Option<RealNumber> {
    match (&num, exactness) {
        (RealNumber::Flonum(flonum), Some(Exactness::Exact)) => flonum
            .clone()
            .checked_as::<Rational>()
            .map(RealNumber::Rational),
        (RealNumber::Fixnum(fixnum), Some(Exactness::Inexact)) => fixnum
            .clone()
            .checked_as::<Flonum>()
            .map(RealNumber::Flonum),
        (RealNumber::Rational(rat), Some(Exactness::Inexact)) => {
            rat.clone().checked_as::<Flonum>().map(RealNumber::Flonum)
        }
        _ => Some(num),
    }
}

fn parse_inf_nan(input: Input) -> ParseResult<RealNumber> {
    alt((
        value(RealNumber::Flonum(Flonum::nan()), tag("+nan.0")),
        value(RealNumber::Flonum(Flonum::nan()), tag("-nan.0")),
        value(RealNumber::Flonum(Flonum::infinity()), tag("+inf.0")),
        value(RealNumber::Flonum(Flonum::neg_infinity()), tag("-inf.0")),
    ))(input)
}

// <uinteger R> -> <digit R>+
fn parse_uinteger<'a>(prefix: Prefix) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    map(parse_digits(prefix.radix), RealNumber::from)
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

fn parse_rational<'a>(prefix: Prefix) -> impl FnMut(Input<'a>) -> ParseResult<'a, RealNumber> {
    map(
        tuple((
            parse_digits(prefix.radix),
            char('/'),
            parse_digits(prefix.radix),
        )),
        |(numer, _, denom)| RealNumber::Rational(Rational::from((numer, denom))),
    )
}

// decimal ->
//   <uniteger 10> <suffix> |
//   . <digit 10>+ <suffix> |
//   <digit 10>+ . <digit 10>* suffix

fn parse_decimal(input: Input) -> ParseResult<RealNumber> {
    map(alt((parse_decimal_short, parse_decimal_full)), |num| {
        RealNumber::Flonum(Flonum::from(num))
    })(input)
}

fn parse_decimal_full(input: Input) -> ParseResult<f64> {
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

fn parse_decimal_short(input: Input) -> ParseResult<f64> {
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
        _ => num as f64,
    }
}

fn parse_digits<'a>(radix: u8) -> impl FnMut(Input<'a>) -> ParseResult<'a, rug::Integer> {
    map(many1(_parse_digits(radix)), move |digits| {
        let digits: String = digits.into_iter().collect();

        rug::Integer::from(
            rug::Integer::parse_radix(digits.as_bytes(), radix as i32)
                .expect("Parse Integer digits can't fail"),
        )
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
fn parse_suffix(input: Input) -> ParseResult<Option<i32>> {
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

#[derive(Clone, Debug)]
struct Prefix {
    radix: u8,
    exactness: Option<Exactness>,
}

fn parse_prefix(input: Input) -> ParseResult<Prefix> {
    alt((
        (map(tuple((parse_exactness, parse_radix)), |res| Prefix {
            radix: res.1,
            exactness: Some(res.0),
        })),
        (map(tuple((parse_radix, parse_exactness)), |res| Prefix {
            radix: res.0,
            exactness: Some(res.1),
        })),
        (map(parse_exactness, |res| Prefix {
            radix: 10,
            exactness: Some(res),
        })),
        (map(parse_radix, |res| Prefix {
            radix: res,
            exactness: None,
        })),
    ))(input)
}

fn parse_radix(input: Input) -> ParseResult<u8> {
    alt((
        value(2, tag("#b")),
        value(8, tag("#o")),
        value(16, tag("#x")),
        value(10, opt(tag("#d"))),
    ))(input)
}

fn parse_exactness(input: Input) -> ParseResult<Exactness> {
    alt((
        value(Exactness::Inexact, tag("#i")),
        value(Exactness::Exact, tag("#e")),
    ))(input)
}

pub fn parse_sign(input: Input) -> ParseResult<Sign> {
    let (s, sign) = opt(alt((
        value(Sign::Plus, char('+')),
        value(Sign::Minus, char('-')),
    )))(input)?;

    Ok((s, sign.unwrap_or(Sign::Plus)))
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::reader::tests::*;
    use crate::vm::value::number::Number;

    use super::*;

    #[test]
    fn parse_integer_10() {
        assert_parse_as("0", Datum::number(Number::fixnum(0), 0..1));
        assert_parse_as("10", Datum::number(Number::fixnum(10), 0..2));
        assert_parse_as("#d10", Datum::number(Number::fixnum(10), 0..4));
        assert_parse_as("#e#d10", Datum::number(Number::fixnum(10), 0..6));
        assert_parse_as("23434", Datum::number(Number::fixnum(23434), 0..5));
        assert_parse_as("-23434", Datum::number(Number::fixnum(-23434), 0..6));
    }

    #[test]
    fn parse_integer_2() {
        assert_parse_as("#b0", Datum::number(Number::fixnum(0), 0..3));
        assert_parse_as("#b01011", Datum::number(Number::fixnum(11), 0..7));
    }

    #[test]
    fn parse_integer_8() {
        assert_parse_as("#o777", Datum::number(Number::fixnum(511), 0..5));
        assert_parse_as("#o0775", Datum::number(Number::fixnum(509), 0..6));

        //assert_parse_error("#o8989")
    }

    #[test]
    fn parse_decimal_short() {
        assert_parse_as(".3", Datum::number(Number::flonum(0.3), 0..2));
        assert_parse_as(".3e1", Datum::number(Number::flonum(3.0), 0..4));
        assert_parse_ok(".3e-1")
    }

    #[test]
    fn parse_decimal() {
        assert_parse_as("135.3", Datum::number(Number::flonum(135.3), 0..5));
        assert_parse_as("-135.3", Datum::number(Number::flonum(-135.3), 0..6));
        assert_parse_as("1.3e2", Datum::number(Number::flonum(130.0), 0..5));
        assert_parse_ok("1.3e-1")
    }

    #[test]
    fn parse_rational() {
        assert_parse_as("3/4", Datum::number(Number::fraction(3, 4), 0..3));
        assert_parse_as("#b111/100", Datum::number(Number::fraction(7, 4), 0..9));
    }

    #[test]
    fn parse_with_exactness() {
        assert_parse_as("#e0.25", Datum::number(Number::fraction(1, 4), 0..6));
        assert_parse_as("#i1", Datum::number(Number::flonum(1.0), 0..3));
        assert_parse_as("#i1/4", Datum::number(Number::flonum(0.25), 0..5));

        // mix radix and exactness
        assert_parse_as("#e#d0.25", Datum::number(Number::fraction(1, 4), 0..8));
        assert_parse_as("#d#e0.25", Datum::number(Number::fraction(1, 4), 0..8));
    }
}
