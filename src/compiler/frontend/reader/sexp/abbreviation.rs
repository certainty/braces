use super::{map_datum, parse_datum, Input, ParseResult};
use crate::compiler::frontend::reader::{datum::Datum, sexp::SExpression};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::combinator::value;
use nom::sequence::pair;

////////////////////////////
// abbreviation
////////////////////////////

#[inline]
pub fn parse(input: Input) -> ParseResult<Datum> {
    let abbrev = pair(parse_abbrev_prefix, parse_datum);

    map_datum(abbrev, |(abbr, datum)| SExpression::list(vec![abbr, datum]))(input)
}

#[inline]
fn parse_abbrev_prefix(input: Input) -> ParseResult<Datum> {
    let abbrev = alt((
        value(SExpression::symbol("quote"), char('\'')),
        value(SExpression::symbol("quasi-quote"), char('`')),
        value(SExpression::symbol("unquote-splicing"), tag(",@")),
        value(SExpression::symbol("unquote"), char(',')),
    ));

    map_datum(abbrev, |v| v)(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::reader::tests::*;

    #[test]
    fn test_read_abbrev() {
        assert_parse_as(
            "'foo",
            Datum::list(
                vec![Datum::symbol("quote", 0..1), Datum::symbol("foo", 1..4)],
                0..4,
            ),
        );
        assert_parse_as(
            ",foo",
            Datum::list(
                vec![Datum::symbol("unquote", 0..1), Datum::symbol("foo", 1..4)],
                0..4,
            ),
        );

        assert_parse_as(
            "`foo",
            Datum::list(
                vec![
                    Datum::symbol("quasi-quote", 0..1),
                    Datum::symbol("foo", 1..4),
                ],
                0..4,
            ),
        );

        assert_parse_as(
            ",@foo",
            Datum::list(
                vec![
                    Datum::symbol("unquote-splicing", 0..2),
                    Datum::symbol("foo", 2..5),
                ],
                0..5,
            ),
        );
    }
}
