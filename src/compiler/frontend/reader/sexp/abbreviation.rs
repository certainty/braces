use super::datum::{Datum, Sexp};
use super::{map_datum, parse_datum, Input, ParseResult};
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

    map_datum(abbrev, |(abbr, datum)| Sexp::list(vec![abbr, datum]))(input)
}

#[inline]
fn parse_abbrev_prefix(input: Input) -> ParseResult<Datum> {
    let abbrev = alt((
        value(Sexp::symbol("quote"), char('\'')),
        value(Sexp::symbol("quasi-quote"), char('`')),
        value(Sexp::symbol("unquote-splicing"), tag(",@")),
        value(Sexp::symbol("unquote"), char(',')),
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
            Sexp::list(vec![
                make_datum(Sexp::symbol("quote"), 0..0),
                make_datum(Sexp::symbol("foo"), 1..1),
            ]),
        );
        assert_parse_as(
            ",foo",
            Sexp::list(vec![
                make_datum(Sexp::symbol("unquote"), 0..0),
                make_datum(Sexp::symbol("foo"), 1..1),
            ]),
        );

        assert_parse_as(
            "`foo",
            Sexp::list(vec![
                make_datum(Sexp::symbol("quasi-quote"), 0..0),
                make_datum(Sexp::symbol("foo"), 1..1),
            ]),
        );

        assert_parse_as(
            ",@foo",
            Sexp::list(vec![
                make_datum(Sexp::symbol("unquote-splicing"), 1..1),
                make_datum(Sexp::symbol("foo"), 1..3),
            ]),
        );
    }
}
