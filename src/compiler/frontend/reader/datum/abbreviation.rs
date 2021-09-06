use super::{parse_datum, with_location, Input, ParseResult};
use crate::compiler::frontend::reader::datum::Datum;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::combinator::value;
use nom::sequence::pair;

////////////////////////////
// abbreviation
////////////////////////////

pub fn parse(input: Input) -> ParseResult<Datum> {
    let abbrev = pair(parse_abbrev_prefix, parse_datum);

    with_location(abbrev, |(abbr, datum), loc| {
        Datum::list(vec![abbr, datum], loc)
    })(input)
}

fn parse_abbrev_prefix(input: Input) -> ParseResult<Datum> {
    let abbrev = alt((
        value("quote", char('\'')),
        value("quasi-quote", char('`')),
        value("unquote-splicing", tag(",@")),
        value("unquote", char(',')),
    ));

    with_location(abbrev, |v, loc| Datum::symbol(v, loc))(input)
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
