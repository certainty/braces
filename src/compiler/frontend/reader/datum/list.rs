use nom::character::complete::char;
use nom::multi::{many0, many1};
use nom::sequence::{delimited, tuple};

use super::whitespace::parse_inter_token_space;
use super::{parse_datum, with_location, Input, ParseResult};
use crate::compiler::frontend::reader::datum::Datum;

/// Parse proper list
/// Ref: r7rs 7.1.2
/// ```grammar
/// <list> -> (<datum>*)  | (<datum>+ . <datum>)
/// ```

pub fn parse_proper_list(input: Input) -> ParseResult<Datum> {
    let list_elements = delimited(
        parse_inter_token_space,
        parse_datum,
        parse_inter_token_space,
    );
    let list = delimited(char('('), many0(list_elements), char(')'));

    with_location(list, Datum::list)(input)
}

/// Parse improper list
///
/// Ref: r7rs 7.1.2
/// ```grammar
/// <list> -> (<datum>*)  | (<datum>+ . <datum>)
/// ```

pub fn parse_improper_list(input: Input) -> ParseResult<Datum> {
    let improper_head = many1(delimited(
        parse_inter_token_space,
        parse_datum,
        parse_inter_token_space,
    ));

    let improper_tail = delimited(
        parse_inter_token_space,
        parse_datum,
        parse_inter_token_space,
    );
    let improper_elements = tuple((improper_head, char('.'), improper_tail));
    let improper_list = delimited(char('('), improper_elements, char(')'));

    with_location(improper_list, |(head, _, tail), loc| {
        Datum::improper_list(head, tail, loc)
    })(input)
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::reader::tests::*;

    use super::*;

    #[test]
    fn test_span_is_correct() {
        assert_parse_as(
            "(#t    #f)",
            Datum::list(
                vec![Datum::boolean(true, 1..3), Datum::boolean(false, 7..9)],
                0..10,
            ),
        );
    }

    #[test]
    fn test_read_proper_list() {
        assert_parse_as(
            "(#t    #f)",
            Datum::list(
                vec![Datum::boolean(true, 1..3), Datum::boolean(false, 7..9)],
                0..10,
            ),
        );
        let v: Vec<Datum> = vec![];

        assert_parse_as("()", Datum::list(v, 0..2));

        assert_parse_as(
            "((foo #t))",
            Datum::list(
                vec![Datum::list(
                    vec![Datum::symbol("foo", 2..5), Datum::boolean(true, 6..8)],
                    1..9,
                )],
                0..10,
            ),
        );
    }

    #[test]
    fn test_read_improper_list() {
        assert_parse_as(
            "(#t  .  #f)",
            Datum::improper_list(
                vec![Datum::boolean(true, 1..3)],
                Datum::boolean(false, 8..10),
                0..11,
            ),
        );

        assert_parse_as(
            "(#t #f #t .  #f)",
            Datum::improper_list(
                vec![
                    Datum::boolean(true, 1..3),
                    Datum::boolean(false, 4..6),
                    Datum::boolean(true, 7..9),
                ],
                Datum::boolean(false, 13..15),
                0..16,
            ),
        );
    }
}
