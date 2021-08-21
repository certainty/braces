use nom::character::complete::char;
use nom::multi::{many0, many1};
use nom::sequence::{delimited, tuple};

use super::datum::{Datum, Sexp};
use super::whitespace::parse_inter_token_space;
use super::{map_datum, parse_datum, Input, ParseResult};

/// Parse proper list
/// Ref: r7rs 7.1.2
/// ```grammar
/// <list> -> (<datum>*)  | (<datum>+ . <datum>)
/// ```

#[inline]
pub fn parse_proper_list<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let list_elements = delimited(
        parse_inter_token_space,
        parse_datum,
        parse_inter_token_space,
    );
    let list = delimited(char('('), many0(list_elements), char(')'));

    map_datum(list, Sexp::list)(input)
}

/// Parse improper list
///
/// Ref: r7rs 7.1.2
/// ```grammar
/// <list> -> (<datum>*)  | (<datum>+ . <datum>)
/// ```

#[inline]
pub fn parse_improper_list<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
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

    map_datum(improper_list, |(improper_head, _, improper_tail)| {
        Sexp::improper_list(improper_head, improper_tail)
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
            Sexp::list(vec![
                make_datum(Sexp::boolean(true), 1..3),
                make_datum(Sexp::boolean(false), 7..9),
            ]),
        );
    }

    #[test]
    fn test_read_proper_list() {
        assert_parse_as(
            "(#t    #f)",
            Sexp::list(vec![
                make_datum(Sexp::boolean(true), 1..3),
                make_datum(Sexp::boolean(false), 7..9),
            ]),
        );
        let v: Vec<Datum> = vec![];

        assert_parse_as("()", Sexp::list(v));

        assert_parse_as(
            "((foo #t))",
            Sexp::list(vec![make_datum(
                Sexp::list(vec![
                    make_datum(Sexp::symbol("foo"), 2..5),
                    make_datum(Sexp::boolean(true), 6..8),
                ]),
                1..9,
            )]),
        );
    }

    #[test]
    fn test_read_improper_list() {
        assert_parse_as(
            "(#t  .  #f)",
            Sexp::improper_list(
                vec![make_datum(Sexp::boolean(true), 1..3)],
                make_datum(Sexp::boolean(false), 8..10),
            ),
        );

        assert_parse_as(
            "(#t #f #t .  #f)",
            Sexp::improper_list(
                vec![
                    make_datum(Sexp::boolean(true), 1..3),
                    make_datum(Sexp::boolean(false), 4..6),
                    make_datum(Sexp::boolean(true), 7..9),
                ],
                make_datum(Sexp::boolean(false), 13..15),
            ),
        );
    }
}
