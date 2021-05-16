use super::datum::Datum;
use super::datum::Sexp;
use super::{map_datum, parse_datum, whitespace::parse_inter_token_space};
use nom::character::complete::char;
use nom::multi::{many0, many1};
use nom::sequence::{delimited, tuple};

use super::Input;
use super::ParseResult;

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
    use super::*;
    use crate::compiler::frontend::parser::sexp::tests::*;

    #[test]
    fn test_read_proper_list() {
        assert_parse_as(
            "(#t    #f)",
            Sexp::list(vec![
                make_datum(Sexp::boolean(true), 1, 2),
                make_datum(Sexp::boolean(false), 1, 8),
            ]),
        );

        let v: Vec<Datum> = vec![];

        assert_parse_as("()", Sexp::list(v));

        assert_parse_as(
            "((foo #t))",
            Sexp::list(vec![make_datum(
                Sexp::list(vec![
                    make_datum(Sexp::symbol("foo"), 1, 3),
                    make_datum(Sexp::boolean(true), 1, 7),
                ]),
                1,
                2,
            )]),
        );
    }

    #[test]
    fn test_read_improper_list() {
        assert_parse_as(
            "(#t  .  #f)",
            Sexp::improper_list(
                vec![make_datum(Sexp::boolean(true), 1, 2)],
                make_datum(Sexp::boolean(false), 1, 9),
            ),
        );

        assert_parse_as(
            "(#t #f #t .  #f)",
            Sexp::improper_list(
                vec![
                    make_datum(Sexp::boolean(true), 1, 2),
                    make_datum(Sexp::boolean(false), 1, 5),
                    make_datum(Sexp::boolean(true), 1, 8),
                ],
                make_datum(Sexp::boolean(false), 1, 14),
            ),
        );
    }
}
