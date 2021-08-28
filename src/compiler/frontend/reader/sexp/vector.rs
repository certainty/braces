use super::datum::{Datum, Sexp};
use super::whitespace::parse_inter_token_space;
use super::{map_datum, parse_datum, Input, ParseResult};
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::multi::many0;
use nom::sequence::delimited;

/// Parse proper list
/// Ref: r7rs 7.1.2
/// ```grammar
/// <vector> -> #(<datum>*)  
/// ```

#[inline]
pub fn parse_vector<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let list_elements = delimited(
        parse_inter_token_space,
        parse_datum,
        parse_inter_token_space,
    );
    let vector = delimited(tag("#("), many0(list_elements), char(')'));

    map_datum(vector, Sexp::vector)(input)
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::reader::tests::*;

    use super::*;

    #[test]
    fn test_span_is_correct() {
        assert_parse_as(
            "#(#t    #f)",
            Sexp::vector(vec![
                make_datum(Sexp::boolean(true), 2..4),
                make_datum(Sexp::boolean(false), 8..10),
            ]),
        );
    }

    #[test]
    fn test_read_vector() {
        assert_parse_as(
            "#(#t    #f)",
            Sexp::vector(vec![
                make_datum(Sexp::boolean(true), 2..4),
                make_datum(Sexp::boolean(false), 8..10),
            ]),
        );
        let v: Vec<Datum> = vec![];

        assert_parse_as("#()", Sexp::vector(v));

        assert_parse_as(
            "#((foo #t))",
            Sexp::vector(vec![make_datum(
                Sexp::list(vec![
                    make_datum(Sexp::symbol("foo"), 3..6),
                    make_datum(Sexp::boolean(true), 7..9),
                ]),
                2..10,
            )]),
        );
    }
}