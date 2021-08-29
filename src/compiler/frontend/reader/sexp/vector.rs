use super::whitespace::parse_inter_token_space;
use super::{map_datum, parse_datum, Input, ParseResult};
use crate::compiler::frontend::reader::{datum::Datum, sexp::SExpression};
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

    map_datum(vector, SExpression::vector)(input)
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::reader::tests::*;

    use super::*;

    #[test]
    fn test_span_is_correct() {
        assert_parse_as(
            "#(#t    #f)",
            Datum::vector(
                vec![Datum::boolean(true, 2..4), Datum::boolean(false, 8..10)],
                0..11,
            ),
        );
    }

    #[test]
    fn test_read_vector() {
        let v: Vec<Datum> = vec![];

        assert_parse_as("#()", Datum::vector(v, 0..3));

        assert_parse_as(
            "#((foo #t))",
            Datum::vector(
                vec![Datum::list(
                    vec![Datum::symbol("foo", 3..6), Datum::boolean(true, 7..9)],
                    2..10,
                )],
                0..11,
            ),
        );
    }
}
