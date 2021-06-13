use super::datum::Datum;
use super::datum::Sexp;
use super::map_datum;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::value;

use super::Input;
use super::ParseResult;

/// Boolean parser
///
/// Ref: r7rs 7.1.1
///
/// ```grammar
/// <BOOLEAN> -> #t | #f | #true | #false
/// ```
pub fn parse<'a>(input: Input<'a>) -> ParseResult<'a, Datum> {
    let bool_literal = alt((
        value(true, tag("#true")),
        value(false, tag("#false")),
        value(true, tag("#t")),
        value(false, tag("#f")),
    ));

    map_datum(bool_literal, Sexp::boolean)(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::sexp::tests::*;

    #[test]
    fn test_read_boolean_literal() {
        assert_parse_as("#t", Sexp::boolean(true));
        assert_parse_as("#true", Sexp::boolean(true));

        assert_parse_as("#f", Sexp::boolean(false));
        assert_parse_as("#false", Sexp::boolean(false));
    }
}
