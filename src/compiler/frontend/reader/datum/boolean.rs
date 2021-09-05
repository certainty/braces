use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::value;

use super::{parser::with_location, parser::Input, parser::ParseResult};
use crate::compiler::frontend::reader::datum::Datum;

/// Boolean parser
///
/// Ref: r7rs 7.1.1
///
/// ```grammar
/// <BOOLEAN> -> #t | #f | #true | #false
/// ```
pub fn parse(input: Input) -> ParseResult<Datum> {
    let bool_literal = alt((
        value(true, tag("#true")),
        value(false, tag("#false")),
        value(true, tag("#t")),
        value(false, tag("#f")),
    ));

    with_location(bool_literal, Datum::boolean)(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::reader::tests::*;

    #[test]
    fn test_read_boolean_literal() {
        assert_parse_as("#t", Datum::boolean(true, 0..2));
        assert_parse_as("#true", Datum::boolean(true, 0..5));

        assert_parse_as("#f", Datum::boolean(false, 0..2));
        assert_parse_as("#false", Datum::boolean(false, 0..6));
    }
}
