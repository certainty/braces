use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::character::complete::{anychar, line_ending};
use nom::error::context;
use nom::multi::many0;
use nom::multi::many_till;
use nom::sequence::{delimited, preceded};

use super::{parse_datum, unit, Input, ParseResult};

#[inline]
pub fn consume_line_ending(input: Input) -> ParseResult<()> {
    unit(line_ending)(input)
}

#[inline]
pub fn parse_intra_line_ws(input: Input) -> ParseResult<()> {
    unit(alt((char(' '), char('\t'))))(input)
}

#[inline]
pub fn parse_inter_token_space(input: Input) -> ParseResult<()> {
    let atmosphere = alt((parse_white_space, parse_comment, parse_directive));
    unit(many0(atmosphere))(input)
}

#[inline]
pub fn parse_white_space(input: Input) -> ParseResult<()> {
    alt((parse_intra_line_ws, consume_line_ending))(input)
}

#[inline]
pub fn parse_comment(input: Input) -> ParseResult<()> {
    context(
        "comment",
        unit(alt((
            parse_line_comment,
            parse_nested_comment,
            parse_inline_comment,
        ))),
    )(input)
}

#[inline]
fn parse_nested_comment(input: Input) -> ParseResult<()> {
    let comment_text = many0(anychar);
    let nested_comment = delimited(tag("#|"), comment_text, tag("|#"));
    context("nested comment", unit(nested_comment))(input)
}

#[inline]
fn parse_line_comment(input: Input) -> ParseResult<()> {
    unit(preceded(char(';'), many_till(anychar, line_ending)))(input)
}

#[inline]
fn parse_inline_comment(input: Input) -> ParseResult<()> {
    unit(preceded(
        tag("#;"),
        preceded(parse_inter_token_space, parse_datum),
    ))(input)
}

#[inline]
pub fn parse_directive(input: Input) -> ParseResult<()> {
    unit(alt((tag("#!fold-case"), tag("#!no-fold-case"))))(input)
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::reader::datum::Datum;
    use crate::compiler::frontend::reader::tests::*;

    #[test]
    fn test_read_comments() {
        assert_parse_as(";foo bar\n #t", Datum::boolean(true, 10..12));
        assert_parse_as(
            "(#t \n #; foo\n #f)",
            Datum::list(
                vec![Datum::boolean(true, 1..3), Datum::boolean(false, 14..16)],
                0..17,
            ),
        );

        //TODO: fix me
        // assert_parse_as(
        //     "#| this is a nested comment\n\n\n followed by more comments\n|# #t",
        //     Value::boolean(true),
        // );
    }
}
