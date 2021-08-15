use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, line_ending};
use nom::character::complete::char;
use nom::error::context;
use nom::multi::many0;
use nom::multi::many_till;
use nom::sequence::{delimited, preceded};

use crate::compiler::frontend::reader::{
    Input,
    parse_datum,
    unit,
    ParseResult
};

#[inline]
pub fn consume_line_ending<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    unit(line_ending)(input)
}

#[inline]
pub fn parse_intra_line_ws<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    unit(alt((char(' '), char('\t'))))(input)
}

#[inline]
pub fn parse_inter_token_space<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    let atmosphere = alt((parse_white_space, parse_comment, parse_directive));
    unit(many0(atmosphere))(input)
}

#[inline]
pub fn parse_white_space<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    alt((parse_intra_line_ws, consume_line_ending))(input)
}

#[inline]
pub fn parse_comment<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
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
fn parse_nested_comment<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    let comment_text = many0(anychar);
    let nested_comment = delimited(tag("#|"), comment_text, tag("|#"));
    context("nested comment", unit(nested_comment))(input)
}

#[inline]
fn parse_line_comment<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    unit(preceded(char(';'), many_till(anychar, line_ending)))(input)
}

#[inline]
fn parse_inline_comment<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    unit(preceded(
        tag("#;"),
        preceded(parse_inter_token_space, parse_datum),
    ))(input)
}

#[inline]
pub fn parse_directive<'a>(input: Input<'a>) -> ParseResult<'a, ()> {
    unit(alt((tag("#!fold-case"), tag("#!no-fold-case"))))(input)
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::parser::sexp::datum::Sexp;
    use crate::compiler::frontend::parser::sexp::tests::*;

    use super::*;

    #[test]
    fn test_read_comments() {
        assert_parse_as(";foo bar\n #t", Sexp::boolean(true));
        assert_parse_as(
            "(#t \n #; foo\n #f)",
            Sexp::list(vec![
                make_datum(Sexp::boolean(true), 1, 2),
                make_datum(Sexp::boolean(false), 3, 2),
            ]),
        );

        //TODO: fix me
        // assert_parse_as(
        //     "#| this is a nested comment\n\n\n followed by more comments\n|# #t",
        //     Value::boolean(true),
        // );
    }
}
