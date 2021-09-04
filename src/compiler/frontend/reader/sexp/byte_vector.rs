use super::number;
use super::whitespace::parse_inter_token_space;
use super::{map_datum, Input, ParseResult};
use crate::compiler::frontend::reader::{datum::Datum, sexp::SExpression};
use crate::vm::value::number::{Number, SchemeNumber};
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::error::{ErrorKind, ParseError};
use nom::multi::many0;
use nom::sequence::delimited;

/// Parse proper list
/// Ref: r7rs 7.1.2
/// ```grammar
/// <byte-vector> -> #u8(<byte>*)  
/// <byte> -> (any exact number between 0 and 255)
/// ```

#[inline]
pub fn parse(input: Input) -> ParseResult<Datum> {
    let byte = delimited(parse_inter_token_space, parse_byte, parse_inter_token_space);
    let vector = delimited(tag("#u8("), many0(byte), char(')'));

    map_datum(vector, SExpression::byte_vector)(input)
}

fn parse_byte(input: Input) -> ParseResult<u8> {
    let (s, num) = number::parse(input)?;

    match num.s_expression() {
        SExpression::Number(n)
            if n.is_exact() && n >= &Number::fixnum(0) && n <= &Number::fixnum(255) =>
        {
            ParseResult::Ok((s, n.to_u8().unwrap()))
        }
        _ => {
            return ParseResult::Err(nom::Err::Error(nom::error::Error::from_error_kind(
                s,
                ErrorKind::Verify,
            )))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::reader::tests::*;

    #[test]
    fn read_byte_vector() {
        assert_parse_as("#u8()", Datum::byte_vector(Vec::<u8>::new(), 0..5));

        assert_parse_as(
            "#u8(10 255    0)",
            Datum::byte_vector(vec![10 as u8, 255 as u8, 0 as u8], 0..16),
        );
    }

    #[test]
    fn read_byte_vector_num_literals() {
        assert_parse_as(
            "#u8(#xff #o7)",
            Datum::byte_vector(vec![255 as u8, 7 as u8], 0..13),
        );
    }

    #[test]
    fn read_byte_vector_out_of_range() {
        assert_parse_error("#u8(300 355)");
    }
}
