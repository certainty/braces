/// The scheme parser that turns read syntax into expressions
///
/// The schema parser, similar to the runtime, relies on the scheme reader
/// to turn an input stream into a sequence of syntax. This parser takes the syntax
/// and parses it into expressions.
///
/// As scheme supports quoted data, the parser has to conditionally disable "evaluation"
/// which is straight forward todo. This is one reason why we re-use the underlying low
/// level parser in the reader.
mod expression;
mod lowlevel;
mod reader;
mod source;
mod syntax;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    ReaderError(#[from] reader::ReaderError),
}

type Result<T> = std::result::Result<T, ParseError>;

pub fn parse<T: source::Source>(source: &mut T) -> Result<expression::Expression> {
    let ast = reader::read_datum(source)?;
    parse_single(ast)
}

/// Convert syntax into expressions
fn parse_single(datum: syntax::Syntax) -> Result<expression::Expression> {
    match datum {
        syntax::Syntax::SelfEvaluatingSyntax(syn) => Ok(expression::Expression::Literal(syn)),
        _ => panic!("Unsupported syntax"),
    }
}

#[cfg(test)]
mod tests {
    use super::expression::Expression;
    use super::*;

    #[test]
    fn test_literal_number() {
        assert_matches!(run_parser("23545").unwrap(), Expression::Literal(_))
    }

    fn run_parser(inp: &str) -> Result<expression::Expression> {
        let mut source: source::StringSource = inp.into();

        parse(&mut source)
    }
}
