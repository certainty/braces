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
