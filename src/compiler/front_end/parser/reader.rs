use super::lowlevel;
use super::source;
use super::syntax;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser;
use std::io;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ReaderError {
    #[error("Read error")]
    ReadError { source: std::io::Error },

    #[error(transparent)]
    IoError(#[from] std::io::Error),

    #[error("Parse Error")]
    ParseError, //TODO: enrich error type

    #[error("Unsupported Syntax")]
    UnsupportedSyntax,
}

type Result<T> = std::result::Result<T, ReaderError>;

pub fn read_datum<T: source::Source>(input: &mut T) -> Result<syntax::Syntax> {
    let mut buffer = String::new();
    input.read_to_string(&mut buffer)?;

    match lowlevel::parse_datum(&buffer) {
        Ok(mut parsed) => to_ast(&parsed.next().unwrap()),
        Err(_e) => Err(ReaderError::ParseError),
    }
}

fn to_ast(pair: &Pair<lowlevel::Rule>) -> Result<syntax::Syntax> {
    match pair.as_rule() {
        lowlevel::Rule::number => match pair.as_str().parse() {
            Ok(num) => Ok(syntax::fixnum(num)),
            Err(_) => Err(ReaderError::ParseError),
        },
        _ => Err(ReaderError::UnsupportedSyntax),
    }
}

#[cfg(test)]
mod tests {
    use super::syntax::*;
    use super::*;

    #[test]
    pub fn test_read_number() {
        assert_eq!(
            Syntax::SelfEvaluatingSyntax(SelfEvaluating::FixNum(42)),
            read("42").unwrap()
        )
    }

    fn read(inp: &str) -> Result<syntax::Syntax> {
        let mut source: source::StringSource = inp.into();
        read_datum(&mut source)
    }
}
