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

pub fn read_program<T: source::Source>(input: &mut T) -> Result<Vec<syntax::Syntax>> {
    let mut buffer = String::new();
    input.read_to_string(&mut buffer)?;

    match lowlevel::parse_program(&buffer) {
        Ok(mut parsed) => to_ast_seq(&mut parsed),
        Err(_e) => Err(ReaderError::ParseError),
    }
}

fn to_ast_seq(pairs: &mut Pairs<lowlevel::Rule>) -> Result<Vec<syntax::Syntax>> {
    pairs.map(|p| to_ast(&p)).collect()
}

fn to_ast(pair: &Pair<lowlevel::Rule>) -> Result<syntax::Syntax> {
    match pair.as_rule() {
        lowlevel::Rule::number => match pair.as_str().parse() {
            Ok(num) => Ok(syntax::fixnum(num)),
            Err(_) => Err(ReaderError::ParseError),
        },
        lowlevel::Rule::BOOL_TRUE => Ok(syntax::boolean(true)),
        lowlevel::Rule::BOOL_FALSE => Ok(syntax::boolean(false)),
        lowlevel::Rule::IDENTIFIER => Ok(syntax::symbol(pair.as_str())),
        lowlevel::Rule::DELIMITED_IDENTIFIER => Ok(syntax::symbol(pair.as_str())),
        lowlevel::Rule::PECULIAR_IDENTIFIER => Ok(syntax::symbol(pair.as_str())),
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

    #[test]
    pub fn test_read_bool_true() {
        assert_eq!(
            Syntax::SelfEvaluatingSyntax(SelfEvaluating::Bool(true)),
            read("#t").unwrap()
        );

        assert_eq!(
            Syntax::SelfEvaluatingSyntax(SelfEvaluating::Bool(true)),
            read("#true").unwrap()
        )
    }

    #[test]
    pub fn test_read_symbol() {
        assert_eq!(
            read("foo").unwrap(),
            Syntax::SelfEvaluatingSyntax(SelfEvaluating::Symbol(String::from("foo")))
        )
    }

    fn read(inp: &str) -> Result<syntax::Syntax> {
        let mut source: source::StringSource = inp.into();
        read_datum(&mut source)
    }
}
