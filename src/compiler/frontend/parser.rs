pub mod ast;
pub mod expression;
pub mod syntax;
use super::reader;
use super::reader::sexp::datum::Datum;
use crate::compiler::source_location::SourceLocation;
use ast::Ast;
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    ReadError(#[from] reader::Error),
    #[error("ParseError")]
    ParseError(String, SourceLocation),
    #[error("DomainError")]
    DomainError(String, SourceLocation),

    #[error(transparent)]
    MacroExpansionError(#[from] syntax::Error),
}

impl Error {
    pub fn parse_error<T>(message: &str, source: SourceLocation) -> Result<T> {
        Err(Error::ParseError(message.to_string(), source))
    }

    pub fn domain_error<T>(message: &str, source: SourceLocation) -> Result<T> {
        Err(Error::DomainError(message.to_string(), source))
    }
}

pub fn parse(datum: &Datum) -> Result<Ast> {
    Ok(Ast {
        expressions: vec![expression::Expression::parse(datum)?],
    })
}

pub fn parse_all(data: Vec<Datum>) -> Result<Ast> {
    Ok(Ast {
        expressions: expression::Expression::parse_all(data)?,
    })
}
