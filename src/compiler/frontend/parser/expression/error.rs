use crate::compiler::frontend::parser::sexp;
use crate::compiler::source_location::SourceLocation;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    ReadError(#[from] sexp::error::ReadError),
    #[error("ParseError")]
    ParseError(String, SourceLocation),
    #[error("DomainError")]
    DomainError(String, SourceLocation),
}

impl Error {
    pub fn parse_error<T>(message: &str, source: SourceLocation) -> Result<T, Error> {
        Err(Error::ParseError(message.to_string(), source))
    }

    pub fn domain_error<T>(message: &str, source: SourceLocation) -> Result<T, Error> {
        Err(Error::DomainError(message.to_string(), source))
    }
}