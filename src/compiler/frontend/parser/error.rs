use crate::compiler::source::SourceType;
use crate::compiler::source_location::SourceLocation;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    IOError(#[from] std::io::Error),
    #[error("SyntaxError")]
    SyntaxError(String, SourceType),
    #[error("ParseError")]
    ParseError(String, SourceLocation),
    #[error("DomainError")]
    DomainError(String, SourceLocation),
}

impl Error {
    pub fn syntax_error<T>(message: &str, source_type: SourceType) -> Result<T, Error> {
        Err(Error::SyntaxError(message.to_string(), source_type))
    }

    pub fn parse_error<T>(message: &str, source: SourceLocation) -> Result<T, Error> {
        Err(Error::ParseError(message.to_string(), source))
    }

    pub fn domain_error<T>(message: &str, source: SourceLocation) -> Result<T, Error> {
        Err(Error::DomainError(message.to_string(), source))
    }
}
