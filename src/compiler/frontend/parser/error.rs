use crate::compiler::source::Location;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("ParseError")]
    ParseError(String, Location),
    #[error("DomainError")]
    DomainError(String, Location),
}

impl Error {
    pub fn parse_error<T>(message: &str, source: Location) -> Result<T, Error> {
        Err(Error::ParseError(message.to_string(), source))
    }

    pub fn domain_error<T>(message: &str, source: Location) -> Result<T, Error> {
        Err(Error::DomainError(message.to_string(), source))
    }
}
