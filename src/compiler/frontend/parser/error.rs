use crate::compiler::source::SourceType;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    IOError(#[from] std::io::Error),
    #[error("SyntaxError")]
    SyntaxError(String, SourceType),
}

impl Error {
    pub fn syntax_error<T>(message: &str, source_type: SourceType) -> Result<T, Error> {
        Err(Error::SyntaxError(message.to_string(), source_type))
    }
}
