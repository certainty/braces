use crate::compiler::source_location::SourceLocation;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    IOError(#[from] std::io::Error),
    #[error("SyntaxError")]
    SyntaxError(String, SourceLocation),
}
