use crate::compiler::frontend::reader;
use crate::compiler::source_location::SourceLocation;
use thiserror::Error;
use crate::compiler::source::SourceType;

#[derive(Debug, Clone)]
pub struct Detail {
    pub content: String,
    pub location: SourceLocation,
}

impl Detail {
    pub fn new<Content: Into<String>, Loc: Into<SourceLocation>>(m: Content, loc: Loc) -> Self {
        Self {
            content: m.into(),
            location: loc.into()
        }
    }
}

#[derive(Debug)]
pub enum Error {
    IoError(String, std::io::Error),
    IncompleteInput(String, Option<SourceType>),
    Bug(String),
    ReadError(String, Detail),
    ParseError(String, Detail),
}

impl Error {
    pub fn io_error<M: Into<String>>(message: M, e: std::io::Error) -> Self {
        Error::IoError(message.into(), e)
    }

    pub fn incomplete_input<M: Into<String>>(message: M, source: Option<SourceType>) -> Self {
        Error::IncompleteInput(message.into(), source)
    }

    pub fn parse_error<M: Into<String>>(message: M, detail: Detail) -> Self {
        Error::ParseError(message.into(), detail)
    }

    pub fn read_error<M: Into<String>>(message: M, detail: Detail) -> Self {
        Error::ReadError(message.into(), detail)
    }

    pub fn bug<M: Into<String>>(message: M) -> Self {
        Error::Bug(message.into())
    }
}
