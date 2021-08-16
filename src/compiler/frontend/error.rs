use crate::compiler::frontend::reader;
use crate::compiler::source::{SourceId, SourceType};
use crate::compiler::source_location::SourceLocation;

#[derive(Debug, Clone)]
pub struct Detail {
    pub content: String,
    pub location: SourceLocation,
}

impl Detail {
    pub fn new<Content: Into<String>, Loc: Into<SourceLocation>>(m: Content, loc: Loc) -> Self {
        Self {
            content: m.into(),
            location: loc.into(),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    IoError(String, SourceId, std::io::Error),
    IncompleteInput(String, Option<SourceType>),
    Bug(String),
    ReadError(String, Detail, Vec<Detail>),
    ParseError(String, Detail, Vec<Detail>),
}

impl Error {
    pub fn detail<M: Into<String>, C: Into<String>>(
        m: M,
        content: C,
        loc: SourceLocation,
    ) -> (String, Detail) {
        (m.into(), Detail::new(content, loc))
    }
    pub fn io_error<M: Into<String>>(message: M, source_id: SourceId, e: std::io::Error) -> Self {
        Error::IoError(message.into(), source_id, e)
    }

    pub fn incomplete_input<M: Into<String>>(message: M, source: Option<SourceType>) -> Self {
        Error::IncompleteInput(message.into(), source)
    }

    pub fn parse_error<M: Into<String>, More: Into<Vec<Detail>>>(
        m: M,
        detail: Detail,
        details: More,
    ) -> Self {
        Error::ParseError(m.into(), detail, details.into())
    }

    pub fn read_error<M: Into<String>, More: Into<Vec<Detail>>>(
        m: M,
        detail: Detail,
        details: More,
    ) -> Self {
        Error::ReadError(m.into(), detail, details.into())
    }

    pub fn bug<M: Into<String>>(message: M) -> Self {
        Error::Bug(message.into())
    }
}
