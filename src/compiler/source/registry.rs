use super::{HasOrigin, Origin, Source, SourceId};
use codespan_reporting::files;
use codespan_reporting::files::Files;
use std::io::Read;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("files error")]
    FilesError(#[from] codespan_reporting::files::Error),
    #[error("IoError")]
    IoError(#[from] std::io::Error),
}

pub struct Registry {
    files: files::SimpleFiles<Origin, String>,
}

impl Registry {
    pub fn new() -> Self {
        Self {
            files: files::SimpleFiles::new(),
        }
    }

    pub fn add<T: HasOrigin + Read>(&mut self, s: &mut T) -> Result<Source> {
        let mut out = String::new();
        s.read_to_string(&mut out)?;
        let handle = SourceId::from(self.files.add(s.origin(), out));
        let code = self.files.source(handle.0)?;

        log::trace!("source registered: {:?} {:?}", s.origin(), handle);
        Ok(Source::new(handle, code))
    }
}

impl<'a> files::Files<'a> for &Registry {
    type FileId = SourceId;
    type Name = Origin;
    type Source = String;

    fn name(&'a self, id: Self::FileId) -> std::result::Result<Self::Name, files::Error> {
        self.files.name(id.0)
    }

    fn source(&'a self, id: Self::FileId) -> std::result::Result<Self::Source, files::Error> {
        self.files.source(id.0).map(String::from)
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> std::result::Result<usize, codespan_reporting::files::Error> {
        self.files.line_index(id.0, byte_index)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> std::result::Result<std::ops::Range<usize>, files::Error> {
        self.files.line_range(id.0, line_index)
    }
}
