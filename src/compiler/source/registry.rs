use super::{HasOrigin, Origin, Source, SourceId};
use codespan_reporting::files;
use std::io::Read;

pub struct Registry<'a> {
    sources: files::SimpleFiles<Origin, String>,
}

impl<'a> Registry<'a> {
    pub fn new() -> Self {
        Self {
            sources: files::SimpleFiles::new(),
        }
    }

    pub fn add<T: HasOrigin + Read>(&mut self, s: T) -> std::io::Result<Source> {
        let mut out = String::new();
        s.read_to_string(&mut out)?;
        let handle = self.sources.add(s.source_type(), out);
        Source::new(SourceId(handle), self.sources.source(handle)?)
    }

    pub fn source_opt<'a>(&'a self, s: &SourceId) -> Option<Source> {
        match self.sources.get(s) {
            Ok(f) => Some(Source::new(s.clone(), self.sources.source(s)?)),
            _ => None,
        }
    }
}

impl<'a> files::Files<'a> for Registry<'a> {
    type FileId = SourceId;
    type Name = Origin;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, files::Error> {
        self.sources.name(id)
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, files::Error> {
        self.sources.source(id)
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        self.sources.line_index(id, byte_index)
    }
}
