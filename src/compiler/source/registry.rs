use super::{HasOrigin, Origin, Source, SourceId};
use codespan_reporting::files;
use std::io::Read;

pub struct Registry<'a> {
    inner: &'a files::SimpleFiles<Origin, String>,
}

impl<'a> Registry<'a> {
    pub fn new() -> Self {
        Self {
            inner: &files::SimpleFiles::new(),
        }
    }

    pub fn add<T: HasOrigin + Read>(&mut self, s: T) -> std::io::Result<Source<'a>> {
        let mut out = String::new();
        s.read_to_string(&mut out)?;
        let handle = self.inner.add(s.origin(), out);
        let code = self.inner.source(handle)?;

        Ok(Source::new(SourceId(handle), code))
    }
}

impl<'a> files::Files<'a> for Registry<'a> {
    type FileId = SourceId;
    type Name = Origin;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, files::Error> {
        self.inner.name(id.0)
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, files::Error> {
        self.inner.source(id.0)
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        self.inner.line_index(id.0, byte_index)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, files::Error> {
        self.inner.line_range(id.0, line_index)
    }
}
