use codespan_reporting::files;
use std::fs::File;
use std::io::Read;
use std::ops::Range;
use std::path;

//
// use crate::compiler::source;
//
// let mut sources = source::Registry::new();
// let buffer = source::StringSource::new("(define x #t)", "the-buffer");
//
// // buffer_id is used in the compiler during all phases
// let buffer_id = sources.add(buffer);
//
// // get a location in the registered source
// let loc = buffer_id.location(10..15);
//
//

#[derive(Debug, Clone, PartialEq)]
#[repr(transparent)]
pub struct Span(Range<usize>);

impl From<Range<usize>> for Span {
    fn from(n: Range<usize>) -> Self {
        Span(n)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Location {
    pub id: SourceId,
    pub span: Span,
}

impl Location {
    pub fn new<S: Into<Span>>(id: SourceId, span: S) -> Self {
        Self {
            id,
            span: span.into(),
        }
    }
}

pub trait HasSourceLocation {
    fn source_location<'a>(&'a self) -> &'a Location;
}

#[derive(Debug, Clone, PartialEq)]
#[repr(transparent)]
pub struct SourceId(usize);

impl SourceId {
    pub fn location<S: Into<Span>>(&self, span: S) -> Location {
        Location::new(self.clone(), span)
    }
}

#[derive(Debug, Clone)]
pub enum SourceType {
    Synthetic,
    Buffer(String),
    File(std::path::PathBuf),
}

impl std::fmt::Display for SourceType {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        todo!()
    }
}

impl From<usize> for SourceId {
    fn from(n: usize) -> SourceId {
        SourceId(n)
    }
}

pub trait Source {
    fn source_type(&self) -> SourceType;
    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<()>;
}

pub struct Registry {
    sources: files::SimpleFiles<SourceType, String>,
}

impl Registry {
    pub fn new() -> Self {
        Self {
            sources: files::SimpleFiles::new(),
        }
    }

    pub fn add(&mut self, s: impl Source) -> std::io::Result<SourceId> {
        let mut out = String::new();
        s.read_to_string(&mut out)?;
        let handle = self.sources.add(s.source_type(), out);
        Ok(SourceId(handle))
    }

    pub fn source_opt<'a>(&'a self, s: &SourceId) -> Option<&'a str> {
        match self.sources.get(s) {
            Ok(f) => self.sources.source(s).into(),
            _ => None,
        }
    }
}

impl<'a> codespan_reporting::files::Files<'a> for Registry {
    type FileId = SourceId;
    type Name = SourceType;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
        self.sources.name(id)
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
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

pub struct FileSource {
    path: path::PathBuf,
}

impl FileSource {
    pub fn new(path: path::PathBuf) -> Self {
        FileSource { path }
    }
}

impl Source for FileSource {
    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<()> {
        let mut f = File::open(self.path.clone())?;
        let mut buf = String::new();
        f.read_to_string(&mut buf)
    }

    fn source_type(&self) -> SourceType {
        SourceType::File(self.path.clone())
    }
}

/// Implementation of source that can be constructed for strings
pub struct StringSource {
    label: String,
    internal: String,
}

impl StringSource {
    pub fn new(inp: &str, label: &str) -> Self {
        StringSource {
            internal: inp.to_string(),
            label: label.to_string(),
        }
    }
}

impl Source for StringSource {
    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<()> {
        *buf = self.internal.clone();
        Ok(())
    }

    fn source_type(&self) -> SourceType {
        SourceType::Buffer(self.label.clone())
    }
}
