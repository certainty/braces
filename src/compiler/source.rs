use super::source_location::SourceLocation;
use codespan_reporting::files;
use std::cell::RefCell;
use std::fs::File;
use std::io::Read;
use std::path;

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

#[derive(Debug, Clone, PartialEq)]
#[repr(transparent)]
pub struct SourceId(u64);

impl SourceId {
    pub fn location(&self, span: std::ops::Range<usize>) -> SourceLocation {
        SourceLocation::new(self.clone(), span)
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

    pub fn sources(&self) -> &files::SimpleFiles<SourceType, String> {
        &self.sources
    }
}

pub struct FileSource {
    path: path::PathBuf,
    content: RefCell<String>,
}

impl FileSource {
    pub fn new(path: path::PathBuf) -> Self {
        FileSource {
            path,
            content: RefCell::new(String::new()),
        }
    }

    fn get_content<'a>(&'a mut self) -> std::io::Result<std::cell::Ref<'a, String>> {
        if self.content.borrow().is_empty() {
            let mut f = File::open(self.path.clone())?;
            let mut buf = String::new();
            f.read_to_string(&mut buf)?;
            self.content.replace(buf);
        };

        Ok(self.content.borrow())
    }
}

impl Source for FileSource {
    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<()> {
        *buf = self.get_content()?.clone();
        Ok(())
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
