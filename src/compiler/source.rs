use super::source_location::SourceLocation;
use std::cell::RefCell;
use std::fs::File;
use std::io::Read;
use std::path;

#[derive(Debug, Clone, PartialEq)]
pub enum SourceType {
    Buffer(String),
    File(path::PathBuf),
}

impl SourceType {
    pub fn location(&self, line: usize, col: usize) -> SourceLocation {
        SourceLocation::new(self.clone(), line, col)
    }
}

pub trait Source {
    fn source_type(&self) -> SourceType;
    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<()>;

    fn location(&self, line: usize, col: usize) -> SourceLocation {
        SourceLocation::new(self.source_type(), line, col)
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

    fn as_str(&mut self) -> std::io::Result<&str> {
        Ok(&self.internal)
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
