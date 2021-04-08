use std::path;

#[derive(Debug, Clone, PartialEq)]
pub enum SourceType {
    Buffer(String),
    File(path::PathBuf),
}

pub trait Source {
    fn source_type(&self) -> SourceType;
    fn as_str(&self) -> std::io::Result<&str>;
    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<()>;
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

    fn as_str(&self) -> std::io::Result<&str> {
        Ok(&self.internal)
    }

    fn source_type(&self) -> SourceType {
        SourceType::Buffer(self.label.clone())
    }
}
