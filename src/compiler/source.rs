use std::path;

#[derive(Debug, Clone, PartialEq)]
pub enum SourceType {
    Buffer(String),
    File(path::PathBuf),
}

pub trait Source {
    fn source_type(&self) -> SourceType;
    fn for_message(&self) -> String;
    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<()>;
}
