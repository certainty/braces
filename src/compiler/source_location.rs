use super::source::SourceType;

#[derive(PartialEq, Debug, Clone)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    pub source_type: SourceType,
}

impl SourceLocation {
    pub fn new(source_type: SourceType, line: usize, column: usize) -> Self {
        SourceLocation {
            line,
            column,
            source_type,
        }
    }
}

pub trait HasSourceLocation {
    fn source_location<'a>(&'a self) -> &'a SourceLocation;
}
