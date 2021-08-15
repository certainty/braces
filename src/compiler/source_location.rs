use super::source::SourceType;
use std::ops::Range;

#[derive(PartialEq, Debug, Clone)]
pub struct SourceLocation {
    pub source_type: SourceType,
    pub span: Range<usize>,
}

impl SourceLocation {
    pub fn new(source_type: SourceType, span: Range<usize>) -> Self {
        SourceLocation {
            source_type,
            span
        }
    }
}

pub trait HasSourceLocation {
    fn source_location<'a>(&'a self) -> &'a SourceLocation;
}
