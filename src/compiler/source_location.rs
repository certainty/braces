use super::source::SourceId;
use std::ops::Range;

#[derive(PartialEq, Debug, Clone)]
pub struct SourceLocation {
    pub id: SourceId,
    pub span: Range<usize>,
}

impl SourceLocation {
    pub fn new(id: SourceId, span: Range<usize>) -> Self {
        SourceLocation { id, span }
    }
}

pub trait HasSourceLocation {
    fn source_location<'a>(&'a self) -> &'a SourceLocation;
}
