use std::ops::Range;
pub mod buffer;
pub mod file;
pub mod location;
pub mod origin;
pub mod registry;
pub mod string;

// re-exports for convenience
pub use buffer::BufferSource;
pub use file::FileSource;
pub use location::{HasSourceLocation, Location};
pub use origin::{HasOrigin, Origin};
pub use registry::Registry;
pub use string::StringSource;

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(transparent)]
pub struct SourceId(usize);

impl SourceId {
    pub fn location<S: Into<Span>>(&self, span: S) -> Location {
        Location::new(self.clone(), span)
    }
}

impl From<usize> for SourceId {
    fn from(n: usize) -> SourceId {
        SourceId(n)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[repr(transparent)]
pub struct Span(Range<usize>);

impl From<Range<usize>> for Span {
    fn from(n: Range<usize>) -> Self {
        Span(n)
    }
}

#[derive(Debug, Clone)]
pub struct Source<'a> {
    pub id: SourceId,
    pub code: &'a str,
}

impl<'a> Source<'a> {
    pub fn new(id: SourceId, code: &'a str) -> Self {
        Self { id, code }
    }
}
