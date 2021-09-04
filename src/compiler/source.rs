/// This module deals with sourc representation and tracking of loctations in source code
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

/// A `Source` is used in the compiler
/// to connect the textual information, the code, to the meta information like diagnostics.
///
/// Sources are read ones and added to a `Registry`, which it turn hands out a `SourceId`
/// to be used in the compilation pipeline.
///
#[derive(Debug, Clone)]
pub struct Source {
    pub id: SourceId,
    /// The literal source code
    pub code: String,
}

impl Source {
    pub fn new<S: Into<String>>(id: SourceId, code: S) -> Self {
        Self {
            id,
            code: code.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(transparent)]
pub struct SourceId(usize);

impl SourceId {
    /// Create a source id for test purposes
    pub fn synthetic() -> Self {
        SourceId(0)
    }
    pub fn location<S: Into<Span>>(&self, span: S) -> Location {
        Location::new(self.clone(), span)
    }
}

impl From<usize> for SourceId {
    fn from(n: usize) -> SourceId {
        SourceId(n)
    }
}

/// A `Span` identifies a part of input in the source text.
///
/// The diagnostic system uses that to print nice error messages which
/// show the part of the code that had problems.
#[derive(Debug, Clone, PartialEq)]
#[repr(transparent)]
pub struct Span(Range<usize>);

impl From<Span> for Range<usize> {
    fn from(s: Span) -> Range<usize> {
        s.0.clone()
    }
}

impl Span {
    /// Returns the offset in the source where this span starts
    #[inline]
    pub fn start(&self) -> usize {
        self.0.start
    }

    /// Returns the offset in the source where this span ends
    #[inline]
    pub fn end(&self) -> usize {
        self.0.end
    }
}

impl From<Range<usize>> for Span {
    fn from(n: Range<usize>) -> Self {
        Span(n)
    }
}
