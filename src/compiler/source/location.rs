use super::SourceId;
use super::Span;
use crate::compiler::source::Registry;
use codespan_reporting::files::Files;

/// A location denotes a specific piece of source text in some
/// source. It combines a source and a span into a logical unit, the `Location`.
///
/// Locations are tracked through the whole compiler pipeline to allow good
/// source mapping back to the original source text.
#[derive(PartialEq, Debug, Clone)]
pub struct Location {
    pub id: SourceId,
    pub span: Span,
}

impl Location {
    pub fn for_syntax_transformer() -> Self {
        Self::new(SourceId::synthetic(), 0..0)
    }

    pub fn new<S: Into<Span>, Id: Into<SourceId>>(id: Id, span: S) -> Self {
        Self {
            id: id.into(),
            span: span.into(),
        }
    }

    /// Retrieve the line of this location.
    ///
    /// You need to hand in the `Registry` to be able to map the
    /// `SourceId` of this location back to source text and thus to the
    /// appropriate line number.
    pub fn line(&self, registry: &Registry) -> Option<usize> {
        registry
            .line_number(self.id.clone(), self.span.start())
            .ok()
    }
}

impl<S: Into<Span>> From<S> for Location {
    fn from(span: S) -> Self {
        Self::new(SourceId::synthetic(), span.into())
    }
}

/// This trait is implemented by every type that tracks locations
///
///  Examples of this are `Datum` and various `Expression`s.
pub trait HasSourceLocation {
    /// return `Location` information for this value
    fn source_location(&self) -> &Location;
}
