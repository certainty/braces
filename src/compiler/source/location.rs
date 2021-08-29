use super::SourceId;
use super::Span;
use crate::compiler::source::Registry;
use codespan_reporting::files::Files;

#[derive(PartialEq, Debug, Clone)]
pub struct Location {
    pub id: SourceId,
    pub span: Span,
}

impl Location {
    pub fn new<S: Into<Span>, Id: Into<SourceId>>(id: Id, span: S) -> Self {
        Self {
            id: id.into(),
            span: span.into(),
        }
    }

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

pub trait HasSourceLocation {
    fn source_location(&self) -> &Location;
}
