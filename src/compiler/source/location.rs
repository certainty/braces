use super::SourceId;
use super::Span;

#[derive(PartialEq, Debug, Clone)]
pub struct Location {
    pub id: SourceId,
    pub span: Span,
}

impl Location {
    pub fn new<S: Into<Span>>(id: SourceId, span: S) -> Self {
        Self {
            id,
            span: span.into(),
        }
    }
}

pub trait HasSourceLocation {
    fn source_location<'a>(&'a self) -> &'a Location;
}
