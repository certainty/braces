pub mod error;
pub mod sexp;
use crate::compiler::frontend;
use crate::compiler::representation::SexpAST;
use crate::compiler::source::Source;
use nom::error::context;
use nom::multi::many1;

pub struct Reader;

impl Reader {
    pub fn new() -> Self {
        Self
    }

    pub fn parse<'a>(
        &self,
        source: &'a Source<'a>,
    ) -> std::result::Result<SexpAST, frontend::error::Error> {
        let input = sexp::Input::new_extra(&source.code, source.id.clone());
        let (_rest, datum) = context("program", many1(sexp::parse_datum))(input)?;

        Ok(SexpAST::new(datum))
    }
}

#[cfg(test)]
mod tests {
    use super::{
        sexp::datum::{Datum, Sexp},
        Reader,
    };
    use crate::compiler::source::{BufferSource, Location, Registry, SourceId, Span};

    // test helpers to use in the sexp parser
    pub fn assert_parse_as(inp: &str, expected: Sexp) {
        let mut registry = Registry::new();
        let source = registry.add(BufferSource::new(inp, "datum-parser-test"));
        let reader = Reader::new();
        let datum = reader.parse(source).unwrap();

        assert_eq!(datum[0].sexp, expected);
    }

    pub fn assert_parse_ok(inp: &str) {
        let mut registry = Registry::new();
        let source = registry.add(BufferSource::new(inp, "datum-parser-test"));
        let reader = Reader::new();
        let parsed = reader.parse(source);

        assert!(parsed.is_ok(), "expected to parse successfully")
    }

    pub fn assert_parse_error(inp: &str) {
        let mut registry = Registry::new();
        let source = registry.add(BufferSource::new(inp, "datum-parser-test"));
        let reader = Reader::new();
        let parsed = reader.parse(source);

        assert!(parsed.is_err(), "expected  parse error")
    }

    pub fn location<S: Into<Span>>(span: S) -> Location {
        Location::new(source_id(), span)
    }

    pub fn make_datum<S: Into<Span>>(sexp: Sexp, span: S) -> Datum {
        Datum::new(sexp, location(span))
    }

    pub fn source_id() -> SourceId {
        SourceId::from(0)
    }
}
