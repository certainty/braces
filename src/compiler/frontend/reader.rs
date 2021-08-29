use nom::error::context;
use nom::multi::many1;

use crate::compiler::frontend;
use crate::compiler::representation::SexpAST;
use crate::compiler::source::Source;

pub mod datum;
pub mod error;
pub mod sexp;

#[derive(Clone, Debug)]
pub struct Reader;

impl Reader {
    pub fn new() -> Self {
        Self
    }

    pub fn parse(&self, source: &Source) -> frontend::Result<SexpAST> {
        let input = sexp::Input::new_extra(&source.code, source.id.clone());
        let (_rest, datum) = context("program", many1(sexp::parse_datum))(input)?;
        Ok(SexpAST::new(datum))
    }
}

#[cfg(test)]
pub mod tests {
    use crate::compiler::frontend::reader::{datum::Datum, sexp::SExpression};
    use crate::compiler::source::{BufferSource, Location, Registry, SourceId, Span};

    use super::Reader;

    pub fn parse_datum(inp: &str) -> Datum {
        let mut registry = Registry::new();
        let source = registry
            .add(&mut BufferSource::new(inp, "datum-parser-test"))
            .unwrap();
        let reader = Reader::new();
        let datum = reader.parse(&source).unwrap();

        datum.to_vec()[0].clone()
    }

    // test helpers to use in the sexp parser
    pub fn assert_parse_as(inp: &str, expected: Datum) {
        let mut registry = Registry::new();
        let source = registry
            .add(&mut BufferSource::new(inp, "datum-parser-test"))
            .unwrap();
        let reader = Reader::new();
        let datum = reader.parse(&source).unwrap();

        assert_eq!(&datum.to_vec()[0], &expected);
    }

    pub fn assert_parse_ok(inp: &str) {
        let mut registry = Registry::new();
        let source = registry
            .add(&mut BufferSource::new(inp, "datum-parser-test"))
            .unwrap();
        let reader = Reader::new();
        let parsed = reader.parse(&source);

        assert!(parsed.is_ok(), "expected to parse successfully")
    }

    pub fn assert_parse_error(inp: &str) {
        let mut registry = Registry::new();
        let source = registry
            .add(&mut BufferSource::new(inp, "datum-parser-test"))
            .unwrap();
        let reader = Reader::new();
        let parsed = reader.parse(&source);

        assert!(parsed.is_err(), "expected  parse error")
    }

    pub fn location<S: Into<Span>>(span: S) -> Location {
        Location::new(source_id(), span)
    }

    pub fn make_datum<S: Into<Span>>(sexp: SExpression, span: S) -> Datum {
        Datum::new(sexp, location(span))
    }

    pub fn source_id() -> SourceId {
        SourceId::from(0)
    }
}
