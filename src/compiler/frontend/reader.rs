use nom::error::context;
use nom::multi::many1;

use crate::compiler::frontend;
use crate::compiler::representation::SexpAST;
use crate::compiler::source::Source;

pub mod datum;
pub mod error;

#[derive(Clone, Debug)]
pub struct Reader;

impl Reader {
    pub fn new() -> Self {
        Self
    }

    pub fn parse(&self, source: &Source) -> frontend::Result<SexpAST> {
        let input = datum::Input::new_extra(&source.code, source.id.clone());
        let (_rest, datum) = context("program", many1(datum::parse_datum))(input)?;
        Ok(SexpAST::new(datum))
    }
}

#[cfg(test)]
pub mod tests {
    use crate::compiler::frontend::reader::datum::Datum;
    use crate::compiler::source::{BufferSource, Registry};

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
}
