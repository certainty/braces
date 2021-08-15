use crate::compiler::frontend::{expression, reader};
use crate::compiler::frontend::expression::error::Error;
use crate::compiler::frontend::reader::sexp::datum::Datum;
use crate::compiler::frontend::reader::error::ReadError;
use crate::compiler::source::Source;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser;

impl Parser {
    pub fn parse_datum<T: Source>(&self, source: &mut T) -> std::result::Result<Datum, ReadError> {
        reader::parse(source)
    }

    pub fn parse_datum_sequence<T: Source>(
        &self,
        source: &mut T,
    ) -> std::result::Result<Vec<Datum>, ReadError> {
        reader::parse_sequence(source)
    }

    pub fn parse_program<T: Source>(&self, source: &mut T) -> Result<Vec<expression::Expression>> {
        expression::Expression::parse_program(source)
    }

    pub fn parse_expression<T: Source>(&self, source: &mut T) -> Result<expression::Expression> {
        expression::Expression::parse_one(source)
    }
}
