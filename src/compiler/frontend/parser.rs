pub mod expression;
pub mod sexp;
use crate::compiler::source::Source;
use expression::error::Error;
use rustc_hash::FxHashMap;
use sexp::datum::{Datum, Sexp};
use sexp::error::ReadError;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser;

impl Parser {
    pub fn parse_datum<T: Source>(&self, source: &mut T) -> std::result::Result<Datum, ReadError> {
        sexp::parse(source)
    }

    pub fn parse_datum_sequence<T: Source>(
        &self,
        source: &mut T,
    ) -> std::result::Result<Vec<Datum>, ReadError> {
        sexp::parse_sequence(source)
    }

    pub fn parse_program<T: Source>(&self, source: &mut T) -> Result<Vec<expression::Expression>> {
        expression::Expression::parse_program(source)
    }

    pub fn parse_expression<T: Source>(&self, source: &mut T) -> Result<expression::Expression> {
        expression::Expression::parse_one(source)
    }
}
