pub mod error;
pub mod expression;
use super::reader;
use crate::compiler::source::Source;
use error::Error;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser;

impl Parser {
    pub fn parse_datum<T: Source>(
        &self,
        source: &mut T,
    ) -> std::result::Result<reader::datum::Datum, reader::error::ReadError> {
        reader::parse(source)
    }

    pub fn parse_program<T: Source>(&self, _source: &mut T) -> Result<Vec<expression::Expression>> {
        todo!()
    }

    pub fn parse_expression<T: Source>(&self, source: &mut T) -> Result<expression::Expression> {
        expression::Expression::parse_one(source)
    }
}
