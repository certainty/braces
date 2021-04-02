pub mod datum;
pub mod error;
pub mod expression;
use crate::compiler::source::Source;
use error::Error;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser;

impl Parser {
    pub fn parse_datum<T: Source>(source: &mut T) -> Result<Option<datum::Datum>> {
        datum::Datum::parse(source)
    }

    pub fn parse_program<T: Source>(_source: &mut T) -> Result<Vec<expression::Expression>> {
        todo!()
    }

    pub fn parse_expression<T: Source>(source: &mut T) -> Result<Option<expression::Expression>> {
        expression::Expression::parse_one(source)
    }
}
