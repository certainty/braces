pub mod datum;
pub mod error;
pub mod expression;
use crate::compiler::source::Source;
use error::Error;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser;

impl Parser {
    pub fn parse<T: Source>(source: &mut T) -> Result<Option<expression::Expression>> {
        expression::Expression::parse(source)
    }
}
