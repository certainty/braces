pub mod datum;
pub mod error;
pub mod expression;
use crate::compiler::source::Source;
use error::Error;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser;

impl Parser {
    pub fn parse_datum<T: Source>(&self, source: &mut T) -> Result<datum::Datum> {
        match datum::parse(source) {
            // TODO: translate the error properly
            Err(datum::Error::ParseError(inner)) => {
                Err(Error::SyntaxError(format!("{:#?}", inner)))
            }
            Err(e) => Err(Error::SyntaxError(format!("{:?}", e))),
            Ok(datum) => Ok(datum),
        }
    }

    pub fn parse_program<T: Source>(&self, _source: &mut T) -> Result<Vec<expression::Expression>> {
        todo!()
    }

    pub fn parse_expression<T: Source>(&self, source: &mut T) -> Result<expression::Expression> {
        expression::Expression::parse_one(source)
    }
}
