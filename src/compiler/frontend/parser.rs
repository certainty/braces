pub mod datum;
pub mod error;
pub mod expression;
use crate::compiler::source::Source;
use error::Error;
use std::collections::HashSet;

// shared constants
lazy_static! {
    pub static ref SPECIAL_INITIAL: HashSet<char> = {
        vec![
            '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '^', '_', '~',
        ]
        .iter()
        .cloned()
        .collect()
    };
}

type Result<T> = std::result::Result<T, Error>;

pub struct Parser;

impl Parser {
    pub fn is_special_initial(c: &char) -> bool {
        SPECIAL_INITIAL.contains(c)
    }

    pub fn parse_datum<T: Source>(&self, source: &mut T) -> Result<Option<datum::Datum>> {
        datum::Datum::parse(source)
    }

    pub fn parse_program<T: Source>(&self, _source: &mut T) -> Result<Vec<expression::Expression>> {
        todo!()
    }

    pub fn parse_expression<T: Source>(
        &self,
        source: &mut T,
    ) -> Result<Option<expression::Expression>> {
        expression::Expression::parse_one(source)
    }
}
