use super::value::Value;
use crate::compiler::frontend::parser::{error, Parser};
use crate::compiler::source::Source;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ReadError {
    #[error(transparent)]
    ReadError(#[from] error::Error),
}

type Result<T> = std::result::Result<T, ReadError>;

pub struct Reader {
    parser: Parser,
}

impl Reader {
    pub fn new() -> Self {
        Reader { parser: Parser }
    }

    pub fn read<T: Source>(&self, source: &mut T) -> Result<Option<Value>> {
        Ok(self.parser.parse_datum(source)?.map(|d| d.value))
    }
}
