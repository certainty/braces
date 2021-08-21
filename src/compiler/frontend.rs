pub mod error;
mod expander;
pub mod parser;
pub mod reader;
pub mod syntax;

use super::representation::{CoreAST, SexpAST};
use super::source::Source;

pub type Result<T> = std::result::Result<T, error::Error>;

#[derive(Debug)]
pub struct Frontend {
    reader: reader::Reader,
    parser: parser::Parser,
}

impl Frontend {
    pub fn new() -> Self {
        Frontend {
            reader: reader::Reader::new(),
            parser: parser::Parser::new(),
        }
    }

    pub fn pass<'a>(&mut self, source: &Source) -> Result<CoreAST> {
        self.parse(&self.read(source)?)
    }

    pub fn read<'a>(&self, source: &Source) -> Result<SexpAST> {
        let ast = self.reader.parse(source)?;
        Ok(ast)
    }

    pub fn parse(&mut self, sexps: &SexpAST) -> Result<CoreAST> {
        let ast = self.parser.parse(sexps)?;
        Ok(ast)
    }
}
