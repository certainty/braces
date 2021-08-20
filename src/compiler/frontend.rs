pub mod error;
pub(crate) mod expression;
pub mod parser;
pub mod reader;
use super::representation::CoreAST;
use super::source::Source;
use reader::sexp::datum::Datum;

pub type Result<T> = std::result::Result<T, error::Error>;

#[derive(Debug, Clone)]
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

    pub fn pass<'a>(&self, source: &Source<'a>) -> Result<CoreAST> {
        self.parse(self.read(source)?)
    }

    pub fn read<'a>(&self, source: &Source<'a>) -> Result<Vec<Datum>> {
        let datum = self.reader.parse(source)?;
        Ok(datum)
    }

    pub fn parse(&self, datum: &Vec<Datum>) -> Result<CoreAST> {
        let ast = self.parser.parse(&datum)?;
        Ok(ast)
    }
}
