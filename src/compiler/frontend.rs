pub mod error;
pub(crate) mod expression;
pub mod parser;
pub mod reader;

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
}
