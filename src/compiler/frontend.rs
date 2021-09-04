use super::representation::{CoreAST, SexpAST};
use super::source::Source;

pub mod error;
mod expander;
pub mod parser;
pub mod reader;
pub mod syntax;

// The frontend of the compiler turns source code into core scheme expressions.
// It's the home of the reading, expansion and parsing phases.
//
// On very basic level the flow of data through the frontend looks something like the following
//
//                         ┌─────────────────┐                    ┌───────────────────┐               ┌───────────────────┐
//                         │                 │      SexpAST       │                   │     SexpAST   │                   │
//  SourceText ───────────►│      Reader     ├───────────────────►│      Expander     ├──────────────►│      Parser       ├───────────────► CoreAST
//                         │                 │                    │                   │               │                   │
//                         └─────────────────┘                    └───────────────────┘               └───────────────────┘
//

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

    /// Read the source code from `source` and turn it into an AST of s-expressions
    pub fn read<'a>(&self, source: &Source) -> Result<SexpAST> {
        let ast = self.reader.parse(source)?;
        Ok(ast)
    }

    /// Take a single s-expression and parse it into an AST of core scheme expressions.
    /// Note that every core scheme form is an s-expression but not every s-expression
    /// is a valid core scheme expression.
    pub fn parse(&mut self, sexps: &SexpAST) -> Result<CoreAST> {
        let ast = self.parser.parse(sexps)?;
        Ok(ast)
    }
}
