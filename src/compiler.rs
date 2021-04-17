pub mod backend;
pub mod error;
pub mod frontend;
pub mod source;
pub mod source_location;
pub mod utils;
use crate::vm::byte_code::chunk;
use backend::code_generator;
use backend::code_generator::CodeGenerator;
use frontend::parser::Parser;
use source::Source;
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    ParseError(#[from] frontend::parser::expression::error::Error),

    #[error(transparent)]
    GenerationError(#[from] code_generator::Error),
}

pub struct Compiler {
    parser: Parser,
}

#[derive(Clone, Debug)]
pub struct CompilationUnit {
    pub symbols: Vec<String>,
    pub strings: Vec<String>,
    pub code: chunk::Chunk,
}

impl CompilationUnit {
    pub fn new(symbols: Vec<String>, strings: Vec<String>, code: chunk::Chunk) -> Self {
        CompilationUnit {
            symbols,
            strings,
            code,
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { parser: Parser }
    }

    pub fn compile_expression<T: Source>(&mut self, source: &mut T) -> Result<CompilationUnit> {
        let ast = self.parser.parse_expression(source)?;
        let mut code_gen = CodeGenerator::new();

        Ok(code_gen.generate(&ast)?)
    }
}
