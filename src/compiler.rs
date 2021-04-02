pub mod backend;
pub mod frontend;
pub mod source;
pub mod source_location;
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
    ParseError(#[from] frontend::parser::error::Error),

    #[error(transparent)]
    GenerationError(#[from] code_generator::Error),
}

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Compiler {}
    }

    pub fn compile_expression<T: Source>(
        &mut self,
        source: &mut T,
    ) -> Result<Option<chunk::Chunk>> {
        if let Some(ast) = Parser::parse_expression(source)? {
            let mut code_gen = CodeGenerator::new();
            let chunk = code_gen.generate(&ast)?;
            Ok(Some(chunk.clone()))
        } else {
            Ok(None)
        }
    }
}
