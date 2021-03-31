use crate::compiler::frontend::parser::expression::Expression;
use crate::vm::byte_code::chunk::Chunk;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {}

type Result<T> = std::result::Result<T, Error>;

pub struct CodeGenerator {}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {}
    }

    pub fn generate(&mut self, ast: &Expression) -> Result<Chunk> {
        let mut chunk = Chunk::new();
        Ok(chunk)
    }
}
