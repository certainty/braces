use crate::compiler::frontend::parser::expression::{
    Expression, LiteralExpression, SelfEvaluatingExpression,
};
use crate::vm::byte_code::chunk::Chunk;
use crate::vm::byte_code::Instruction;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {}

type Result<T> = std::result::Result<T, Error>;

pub struct CodeGenerator {
    chunk: Chunk,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            chunk: Chunk::new(),
        }
    }

    pub fn generate(&mut self, ast: &Expression) -> Result<&Chunk> {
        self.emit_instructions(ast)?;
        self.chunk.write_instruction(Instruction::Halt);

        Ok(&self.chunk)
    }

    fn emit_instructions(&mut self, ast: &Expression) -> Result<()> {
        match ast {
            Expression::Literal(LiteralExpression::SelfEvaluating(constant), loc) => {
                self.emit_constant(constant)?
            }
            Expression::Literal(LiteralExpression::Quotation(datum), loc) => todo!(),
        }
        Ok(())
    }

    fn emit_constant(&mut self, expr: &SelfEvaluatingExpression) -> Result<()> {
        Ok(())
    }
}
