use crate::compiler::frontend::parser::expression::{Expression, LiteralExpression};
use crate::compiler::source_location::SourceLocation;
use crate::vm::byte_code::chunk::Chunk;
use crate::vm::byte_code::Instruction;
use crate::vm::scheme::value::Value;
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
                self.emit_constant(constant, loc)?
            }
            Expression::Literal(LiteralExpression::Quotation(_datum), _loc) => todo!(),
        }
        Ok(())
    }

    fn emit_constant(&mut self, value: &Value, source_location: &SourceLocation) -> Result<()> {
        let const_addr = self.chunk.add_constant(value);
        let inst_addr = self.chunk.write_instruction(Instruction::Const(const_addr));

        self.chunk
            .write_line(inst_addr, inst_addr, source_location.line);
        Ok(())
    }
}
