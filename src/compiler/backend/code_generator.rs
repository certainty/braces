use crate::compiler::frontend::parser::expression::{Expression, LiteralExpression};
use crate::compiler::source_location::SourceLocation;
use crate::vm::byte_code::chunk::Chunk;
use crate::vm::byte_code::Instruction;
#[cfg(feature = "debug_code")]
use crate::vm::disassembler::Disassembler;
use crate::vm::scheme::value::list::List;
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
        self.current_chunk().write_instruction(Instruction::Halt);

        #[cfg(feature = "debug_code")]
        Disassembler::new(std::io::stdout()).disassemble(self.current_chunk(), "code");

        Ok(self.current_chunk())
    }

    fn emit_instructions(&mut self, ast: &Expression) -> Result<()> {
        match ast {
            Expression::Literal(LiteralExpression::SelfEvaluating(constant), loc) => {
                self.emit_lit(constant, loc)?
            }
            Expression::Literal(LiteralExpression::Quotation(datum), loc) => {
                self.emit_lit(datum, loc)?
            }
        }
        Ok(())
    }

    fn emit_constant(&mut self, value: &Value, source_location: &SourceLocation) -> Result<()> {
        let const_addr = self.current_chunk().add_constant(value);
        let inst_addr = self
            .current_chunk()
            .write_instruction(Instruction::Const(const_addr));

        self.current_chunk()
            .write_line(inst_addr, inst_addr, source_location.line);
        Ok(())
    }

    fn emit_lit(&mut self, value: &Value, loc: &SourceLocation) -> Result<()> {
        match value {
            Value::Bool(true) => self.emit_instruction(Instruction::True, loc)?,
            Value::Bool(false) => self.emit_instruction(Instruction::False, loc)?,
            Value::ProperList(List::Nil) => self.emit_instruction(Instruction::Nil, loc)?,
            _ => self.emit_constant(value, loc)?,
        }

        Ok(())
    }

    fn emit_instruction(
        &mut self,
        instr: Instruction,
        source_location: &SourceLocation,
    ) -> Result<()> {
        let addr = self.current_chunk().write_instruction(instr);

        self.current_chunk()
            .write_line(addr, addr, source_location.line);
        Ok(())
    }

    #[inline]
    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
}
