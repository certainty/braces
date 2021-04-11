use crate::compiler::frontend::parser::expression::Identifier;
use crate::compiler::frontend::parser::expression::{Expression, LiteralExpression};
use crate::compiler::frontend::parser::sexp::datum;
use crate::compiler::source_location::SourceLocation;
use crate::vm::byte_code::chunk::Chunk;
use crate::vm::byte_code::Instruction;
#[cfg(feature = "debug_code")]
use crate::vm::disassembler::Disassembler;
use crate::vm::scheme::value;
use crate::vm::scheme::value::Value;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {}

type Result<T> = std::result::Result<T, Error>;

pub struct CodeGenerator {
    values: value::Factory,
    chunk: Chunk,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            chunk: Chunk::new(),
            values: value::Factory::default(),
        }
    }

    pub fn generate(&mut self, ast: &Expression) -> Result<&Chunk> {
        self.emit_instructions(ast)?;
        self.current_chunk().write_instruction(Instruction::Halt);

        #[cfg(feature = "debug_code")]
        Disassembler::new(std::io::stdout()).disassemble(self.current_chunk(), "code");

        Ok(self.current_chunk())
    }

    #[inline]
    fn sym(&mut self, s: &str) -> Value {
        self.values.symbol(s)
    }

    fn emit_instructions(&mut self, ast: &Expression) -> Result<()> {
        match ast {
            Expression::Identifier(id, loc) => self.emit_read_variable(id, loc)?,
            Expression::Assign(id, expr, loc) => self.emit_assignment(id, expr, loc)?,
            Expression::Literal(LiteralExpression::SelfEvaluating(constant)) => {
                self.emit_lit(constant)?
            }
            Expression::Literal(LiteralExpression::Quotation(datum)) => self.emit_lit(datum)?,
            Expression::Conditional(test, consequent, alternate, loc) => todo!(),
        }
        Ok(())
    }

    fn emit_read_variable(&mut self, id: &Identifier, loc: &SourceLocation) -> Result<()> {
        let id_sym = self.sym(&id.string());
        let const_addr = self.current_chunk().add_constant(&id_sym);

        self.emit_instruction(Instruction::Get(const_addr), loc)
    }

    fn emit_assignment(
        &mut self,
        id: &Identifier,
        expr: &Expression,
        loc: &SourceLocation,
    ) -> Result<()> {
        self.emit_instructions(expr)?;
        let id_sym = self.sym(&id.string());
        let const_addr = self.current_chunk().add_constant(&id_sym);
        self.emit_instruction(Instruction::Set(const_addr), loc)
    }

    fn emit_constant(&mut self, value: &Value, loc: &SourceLocation) -> Result<()> {
        let const_addr = self.current_chunk().add_constant(value);
        let inst_addr = self
            .current_chunk()
            .write_instruction(Instruction::Const(const_addr));

        self.current_chunk()
            .write_line(inst_addr, inst_addr, loc.line);
        Ok(())
    }

    fn emit_lit(&mut self, datum: &datum::Datum) -> Result<()> {
        match datum.sexp() {
            datum::Sexp::Bool(true) => self.emit_instruction(Instruction::True, &datum.location)?,
            datum::Sexp::Bool(false) => {
                self.emit_instruction(Instruction::False, &datum.location)?
            }
            datum::Sexp::List(ls) if ls.is_empty() => {
                self.emit_instruction(Instruction::Nil, &datum.location)?
            }
            datum::Sexp::String(s) => {
                let interned = self.values.interned_string(s);
                self.emit_constant(&interned, &datum.location)?;
            }
            _ => {
                let value = self.values.from_datum(datum);
                self.emit_constant(&value, &datum.location)?
            }
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
