use crate::compiler::frontend::parser::expression::Identifier;
use crate::compiler::frontend::parser::expression::{Expression, LiteralExpression};
use crate::compiler::frontend::parser::sexp::datum;
use crate::compiler::source_location::SourceLocation;
use crate::compiler::CompilationUnit;
use crate::vm::byte_code::chunk::Chunk;
use crate::vm::byte_code::Instruction;
#[cfg(feature = "debug_code")]
use crate::vm::disassembler::Disassembler;
use crate::vm::scheme::value;
use crate::vm::scheme::value::Value;
use thiserror::Error;

const MAX_LOCALS: usize = 256;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Too many locals defined")]
    TooManyLocals,
}

type Result<T> = std::result::Result<T, Error>;

pub struct Local {
    name: Identifier,
    depth: usize,
}

pub struct CodeGenerator {
    scope_depth: usize,
    locals: Vec<Local>,
    values: value::Factory,
    chunk: Chunk,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            scope_depth: 0,
            locals: Vec::with_capacity(MAX_LOCALS),
            values: value::Factory::default(),
            chunk: Chunk::new(),
        }
    }

    pub fn generate(&mut self, ast: &Expression) -> Result<CompilationUnit> {
        self.emit_instructions(ast)?;
        self.current_chunk().write_instruction(Instruction::Halt);

        #[cfg(feature = "debug_code")]
        Disassembler::new(std::io::stdout()).disassemble(self.current_chunk(), "code");

        let unit = CompilationUnit::new(
            self.values.symbol_set(),
            self.values.string_set(),
            self.current_chunk().clone(),
        );

        Ok(unit)
    }

    #[inline]
    fn sym(&mut self, s: &str) -> Value {
        self.values.symbol(s)
    }

    #[inline]
    fn intern(&mut self, s: &str) -> Value {
        self.values.interned_string(s)
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
    }

    fn add_local(&mut self, name: Identifier) -> Result<()> {
        if self.locals.len() >= MAX_LOCALS {
            Err(Error::TooManyLocals)
        } else {
            let local = Local {
                name,
                depth: self.scope_depth,
            };
            self.locals.push(local);
            Ok(())
        }
    }

    fn emit_instructions(&mut self, ast: &Expression) -> Result<()> {
        match ast {
            Expression::Identifier(id, loc) => self.emit_read_variable(id, loc)?,
            Expression::Assign(id, expr, loc) => self.emit_assignment(id, expr, loc)?,
            Expression::Literal(LiteralExpression::SelfEvaluating(constant)) => {
                self.emit_lit(constant)?
            }
            Expression::Literal(LiteralExpression::Quotation(datum)) => self.emit_lit(datum)?,
            Expression::If(_if_expr, _loc) => todo!(),
            Expression::Let(_let_expr, _loc) => todo!(),
            Expression::Define(_definition, _loc) => todo!(),
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
                let interned = self.intern(s);
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
