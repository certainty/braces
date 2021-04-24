use crate::compiler::frontend::parser::expression::{
    BindingSpec, BodyExpression, DefinitionExpression, Expression, Formals, Identifier,
    LambdaExpression, LetExpression, LiteralExpression,
};
use crate::compiler::frontend::parser::sexp::datum;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use crate::compiler::CompilationUnit;
use crate::vm::byte_code::chunk::Chunk;
use crate::vm::byte_code::chunk::ConstAddressType;
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

impl Local {
    pub fn new(name: Identifier, scope: usize) -> Self {
        Local { name, depth: scope }
    }
}

pub enum Target {
    TopLevel,
    Procedure,
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
        self.emit_return()?;

        #[cfg(feature = "debug_code")]
        Disassembler::new(std::io::stdout()).disassemble(self.current_chunk(), &"toplevel");
        let unit = CompilationUnit::new(self.values.clone(), self.current_chunk().clone());

        Ok(unit)
    }

    fn generate_procedure(
        name: Option<String>,
        arity: value::lambda::Arity,
        ast: &BodyExpression,
        loc: &SourceLocation,
    ) -> Result<value::lambda::Procedure> {
        let mut generator = CodeGenerator::new();
        // generate code to access locals
        // generate the body
        generator.emit_body(ast, loc)?;
        generator.emit_return()?;

        #[cfg(feature = "debug_code")]
        let proc_name = name.clone().unwrap_or(String::from("lambda"));
        #[cfg(feature = "debug_code")]
        Disassembler::new(std::io::stdout()).disassemble(&generator.chunk, &proc_name);

        match name {
            Some(name) => Ok(value::lambda::Procedure::named(
                name,
                arity,
                generator.chunk,
            )),
            _ => Ok(value::lambda::Procedure::lambda(arity, generator.chunk)),
        }
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

        while self.locals.len() > 0 && self.locals[self.locals.len() - 1].depth > self.scope_depth {
            self.current_chunk().write_instruction(Instruction::Pop);
            self.locals.pop();
        }
    }

    fn add_local(&mut self, name: Identifier) -> Result<ConstAddressType> {
        if self.locals.len() >= MAX_LOCALS {
            Err(Error::TooManyLocals)
        } else {
            self.locals.push(Local::new(name, self.scope_depth));
            Ok((self.locals.len() - 1) as ConstAddressType)
        }
    }

    fn resolve_local(&self, name: &Identifier) -> Option<ConstAddressType> {
        for (addr, local) in self.locals.iter().enumerate().rev() {
            if &local.name == name {
                return Some(addr as ConstAddressType);
            }
        }
        None
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
            Expression::Let(LetExpression::Let(bindings, body), loc) => {
                self.begin_scope();
                self.emit_bindings(&bindings)?;
                self.emit_body(&body, loc)?;
                self.end_scope();
            }
            Expression::Define(definition, loc) => self.emit_definition(definition, &loc)?,
            Expression::Lambda(expr, loc) => self.emit_lambda(expr, &loc)?,
        }
        Ok(())
    }

    fn emit_lambda(&mut self, expr: &LambdaExpression, loc: &SourceLocation) -> Result<()> {
        self.begin_scope();

        let arity = match &expr.formals {
            Formals::RestArg(_) => value::lambda::Arity::Variadic,
            Formals::VarArg(head, _) => value::lambda::Arity::FixedWithRest(head.len()),
            Formals::ArgList(args) => value::lambda::Arity::Fixed(args.len()),
        };

        let lambda = Self::generate_procedure(None, arity, &expr.body, &loc)?;
        let proc = self.values.procedure(lambda);
        self.emit_constant(proc, &loc)
    }

    fn emit_read_variable(&mut self, id: &Identifier, loc: &SourceLocation) -> Result<()> {
        if let Some(addr) = self.resolve_local(id) {
            self.emit_instruction(Instruction::GetLocal(addr), loc)
        } else {
            let id_sym = self.sym(&id.string());
            let const_addr = self.current_chunk().add_constant(id_sym);
            self.emit_instruction(Instruction::Get(const_addr), loc)
        }
    }

    fn emit_bindings(&mut self, bindings: &Vec<BindingSpec>) -> Result<()> {
        for binding in bindings {
            self.emit_assignment(&binding.0, &binding.1, &binding.1.source_location())?;
        }

        Ok(())
    }

    fn emit_body(&mut self, body: &BodyExpression, loc: &SourceLocation) -> Result<()> {
        for def in &body.definitions {
            self.emit_definition(&def, loc)?;
        }

        for expr in &body.sequence {
            self.emit_instructions(&expr)?;
        }
        Ok(())
    }

    fn emit_return(&mut self) -> Result<()> {
        self.current_chunk().write_instruction(Instruction::Return);
        Ok(())
    }

    fn emit_definition(
        &mut self,
        definition: &DefinitionExpression,
        loc: &SourceLocation,
    ) -> Result<()> {
        match definition {
            DefinitionExpression::DefineSimple(id, expr) => {
                self.emit_instructions(expr)?;
                let id_sym = self.sym(&id.string());
                let const_addr = self.current_chunk().add_constant(id_sym);
                self.emit_instruction(Instruction::Define(const_addr), loc)
            }
            DefinitionExpression::Begin(_inner) => todo!(),
        }
    }

    fn emit_assignment(
        &mut self,
        id: &Identifier,
        expr: &Expression,
        loc: &SourceLocation,
    ) -> Result<()> {
        self.emit_instructions(expr)?;

        if self.scope_depth > 0 {
            // local variable
            let const_addr = self.add_local(id.clone())?;
            self.emit_instruction(Instruction::SetLocal(const_addr), loc)
        } else {
            // top level variable
            let id_sym = self.sym(&id.string());
            let const_addr = self.current_chunk().add_constant(id_sym);
            self.emit_instruction(Instruction::Set(const_addr), loc)
        }
    }

    fn emit_constant(&mut self, value: Value, loc: &SourceLocation) -> Result<()> {
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
                self.emit_constant(interned, &datum.location)?;
            }
            _ => {
                let value = self.values.from_datum(datum);
                self.emit_constant(value, &datum.location)?
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
