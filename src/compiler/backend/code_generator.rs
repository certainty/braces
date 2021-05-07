use std::{cell::RefCell, rc::Rc};

use crate::compiler::frontend::parser::expression::lambda::{Formals, LambdaExpression};
use crate::compiler::frontend::parser::expression::Expression;
use crate::compiler::frontend::parser::expression::{
    apply::ApplicationExpression, assignment::SetExpression,
};
use crate::compiler::frontend::parser::expression::{
    body::BodyExpression, sequence::BeginExpression,
};
use crate::compiler::frontend::parser::expression::{
    conditional::IfExpression, define::DefinitionExpression,
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
use crate::{
    compiler::frontend::parser::expression::identifier::Identifier,
    vm::byte_code::chunk::AddressType,
};
use thiserror::Error;

const MAX_LOCALS: usize = 256;
const MAX_UP_VALUES: usize = MAX_LOCALS * 256;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Too many locals defined")]
    TooManyLocals,
    #[error("Too many up values defined")]
    TooManyUpValues,
    #[error("CompilerBug: {0}")]
    CompilerBug(String),
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Local {
    name: Identifier,
    depth: usize,
}

impl Local {
    pub fn new(name: Identifier, scope: usize) -> Self {
        Local { name, depth: scope }
    }

    pub fn for_vm() -> Self {
        Local {
            name: Identifier::synthetic(""),
            depth: 0,
        }
    }
}

pub struct Locals {
    max: usize,
    locals: Vec<Local>,
}

impl Locals {
    pub fn new(limit: usize) -> Self {
        let mut i = Self {
            max: limit,
            locals: Vec::with_capacity(limit),
        };

        // first slot is reserved for the vm
        i.locals.push(Local::for_vm());
        i
    }

    pub fn at<'a>(&'a self, idx: usize) -> &'a Local {
        &self.locals[idx]
    }

    pub fn add(&mut self, name: Identifier, scope_depth: usize) -> Result<()> {
        if self.locals.len() >= self.max {
            Err(Error::TooManyLocals)
        } else {
            self.locals.push(Local::new(name, scope_depth));
            Ok(())
        }
    }

    pub fn pop(&mut self) -> Result<()> {
        self.locals.pop();
        Ok(())
    }

    pub fn last_address(&self) -> ConstAddressType {
        (self.locals.len() - 1) as ConstAddressType
    }

    pub fn len(&self) -> usize {
        self.locals.len()
    }

    pub fn resolve(&self, id: &Identifier) -> Option<usize> {
        self.locals
            .iter()
            .rposition(|l| l.name == *id)
            .map(|e| e - 1)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct UpValue {
    address: usize,
    is_local: bool,
}
impl UpValue {
    pub fn new(address: usize, is_local: bool) -> Self {
        Self { address, is_local }
    }
}

pub struct UpValues {
    max: usize,
    up_values: Vec<UpValue>,
}

impl UpValues {
    pub fn new(limit: usize) -> Self {
        Self {
            max: limit,
            up_values: Vec::with_capacity(limit),
        }
    }

    pub fn add(&mut self, local_addr: usize, is_local: bool) -> Result<ConstAddressType> {
        if self.up_values.len() >= self.max {
            Err(Error::TooManyUpValues)
        } else {
            let value = UpValue::new(local_addr, is_local);
            if let Some(addr) = self.up_values.iter().position(|v| v == &value) {
                Ok(addr as ConstAddressType)
            } else {
                self.up_values.push(value);
                Ok(self.last_address())
            }
        }
    }

    pub fn len(&self) -> usize {
        self.up_values.len()
    }

    pub fn last_address(&self) -> ConstAddressType {
        (self.up_values.len() - 1) as ConstAddressType
    }
}

type VariablesRef = Rc<RefCell<Variables>>;

pub struct Variables {
    parent: Option<VariablesRef>,
    locals: Locals,
    up_values: UpValues,
}

impl Variables {
    pub fn child(parent: Option<VariablesRef>) -> VariablesRef {
        Rc::new(RefCell::new(Variables {
            locals: Locals::new(MAX_LOCALS),
            up_values: UpValues::new(MAX_UP_VALUES),
            parent,
        }))
    }

    pub fn root() -> VariablesRef {
        Self::child(None)
    }

    pub fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    pub fn locals<'a>(&'a mut self) -> &'a mut Locals {
        &mut self.locals
    }
}

#[derive(Debug, Clone)]
pub enum Target {
    TopLevel,
    Procedure(Option<String>),
}

pub struct CodeGenerator {
    scope_depth: usize,
    variables: VariablesRef,
    values: value::Factory,
    target: Target,
    chunk: Chunk,
}

impl CodeGenerator {
    pub fn new(target: Target, parent_variables: Option<VariablesRef>) -> Self {
        let variables = Variables::child(parent_variables);

        CodeGenerator {
            scope_depth: 0,
            variables,
            target,
            values: value::Factory::default(),
            chunk: Chunk::new(),
        }
    }

    pub fn generate(&mut self, ast: Vec<Expression>) -> Result<CompilationUnit> {
        let proc = Self::generate_procedure(
            None,
            Target::TopLevel,
            &Expression::body(ast),
            &Formals::empty(),
        )?;
        Ok(CompilationUnit::new(self.values.clone(), proc))
    }

    pub fn generate_procedure(
        parent_variables: Option<VariablesRef>,
        target: Target,
        ast: &BodyExpression,
        formals: &Formals,
    ) -> Result<value::procedure::Procedure> {
        let mut generator = CodeGenerator::new(target.clone(), parent_variables);

        generator.begin_scope();
        for argument in formals.identifiers() {
            generator.declare_binding(&argument)?;
        }
        generator.emit_body(ast)?;
        generator.emit_return()?;
        generator.end_scope()?;

        match target {
            Target::TopLevel => Ok(value::procedure::thunk(generator.chunk)),
            Target::Procedure(Some(name)) => Ok(value::procedure::named(
                name,
                formals.arity(),
                generator.chunk,
            )),
            Target::Procedure(None) => {
                Ok(value::procedure::lambda(formals.arity(), generator.chunk))
            }
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

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Locals and local tracking
    //
    // In order to make locals efficient and fast, they're represented as values on the stack.
    // This way access to locals is an indexed access into the VM's stack.
    // In order to make sure that the address calculation works correctly the code-generator
    // tracks (emulates) the state of the stack. Each new scope is introduced by a binding construct
    // which means there will be a new stack frame pushed.
    // TODO: add better documentation on how this works
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) -> Result<()> {
        self.scope_depth -= 1;

        let mut locals_len = self.variables.borrow().locals.len();
        let mut current_depth = self.variables.borrow().locals.at(locals_len - 1).depth;

        while locals_len > 0 && current_depth > self.scope_depth {
            self.current_chunk().write_instruction(Instruction::Pop);
            self.variables.borrow_mut().locals.pop()?;

            locals_len -= 1;
            current_depth = self.variables.borrow().locals.at(locals_len - 1).depth;
        }

        Ok(())
    }

    fn register_local(&mut self, name: Identifier) -> Result<ConstAddressType> {
        self.variables
            .borrow_mut()
            .locals
            .add(name, self.scope_depth)?;
        Ok(self.variables.borrow().locals.last_address())
    }

    fn resolve_local(&self, name: &Identifier) -> Option<usize> {
        self.variables.borrow().locals.resolve(name)
    }

    fn resolve_up_value(&mut self, name: &Identifier) -> Result<Option<ConstAddressType>> {
        if self.variables.borrow().is_root() {
            return Ok(None);
        }
        let addr = self.variables.borrow().locals.resolve(name);

        if let Some(local_addr) = addr {
            Ok(Some(self.add_up_value(local_addr, true)?))
        } else {
            Ok(None)
        }
    }

    fn add_up_value(&mut self, local_addr: usize, is_local: bool) -> Result<ConstAddressType> {
        self.variables
            .borrow_mut()
            .up_values
            .add(local_addr, is_local)
    }

    fn declare_binding(&mut self, id: &Identifier) -> Result<()> {
        // top level bindings aren't tracked on the stack
        if self.scope_depth == 0 {
            return Ok(());
        }

        // make sure the variable doesn't already exist in current scope

        // register the local in current scope
        self.register_local(id.clone())?;
        Ok(())
    }

    /////////////////////////////////////////////////////////////////////////
    //
    // VM instructions
    //
    /////////////////////////////////////////////////////////////////////////

    fn emit_instructions(&mut self, ast: &Expression) -> Result<()> {
        match ast {
            Expression::Identifier(id) => self.emit_get_variable(id)?,
            Expression::Assign(expr) => self.emit_set_variable(expr)?,
            Expression::Literal(lit) => self.emit_lit(lit.datum())?,
            Expression::Quotation(quoted) => self.emit_lit(quoted.datum())?,
            Expression::If(if_expr) => self.emit_if(if_expr)?,
            Expression::Let(let_exp) => {
                self.emit_instructions(&let_exp.to_lambda())?;
            }
            Expression::Define(definition) => self.emit_definition(definition)?,
            Expression::Lambda(expr) => self.emit_lambda(expr)?,
            Expression::Begin(expr) => self.emit_begin(expr)?,
            Expression::Command(expr) => self.emit_instructions(expr)?,
            Expression::Apply(expr) => self.emit_apply(expr)?,
        }
        Ok(())
    }

    fn emit_if(&mut self, expr: &IfExpression) -> Result<()> {
        self.emit_instructions(&expr.test)?;
        let then_jump = self.emit_jump(Instruction::JumpIfFalse(0), expr.source_location())?;
        self.emit_pop()?;
        self.emit_instructions(&expr.consequent)?;

        match &expr.alternate {
            Some(else_expr) => {
                let else_jump =
                    self.emit_jump(Instruction::Jump(0), else_expr.source_location())?;
                self.patch_jump(then_jump)?;
                self.emit_pop()?;
                self.emit_instructions(else_expr)?;
                self.patch_jump(else_jump)?;
            }
            _ => {
                let else_jump = self.emit_jump(Instruction::Jump(0), expr.source_location())?;
                self.patch_jump(then_jump)?;
                self.emit_pop()?;
                self.emit_constant(Value::Unspecified, expr.source_location())?;
                self.patch_jump(else_jump)?;
            }
        }

        Ok(())
    }

    fn emit_jump(&mut self, instr: Instruction, loc: &SourceLocation) -> Result<AddressType> {
        self.emit_instruction(instr, loc)?;
        Ok(self.current_chunk().size() - 1)
    }

    fn patch_jump(&mut self, jump_addr: AddressType) -> Result<()> {
        let to_addr = self.current_chunk().size();
        let new_instruction = match self.current_chunk().at(jump_addr) {
            &Instruction::JumpIfFalse(_) => Instruction::JumpIfFalse(to_addr),
            &Instruction::Jump(_) => Instruction::Jump(to_addr),
            _ => return Err(Error::CompilerBug("Can't patch non-jump".to_string())),
        };

        self.current_chunk().patch(jump_addr, new_instruction);
        Ok(())
    }

    fn emit_apply(&mut self, application: &ApplicationExpression) -> Result<()> {
        self.emit_instructions(&application.operator)?;
        for operand in &application.operands {
            self.emit_instructions(&operand)?;
        }
        self.emit_instruction(
            Instruction::Call(application.operands.len()),
            application.source_location(),
        )?;
        Ok(())
    }

    fn emit_begin(&mut self, expr: &BeginExpression) -> Result<()> {
        self.begin_scope();
        self.emit_instructions(&expr.first)?;
        for exp in &expr.rest {
            self.emit_instructions(&*exp)?;
        }
        self.end_scope()?;
        Ok(())
    }

    fn emit_lambda(&mut self, expr: &LambdaExpression) -> Result<()> {
        let lambda = Self::generate_procedure(
            Some(self.variables.clone()),
            Target::Procedure(None),
            &expr.body,
            &expr.formals,
        )?;
        let proc = self.values.procedure(lambda);

        self.emit_closure(proc, expr.source_location())
    }

    fn emit_get_variable(&mut self, id: &Identifier) -> Result<()> {
        if let Some(addr) = self.resolve_local(id) {
            self.emit_instruction(
                Instruction::GetLocal(addr as ConstAddressType),
                id.source_location(),
            )
        } else if let Some(addr) = self.resolve_up_value(id)? {
            self.emit_instruction(
                Instruction::GetUpValue(addr as ConstAddressType),
                id.source_location(),
            )
        } else {
            let id_sym = self.sym(&id.string());
            let const_addr = self.current_chunk().add_constant(id_sym);
            self.emit_instruction(Instruction::GetGlobal(const_addr), id.source_location())
        }
    }

    fn emit_set_variable(&mut self, expr: &SetExpression) -> Result<()> {
        // push the value of the expression
        self.emit_instructions(&expr.value)?;

        // is it local
        if let Some(addr) = self.resolve_local(&expr.name) {
            self.emit_instruction(
                Instruction::SetLocal(addr as ConstAddressType),
                expr.source_location(),
            )
        } else if let Some(addr) = self.resolve_up_value(&expr.name)? {
            self.emit_instruction(
                Instruction::SetUpValue(addr as ConstAddressType),
                expr.source_location(),
            )
        } else {
            // top level variable
            let id_sym = self.sym(&expr.name.string());
            let const_addr = self.current_chunk().add_constant(id_sym);
            self.emit_instruction(Instruction::SetGlobal(const_addr), expr.source_location())
        }
    }

    fn emit_body(&mut self, body: &BodyExpression) -> Result<()> {
        for def in &body.definitions {
            self.emit_definition(&def)?;
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

    fn emit_definition(&mut self, definition: &DefinitionExpression) -> Result<()> {
        match definition {
            DefinitionExpression::DefineSimple(id, expr, _loc) => {
                self.emit_instructions(expr)?;
                let id_sym = self.sym(&id.string());
                let const_addr = self.current_chunk().add_constant(id_sym);

                if let Target::TopLevel = self.target {
                    self.emit_instruction(
                        Instruction::Define(const_addr),
                        &definition.source_location(),
                    )
                } else {
                    // internal define
                    todo!()
                }
            }
            DefinitionExpression::Begin(_inner, _loc) => todo!(),
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

    fn emit_closure(&mut self, value: Value, loc: &SourceLocation) -> Result<()> {
        let const_addr = self.current_chunk().add_constant(value);
        let inst_addr = self
            .current_chunk()
            .write_instruction(Instruction::Closure(const_addr));

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

    fn emit_pop(&mut self) -> Result<()> {
        self.current_chunk().write_instruction(Instruction::Pop);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_local() {
        let mut locals = Locals::new(10);

        locals.add(Identifier::synthetic("foo"), 1).unwrap();
        locals.add(Identifier::synthetic("bar"), 1).unwrap();
        locals.add(Identifier::synthetic("barz"), 1).unwrap();

        assert_eq!(locals.resolve(&Identifier::synthetic("foo")), Some(0))
    }

    #[test]
    fn test_up_value_add_deduplicates() {
        let mut up_values = UpValues::new(10);

        let first_addr = up_values.add(10, true).unwrap();
        up_values.add(20, true).unwrap();
        //again
        let second_addr = up_values.add(10, true).unwrap();
        // again but not local
        let third_addr = up_values.add(10, false).unwrap();

        assert_eq!(first_addr, second_addr);
        assert_ne!(first_addr, third_addr);

        assert_eq!(
            up_values.up_values,
            vec![
                UpValue::new(10, true),
                UpValue::new(20, true),
                UpValue::new(10, false)
            ]
        )
    }
}
