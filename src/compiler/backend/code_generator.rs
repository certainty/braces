///////////////////////////////////////////////////////////////////////////////////////////////
//
//
// ## Internals
//
//
// ### Design choices
//
// #### Compilation of procedures
//
// The compile uses a chunk-per-procedure approach, which means that final compilation result
// is not a single contingent chunk of instructions, but each procedure will be compiled to a chunk instead.
// The individually compiled procedures are stored as values in the compilation units constants table
// and will thus be accessible by the VM. This compilation model gives rather clear semantics in terms
// of execution since it allows to easily activate individual procedures without having to do extensive
// computation of addresses.

use crate::vm::value::closure::Closure;
use crate::vm::value::procedure::Arity;

use super::variables::{Variables, VariablesRef};
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
use crate::vm::byte_code::Instruction;
#[cfg(feature = "debug_code")]
use crate::vm::disassembler::Disassembler;
use crate::vm::value;
use crate::vm::value::Value;
use crate::{
    compiler::frontend::parser::expression::identifier::Identifier,
    vm::byte_code::chunk::AddressType,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Too many locals defined")]
    TooManyLocals,
    #[error("Too many up values defined")]
    TooManyUpValues,
    #[error("CompilerBug: {0}")]
    CompilerBug(String),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Target {
    TopLevel,
    Procedure(Option<String>),
}

pub struct CodeGenerator {
    variables: VariablesRef,
    values: value::Factory,
    target: Target,
    chunk: Chunk,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Context {
    Tail,
    NonTail,
}

impl Context {
    pub fn is_tail_context(&self) -> bool {
        match self {
            Self::Tail => true,
            _ => false,
        }
    }
}

impl CodeGenerator {
    pub fn new(target: Target, parent_variables: Option<VariablesRef>) -> Self {
        let variables = Variables::child(parent_variables);

        CodeGenerator {
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

        Ok(CompilationUnit::new(
            self.values.clone(),
            Closure::new(proc, vec![]),
        ))
    }

    pub fn generate_procedure(
        parent_variables: Option<VariablesRef>,
        target: Target,
        ast: &BodyExpression,
        formals: &Formals,
    ) -> Result<value::procedure::native::Procedure> {
        let mut generator = CodeGenerator::new(target.clone(), parent_variables);

        generator.begin_scope();
        for argument in formals.identifiers() {
            generator.declare_binding(&argument)?;
        }
        generator.emit_body(ast)?;
        generator.end_scope(false)?;
        generator.emit_return()?;

        let up_value_count = generator.variables.up_value_count();

        match target {
            Target::TopLevel => Ok(value::procedure::native::Procedure::named(
                String::from("core#toplevel"),
                Arity::Exactly(0),
                generator.chunk,
                up_value_count,
            )),
            Target::Procedure(Some(name)) => Ok(value::procedure::native::Procedure::named(
                name,
                formals.arity(),
                generator.chunk,
                up_value_count,
            )),
            Target::Procedure(None) => Ok(value::procedure::native::Procedure::lambda(
                formals.arity(),
                generator.chunk,
                up_value_count,
            )),
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

    #[inline]
    fn begin_scope(&mut self) {
        self.variables.begin_scope();
    }

    fn end_scope(&mut self, pop_locals: bool) -> Result<()> {
        let processed_variables = self.variables.end_scope()?;

        if processed_variables.len() > 0 {
            for (addr, was_captured) in processed_variables {
                if was_captured {
                    self.current_chunk()
                        .write_instruction(Instruction::CloseUpValue(addr));
                } else if pop_locals {
                    self.current_chunk().write_instruction(Instruction::Pop);
                }
            }
        }
        Ok(())
    }

    #[inline]
    fn register_local(&mut self, name: Identifier) -> Result<usize> {
        self.variables.add_local(name)
    }

    #[inline]
    fn resolve_local(&self, name: &Identifier) -> Option<usize> {
        self.variables.resolve_local(name)
    }

    /////////////////////////////////////////////////
    //
    // TODO: explain how up-values are handles
    //
    /////////////////////////////////////////////////

    #[inline]
    fn resolve_up_value(&mut self, name: &Identifier) -> Result<Option<usize>> {
        self.variables.resolve_up_value(name)
    }

    #[inline]
    fn declare_binding(&mut self, id: &Identifier) -> Result<()> {
        if self.variables.is_top_level() {
            return Ok(());
        }

        //TODO: make sure the variable doesn't already exist in current scope

        // register the local in current scope
        self.register_local(id.clone())?;
        Ok(())
    }

    /////////////////////////////////////////////////////////////////////////
    //
    // VM instructions
    //
    /////////////////////////////////////////////////////////////////////////

    fn emit_instructions(&mut self, ast: &Expression, context: &Context) -> Result<()> {
        match ast {
            Expression::Identifier(id) => self.emit_get_variable(id)?,
            Expression::Assign(expr) => self.emit_set_variable(expr, context)?,
            Expression::Literal(lit) => self.emit_lit(lit.datum())?,
            Expression::Quotation(quoted) => self.emit_lit(quoted.datum())?,
            Expression::If(if_expr) => self.emit_if(if_expr, context)?,
            Expression::Define(definition) => self.emit_definition(definition)?,
            Expression::Lambda(expr) => self.emit_lambda(expr)?,
            Expression::Begin(expr) => self.emit_begin(expr, context)?,
            Expression::Command(expr) => self.emit_instructions(expr, context)?,
            Expression::Apply(expr) => self.emit_apply(expr, context)?,
        }
        Ok(())
    }

    fn emit_if(&mut self, expr: &IfExpression, context: &Context) -> Result<()> {
        self.emit_instructions(&expr.test, &Context::NonTail)?;
        let then_jump = self.emit_jump(Instruction::JumpIfFalse(0), expr.source_location())?;
        self.emit_pop()?;

        if expr.alternate.is_some() {
            self.emit_instructions(&expr.consequent, &Context::NonTail)?;
        } else {
            self.emit_instructions(&expr.consequent, context)?;
        }

        match &expr.alternate {
            Some(else_expr) => {
                let else_jump =
                    self.emit_jump(Instruction::Jump(0), else_expr.source_location())?;
                self.patch_jump(then_jump)?;
                self.emit_pop()?;
                self.emit_instructions(else_expr, context)?;
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

    // Emit instructions to call a procedure
    //
    // First we emit the instruction for the operator and the for all the operands.
    // Finally we emit the `call` instruction with the number of operands that have been applied.
    //
    // The following code: `(foo 'bar 'baz)` might results in something like:
    //
    // Const(0)
    // GetGlobal(0)
    // Const(1)
    // Const(2)
    // Call(2)
    //
    //
    fn emit_apply(&mut self, application: &ApplicationExpression, context: &Context) -> Result<()> {
        self.emit_instructions(&application.operator, &Context::NonTail)?;

        for operand in &application.operands {
            self.emit_instructions(&operand, &Context::NonTail)?;
        }

        let instr = if context.is_tail_context() {
            Instruction::TailCall(application.operands.len())
        } else {
            Instruction::Call(application.operands.len())
        };

        self.emit_instruction(instr, application.source_location())?;

        Ok(())
    }

    // Emit the instructions for a sequence of expressions
    //
    // `(begin )` introduces a new scope and thus this function
    // emits the instructions that are required to clean-up when
    // the scope is left again.
    //
    fn emit_begin(&mut self, expr: &BeginExpression, context: &Context) -> Result<()> {
        self.begin_scope();

        if expr.rest.is_empty() {
            self.emit_instructions(&expr.first, context)?;
        } else {
            self.emit_instructions(&expr.first, &Context::NonTail)?;

            for exp in &expr.rest {
                self.emit_instructions(&*exp, context)?;
            }
        }
        self.end_scope(true)?;
        Ok(())
    }

    fn emit_lambda(&mut self, expr: &LambdaExpression) -> Result<()> {
        let lambda = Self::generate_procedure(
            Some(self.variables.clone()),
            Target::Procedure(expr.label.clone()),
            &expr.body,
            &expr.formals,
        )?;
        let proc = self.values.native_procedure(lambda);
        self.emit_closure(proc, expr.source_location())
    }

    fn emit_get_variable(&mut self, id: &Identifier) -> Result<()> {
        if let Some(addr) = self.resolve_local(id) {
            self.emit_instruction(Instruction::GetLocal(addr), id.source_location())
        } else if let Some(addr) = self.resolve_up_value(id)? {
            self.emit_instruction(Instruction::GetUpValue(addr), id.source_location())
        } else {
            let id_sym = self.sym(&id.string());
            let const_addr = self.current_chunk().add_constant(id_sym);
            self.emit_instruction(Instruction::GetGlobal(const_addr), id.source_location())
        }
    }

    fn emit_set_variable(&mut self, expr: &SetExpression, context: &Context) -> Result<()> {
        // push the value of the expression
        self.emit_instructions(&expr.value, context)?;

        // is it local?
        if let Some(addr) = self.resolve_local(&expr.name) {
            self.emit_instruction(Instruction::SetLocal(addr), expr.source_location())
        } else if let Some(addr) = self.resolve_up_value(&expr.name)? {
            self.emit_instruction(Instruction::SetUpValue(addr), expr.source_location())
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

        for (idx, expr) in body.sequence.iter().enumerate() {
            // last expression
            if idx == (body.sequence.len() - 1) {
                self.emit_instructions(&expr, &Context::Tail)?;
            } else {
                self.emit_instructions(&expr, &Context::NonTail)?;
            }
        }
        Ok(())
    }

    fn emit_return(&mut self) -> Result<()> {
        self.current_chunk().write_instruction(Instruction::Return);
        Ok(())
    }

    fn emit_definition(&mut self, definition: &DefinitionExpression) -> Result<()> {
        match definition {
            DefinitionExpression::DefineProcedure(id, lambda_expr, _loc) => {
                // name of procedure is part is the label of the lambda expression
                self.emit_lambda(lambda_expr)?;
                let id_sym = self.sym(&id.string());
                let const_addr = self.current_chunk().add_constant(id_sym);

                if let Target::TopLevel = self.target {
                    self.emit_instruction(
                        Instruction::Define(const_addr),
                        &definition.source_location(),
                    )
                } else {
                    // internal define sets a local variable instead
                    todo!()
                }
            }
            DefinitionExpression::DefineSimple(id, expr, _loc) => {
                self.emit_instructions(expr, &Context::NonTail)?;
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

    // a closure is compiled to a sequence of up-value markers followed by the closure
    fn emit_closure(&mut self, value: Value, loc: &SourceLocation) -> Result<()> {
        // add up-values
        let up_values = self.variables.up_values_vec();
        for up_value in up_values {
            self.current_chunk()
                .write_instruction(Instruction::UpValue(up_value.address, up_value.is_local));
        }

        // now add the closure
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
    use crate::compiler::source::StringSource;
    use crate::compiler::Compiler;
    use crate::vm::value::procedure::native;
    use std::rc::Rc;

    // Test tail context and tail calls
    // R7RS Section 3.5
    #[test]
    fn test_compile_tail_call_in_conditional() {
        let chunk = compile("(if #t (foo))");

        assert_matches!(
            &chunk.code[..],
            [
                Instruction::True,
                Instruction::JumpIfFalse(_),
                Instruction::Pop,
                Instruction::GetGlobal(_),
                // (foo)
                Instruction::TailCall(_),
                Instruction::Jump(_),
                Instruction::Pop,
                Instruction::Const(_),
                Instruction::Return
            ]
        );

        let chunk = compile("(if #t (foo) (bar))");

        assert_matches!(
            &chunk.code[..],
            [
                Instruction::True,
                Instruction::JumpIfFalse(_),
                Instruction::Pop,
                Instruction::GetGlobal(_),
                // (foo)
                Instruction::Call(_),
                Instruction::Jump(_),
                Instruction::Pop,
                Instruction::GetGlobal(_),
                Instruction::TailCall(_),
                Instruction::Return
            ]
        )
    }

    #[test]
    fn test_compile_tail_call_in_begin() {
        let chunk = compile("(begin (foo))");

        assert_matches!(
            &chunk.code[..],
            [
                // (bar)
                Instruction::GetGlobal(_),
                Instruction::TailCall(_),
                Instruction::Return
            ]
        );

        let chunk = compile("(begin (bar) (foo))");

        assert_matches!(
            &chunk.code[..],
            [
                // (bar)
                Instruction::GetGlobal(_),
                Instruction::Call(_),
                Instruction::GetGlobal(_),
                Instruction::TailCall(_),
                Instruction::Return
            ]
        );
    }

    #[test]
    fn test_compile_tail_call_in_let() {
        let chunk = compile("(let ((x (foo))) (bar) (baz))");

        assert_matches!(
            &chunk.code[..],
            [
                Instruction::Closure(_),
                Instruction::GetGlobal(_),
                // (foo)
                Instruction::Call(_),
                // the body of the (let)
                Instruction::TailCall(_),
                Instruction::Return
            ]
        );

        let let_closure = proc_from(&chunk.constants[0]);
        assert_matches!(
            &let_closure.code().code[..],
            [
                Instruction::GetGlobal(_),
                Instruction::Call(_),
                Instruction::GetGlobal(_),
                Instruction::TailCall(_),
                Instruction::Return
            ]
        )
    }

    #[test]
    fn test_compile_closures() {
        let chunk = compile(
            "
          (define foo
             ((lambda (x)
               (y 'foo)
               (lambda () x))
              #t))

          (foo)
        ",
        );

        // top level
        assert_matches!(
            &chunk.code[..],
            [
                // create closure
                Instruction::Closure(_), // define closure
                Instruction::True,
                Instruction::Call(_), // call closure
                // define foo
                Instruction::Define(_),
                // apply foo
                Instruction::GetGlobal(_),
                Instruction::TailCall(_),
                Instruction::Return
            ]
        );

        let foo_closure = proc_from(&chunk.constants[0]); // the let closure
        assert_matches!(
            &foo_closure.code().code[..],
            [
                Instruction::GetGlobal(_), // get y
                Instruction::Const(_),     // 'foo
                Instruction::Call(_),      // apply 'foo to y
                // setup the up-value for the x constant
                Instruction::UpValue(0, true), //x #t
                // build the closure
                Instruction::Closure(_), // (lambda ..)
                Instruction::CloseUpValue(_),
                Instruction::Return
            ]
        );

        let inner_closure = proc_from(&foo_closure.code().constants[2]);
        assert_matches!(
            &inner_closure.code().code[..],
            [Instruction::GetUpValue(0), Instruction::Return]
        );
    }

    fn compile(input: &str) -> Chunk {
        let mut compiler = Compiler::new();
        let mut source = StringSource::new(input, "test");
        let unit = compiler.compile_program(&mut source).unwrap();
        unit.closure.code().clone()
    }

    fn proc_from(proc: &Value) -> Rc<native::Procedure> {
        match proc {
            Value::Procedure(n) if n.is_native() => n.as_native().clone(),
            Value::Closure(p) => p.proc.clone(),
            _ => panic!("Not a procedure"),
        }
    }
}
