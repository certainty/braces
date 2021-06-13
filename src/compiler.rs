pub mod backend;
pub mod error;
pub mod frontend;
pub mod source;
pub mod source_location;
pub mod utils;
use crate::vm::value;
use backend::code_generator;
use backend::code_generator::{CodeGenerator, Target};
use frontend::parser::expression::Expression;
use frontend::reader::sexp::datum::Datum;
use source::Source;
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    ParseError(#[from] frontend::parser::Error),

    #[error(transparent)]
    GenerationError(#[from] code_generator::Error),
}

pub struct Compiler;

#[derive(Clone, Debug)]
pub struct CompilationUnit {
    pub values: value::Factory,
    pub closure: value::closure::Closure,
}

impl CompilationUnit {
    pub fn new(values: value::Factory, closure: value::closure::Closure) -> Self {
        CompilationUnit { values, closure }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Compiler
    }

    pub fn compile_program<T: Source>(&mut self, source: &mut T) -> Result<CompilationUnit> {
        self.compile(frontend::parse_all(source)?)
    }

    pub fn compile_expression<T: Source>(&mut self, source: &mut T) -> Result<CompilationUnit> {
        self.compile(frontend::parse(source)?)
    }

    fn compile(&mut self, ast: frontend::parser::ast::Ast) -> Result<CompilationUnit> {
        let mut code_gen = CodeGenerator::new(Target::TopLevel, None);
        Ok(code_gen.generate(ast.expressions)?)
    }

    pub fn compile_lambda(&mut self, datum: &Datum) -> Result<value::procedure::Procedure> {
        let lambda_expr = Expression::parse_lambda(datum)?;
        let proc = CodeGenerator::generate_procedure(
            None,
            Target::Procedure(None),
            &lambda_expr.body,
            &lambda_expr.formals,
        )?;

        Ok(value::procedure::Procedure::native(proc))
    }
}
