pub mod backend;
pub mod error;
pub mod frontend;
pub mod source;
pub mod source_location;
pub mod utils;
use crate::vm::scheme::value;
use backend::code_generator;
use backend::code_generator::{CodeGenerator, Target};
use frontend::parser::Parser;
use source::Source;
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    ParseError(#[from] frontend::parser::expression::error::Error),

    #[error(transparent)]
    GenerationError(#[from] code_generator::Error),
}

pub struct Compiler {
    parser: Parser,
}

#[derive(Clone, Debug)]
pub struct CompilationUnit {
    pub values: value::Factory,
    pub proc: value::lambda::Procedure,
}

impl CompilationUnit {
    pub fn new(values: value::Factory, proc: value::lambda::Procedure) -> Self {
        CompilationUnit { values, proc }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { parser: Parser }
    }

    pub fn compile_program<T: Source>(&mut self, source: &mut T) -> Result<CompilationUnit> {
        let ast = self.parser.parse_program(source)?;
        let mut code_gen = CodeGenerator::new(Target::TopLevel);
        Ok(code_gen.generate_program(ast)?)
    }

    pub fn compile_expression<T: Source>(&mut self, source: &mut T) -> Result<CompilationUnit> {
        let ast = self.parser.parse_expression(source)?;
        let mut code_gen = CodeGenerator::new(Target::TopLevel);

        Ok(code_gen.generate(&ast)?)
    }
}
