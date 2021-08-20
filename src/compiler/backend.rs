pub mod code_generator;
pub mod error;
pub mod variables;
use super::representation::CoreAST;

use super::CompilationUnit;
use code_generator::{CodeGenerator, Target};

pub struct Backend {}

impl Backend {
    pub fn new() -> Self {
        Self {}
    }

    pub fn pass(ast: &CoreAST) -> code_generator::Result<CompilationUnit> {
        let mut code_gen = CodeGenerator::new(Target::TopLevel, None);
        Ok(code_gen.generate(&ast)?)
    }
}
