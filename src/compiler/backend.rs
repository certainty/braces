pub mod code_generator;
pub mod error;
pub mod variables;
use super::representation::CoreAST;

use super::CompilationUnit;
use crate::compiler::source::Registry;
use code_generator::CodeGenerator;

#[derive(Debug)]
pub struct Backend {}

impl Backend {
    pub fn new() -> Self {
        Self {}
    }

    /// compile the `CoreAST` into a `CompilationUnit`
    pub fn pass(
        &self,
        ast: &CoreAST,
        registry: &Registry,
    ) -> std::result::Result<CompilationUnit, error::Error> {
        let unit = CodeGenerator::generate(registry, ast)?;
        Ok(unit)
    }
}
