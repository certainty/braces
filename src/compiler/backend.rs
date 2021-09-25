pub mod code_generator;
pub mod error;
pub mod variables;
use super::representation::CoreAST;

use super::CompilationUnit;
use crate::compiler::backend::code_generator::Target;
use crate::compiler::frontend::parser::lambda::LambdaExpression;
use crate::compiler::source::Registry;
use crate::vm::value;
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

    /// Generate a native procedure from a lambda expression
    /// This is used by the macro expander to compiler transformer procedures
    pub fn generate_lambda(
        &self,
        lambda_expression: &LambdaExpression,
    ) -> std::result::Result<value::procedure::native::Procedure, error::Error> {
        let registry = Registry::new();
        let compiled = CodeGenerator::generate_procedure(
            &registry,
            None,
            Target::Procedure(None),
            &lambda_expression.body,
            &lambda_expression.formals,
        )?;

        Ok(compiled)
    }
}
