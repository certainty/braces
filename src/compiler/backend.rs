pub mod error;
pub mod rust;
pub mod variables;
pub mod vm;

use super::representation::CoreAST;
use super::CompilationUnit;
use crate::compiler::backend::vm::code_generator::Target;
use crate::compiler::frontend::parser::lambda::LambdaExpression;
use crate::compiler::source::Registry;
use crate::vm::value;
use vm::code_generator::CodeGenerator;

pub type Result<T> = std::result::Result<T, error::Error>;

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

    pub fn rust_pass(
        &self,
        ast: &CoreAST,
        registry: &Registry,
    ) -> std::result::Result<String, error::Error> {
        let source = rust::code_generator::CodeGenerator::generate(registry, ast)?;
        Ok(source)
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
