use crate::compiler;
use crate::compiler::frontend::parser::lambda::LambdaExpression;
use crate::compiler::representation::CoreAST;
use crate::compiler::source::Registry;
use crate::compiler::{backend, CompilationUnit};
use crate::vm::value;

/// The `CoreCompiler` is used to compile the `CoreAST`
/// down to the byte code representation.
///
/// It is used in the `Compiler` but also in the `Expander`.
/// The latter uses it to compile procedural macro definitions.
///
#[derive(Debug)]
pub struct CoreCompiler {
    backend: backend::Backend,
}

impl CoreCompiler {
    pub fn new() -> Self {
        Self {
            backend: backend::Backend::new(),
        }
    }

    pub fn compile(
        &mut self,
        ast: &CoreAST,
        registry: &Registry,
    ) -> compiler::Result<CompilationUnit> {
        let unit = self.backend.pass(&ast, &registry)?;
        log::trace!("backend pass done: {:#?}", unit);
        Ok(unit)
    }

    pub fn compile_lambda(
        &mut self,
        lambda: &LambdaExpression,
    ) -> compiler::Result<value::procedure::Procedure> {
        let proc = self.backend.generate_lambda(&lambda)?;
        Ok(value::procedure::Procedure::native(proc))
    }
}
