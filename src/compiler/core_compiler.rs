use crate::compiler;
use crate::compiler::representation::CoreAST;
use crate::compiler::source::Registry;
use crate::compiler::{backend, frontend, CompilationUnit};

/// The `CoreCompiler` is used to compile the `CoreAST`
/// down to the byte code representation.
///
/// It is used in the `Compiler` but also in the `Expander`.
/// The latter uses it to compile procedural macro definitions.
///
#[derive(Debug)]
pub struct CoreCompiler {
    parser: frontend::parser::CoreParser,
    backend: backend::Backend,
}

impl CoreCompiler {
    pub fn new() -> Self {
        Self {
            parser: frontend::parser::CoreParser::new(),
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
}
