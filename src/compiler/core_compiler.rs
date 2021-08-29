use crate::compiler;
use crate::compiler::representation::CoreAST;
use crate::compiler::source::Registry;
use crate::compiler::{backend, frontend, CompilationUnit};

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
