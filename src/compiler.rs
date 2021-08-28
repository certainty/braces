use std::io::Read;

pub use compilation_unit::CompilationUnit;
use source::{HasOrigin, Source};

use crate::compiler::representation::CoreAST;
use crate::compiler::source::Registry;

pub mod backend;
pub mod compilation_unit;
pub mod error;
pub mod frontend;
pub mod representation;
pub mod source;
pub mod utils;

pub type Result<T> = std::result::Result<T, error::Error>;

#[derive(Debug)]
pub struct Compiler {
    sources: source::Registry,
    frontend: frontend::Frontend,
    core: CoreCompiler,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            sources: source::Registry::new(),
            frontend: frontend::Frontend::new(),
            core: CoreCompiler::new(),
        }
    }

    pub fn compile<T: HasOrigin + Read>(&mut self, input: &mut T) -> Result<CompilationUnit> {
        log::trace!("compiling source {:?}", input.origin());
        let source = self.sources.add(input)?;
        self.compile_source(&source)
    }

    pub fn print_error(&self, _e: &error::Error) {
        todo!()
    }

    fn compile_source(&mut self, source: &Source) -> Result<CompilationUnit> {
        let ast = self.frontend.pass(&source)?;
        log::trace!("frontend pass done: {:#?}", ast);
        let unit = self.core.compile(&ast, &self.sources)?;
        log::trace!("core pass done: {:#?}", unit);
        Ok(unit)
    }
}

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

    pub fn compile(&mut self, ast: &CoreAST, registry: &Registry) -> Result<CompilationUnit> {
        let unit = self.backend.pass(&ast, &registry)?;
        log::trace!("backend pass done: {:#?}", unit);
        Ok(unit)
    }
}
