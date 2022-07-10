use crate::compiler::error::reporting::ErrorReporter;
pub use compilation_unit::CompilationUnit;
use core_compiler::CoreCompiler;
use source::{HasOrigin, Source};
use std::io::Read;

pub mod backend;
pub mod compilation_unit;
mod core_compiler;
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

    pub fn compile_rust<T: HasOrigin + Read>(&mut self, input: &mut T) -> Result<String> {
        log::trace!("compiling source {:?}", input.origin());
        println!("attempting to build rust");
        let source = self.sources.add(input)?;
        self.compile_source_rust(&source)
    }

    pub fn print_error(&self, e: &error::Error) {
        let reporter = ErrorReporter::new(&self.sources);
        reporter.report_error(&e);
    }

    pub fn error_reporter(&self) -> ErrorReporter {
        ErrorReporter::new(&self.sources)
    }

    fn compile_source(&mut self, source: &Source) -> Result<CompilationUnit> {
        let ast = self.frontend.pass(&source)?;
        log::trace!("frontend pass done: {:#?}", ast);
        let unit = self.core.compile(&ast, &self.sources)?;
        log::trace!("core pass done: {:#?}", unit);
        Ok(unit)
    }

    fn compile_source_rust(&mut self, source: &Source) -> Result<String> {
        println!("compiling source");
        let ast = self.frontend.pass(source)?;
        log::trace!("frontend pass done {:#?}", ast);
        let rust_source = self.core.compile_rust(&ast, &self.sources)?;
        log::trace!("core pass done");
        Ok(rust_source)
    }
}
