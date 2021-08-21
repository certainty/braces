pub mod backend;
pub mod compilation_unit;
pub mod error;
pub mod frontend;
pub mod representation;
pub mod source;
pub mod utils;
use source::{HasOrigin, Source};
use std::io::Read;

pub type Result<T> = std::result::Result<T, error::Error>;

pub use compilation_unit::CompilationUnit;

pub struct Compiler {
    sources: source::Registry,
    frontend: frontend::Frontend,
    backend: backend::Backend,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            sources: source::Registry::new(),
            frontend: frontend::Frontend::new(),
            backend: backend::Backend::new(),
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
        let unit = self.backend.pass(&ast, &self.sources)?;
        log::trace!("backend pass done: {:#?}", unit);
        Ok(unit)
    }
}
