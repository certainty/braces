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

pub struct Compiler<'a> {
    sources: source::Registry<'a>,
    frontend: frontend::Frontend,
    backend: backend::Backend,
}

impl<'a> Compiler<'a> {
    pub fn new() -> Self {
        Compiler {
            sources: source::Registry::new(),
            frontend: frontend::Frontend::new(),
            backend: backend::Backend::new(),
        }
    }

    pub fn compile<T: HasOrigin + Read>(&mut self, input: T) -> Result<CompilationUnit> {
        let source = self.sources.add(input)?;
        self.compile_source(source)
    }

    fn compile_source(&mut self, source: &Source<'a>) -> Result<CompilationUnit> {
        let ast = self.frontend.pass(&source)?;
        self.backend.pass(&ast)
    }
}
