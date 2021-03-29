pub mod backend;
pub mod frontend;
pub mod source;

use super::vm::byte_code::chunk;
use frontend::parser;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error(transparent)]
    ParseError(#[from] parser::ParseError),
    #[error(transparent)]
    CodegenerationError(#[from] backend::byte_code::GenerationError),
}

type Result<T> = std::result::Result<T, CompileError>;

pub fn jit_compile<T: source::Source>(source: &mut T) -> Result<Option<chunk::Chunk>> {
    if let Some(ast) = parser::parse(source)? {
        let mut chunk = backend::byte_code::generate(&ast)?;
        backend::byte_code::finalise(&mut chunk);
        Ok(Some(chunk))
    } else {
        Ok(None)
    }
}
