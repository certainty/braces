pub mod reporting;
use super::value::error;
use crate::compiler;
use crate::vm::stack_trace;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("compiler error")]
    CompilerError(compiler::error::Error),
    #[error("RuntimeError at {1}: {0}")]
    RuntimeError(
        error::RuntimeError,
        usize,
        stack_trace::StackTrace,
        Option<String>,
    ),
    #[error("CompilerBug: {0}")]
    CompilerBug(String),
}

impl From<compiler::error::Error> for Error {
    fn from(e: compiler::error::Error) -> Error {
        Error::CompilerError(e)
    }
}
