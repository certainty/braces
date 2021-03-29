use crate::compiler;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum VmError {
    #[error("Failed to run")]
    RuntimeError(String),
    #[error("TypeError")]
    TypeError(String),
    #[error(transparent)]
    CompileError(#[from] compiler::CompileError),
}
