use super::stack_trace;
use super::value::error;
use super::value::Value;
use crate::compiler;
use crate::compiler::source::SourceId;
use codespan_reporting::diagnostic::{Diagnostic, Label};
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
    #[error("CompilerBug: {}", 0)]
    CompilerBug(String),
}

impl From<compiler::error::Error> for Error {
    fn from(e: compiler::error::Error) -> Error {
        Error::CompilerError(e)
    }
}

impl Error {
    pub fn diagnostic(e: Error) -> Diagnostic<SourceId> {
        match e {
            Error::CompilerError(e) => compiler::error::Error::diagnostic(e),
            Error::RuntimeError(e, _, stack_trace, label) => {
                Self::runtime_error(e, stack_trace, label)
            }
            Error::CompilerBug(e) => Diagnostic::bug().with_message(e),
        }
    }

    fn runtime_error(
        e: error::RuntimeError,
        st: stack_trace::StackTrace,
        label: Option<String>,
    ) -> Diagnostic<SourceId> {
        match e {
            error::RuntimeError::ArgumentError(value, message) => todo!(),
            error::RuntimeError::ArithmeticError(message) => todo!(),
            error::RuntimeError::ArityError(arity, argc) => todo!(),
            error::RuntimeError::NoncallableError(v) => todo!(),
            error::RuntimeError::UndefinedVariable(s) => todo!(),
        }
    }
}
