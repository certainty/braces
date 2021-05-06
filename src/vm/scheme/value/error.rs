use super::{foreign, Symbol, Value};
use std::rc::Rc;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("UndefinedVariableError: Variable `{0:?}` is undefined")]
    UndefinedVariable(Symbol),
    #[error("ArgumentError: `{0}` is undefined")]
    ArgumentError(String),
    #[error("AppplicationError: `{0:?}` is not callable")]
    NoncallableError(Value),
    #[error("ForeignError:  {0} ")]
    ForeignError(String, Rc<foreign::Procedure>, foreign::Error),
}

pub fn non_callable(v: Value) -> RuntimeError {
    RuntimeError::NoncallableError(v)
}

pub fn undefined_variable(id: Symbol) -> RuntimeError {
    RuntimeError::UndefinedVariable(id)
}

pub fn argument_error<I: Into<String>>(message: I) -> RuntimeError {
    RuntimeError::ArgumentError(message.into())
}

pub fn foreign_error<I: Into<String>>(
    message: I,
    proc: Rc<foreign::Procedure>,
    error: foreign::Error,
) -> RuntimeError {
    RuntimeError::ForeignError(message.into(), proc, error)
}
