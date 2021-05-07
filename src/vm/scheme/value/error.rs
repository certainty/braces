use super::procedure::Arity;
use super::{Symbol, Value};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("UndefinedVariableError: Variable `{0:?}` is undefined")]
    UndefinedVariable(Symbol),
    #[error("ArityError: Function expected `{0:?}` arguments")]
    ArityError(Arity, usize),
    #[error("ArgumentError: `{0}` is undefined")]
    ArgumentError(String),
    #[error("AppplicationError: `{0:?}` is not callable")]
    NoncallableError(Value),
}

pub fn arity_mismatch(arity: Arity, arg_count: usize) -> RuntimeError {
    RuntimeError::ArityError(arity, arg_count)
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