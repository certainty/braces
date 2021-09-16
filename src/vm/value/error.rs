use super::procedure::Arity;
use super::{Symbol, Value};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Undefined Variable")]
    UndefinedVariable(Symbol),
    #[error("ArityError: Function expected `{0:?}` arguments but received {1}")]
    ArityError(Arity, usize),
    #[error("ArgumentError: {1}")]
    ArgumentError(Value, String),
    #[error("ApplicationError: `{0:?}` is not callable")]
    NoncallableError(Value),
    #[error("OutOfBoundError")]
    OutOfBoundError(usize, std::ops::Range<usize>),
    #[error("ArithmeticError: `{0}`")]
    ArithmeticError(String),
    #[error("SyntaxError")]
    SyntaxError(String),
}

pub fn syntax_error<T: Into<String>>(msg: T) -> RuntimeError {
    RuntimeError::SyntaxError(msg.into())
}

pub fn out_of_bound_error<R: Into<std::ops::Range<usize>>>(
    idx: usize,
    accepted: R,
) -> RuntimeError {
    RuntimeError::OutOfBoundError(idx, accepted.into())
}

pub fn arithmetic_error<T: Into<String>>(msg: T) -> RuntimeError {
    RuntimeError::ArithmeticError(msg.into())
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

pub fn argument_error<I: Into<String>>(v: Value, message: I) -> RuntimeError {
    RuntimeError::ArgumentError(v, message.into())
}
