use crate::vm::value::{error, procedure::Arity};
use crate::vm::value::{Factory, Value};
use thiserror::Error;

pub type FunctionResult<T> = std::result::Result<T, error::RuntimeError>;
pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Can't convert rust value to scheme value")]
    ConversionError,
    #[error(transparent)]
    SchemeError(error::RuntimeError),
}

trait ToScheme {
    fn to_scheme(&self) -> Value;
}

impl ToScheme for bool {
    fn to_scheme(&self) -> Value {
        Value::Bool(self.clone())
    }
}

// Helpers

pub fn ternary_procedure(args: &Vec<Value>) -> FunctionResult<(&Value, &Value, &Value)> {
    match &args[..] {
        [first, second, third] => Ok((first, second, third)),
        _ => Err(error::arity_mismatch(Arity::Exactly(3), args.len())),
    }
}

pub fn binary_procedure(args: &Vec<Value>) -> FunctionResult<(&Value, &Value)> {
    match &args[..] {
        [first, second] => Ok((first, second)),
        _ => Err(error::arity_mismatch(Arity::Exactly(2), args.len())),
    }
}

pub fn unary_procedure(args: &Vec<Value>) -> FunctionResult<&Value> {
    match &args[..] {
        [first] => Ok(first),
        _ => Err(error::arity_mismatch(Arity::Exactly(1), args.len())),
    }
}
