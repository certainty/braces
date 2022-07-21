use crate::vm::value::access::Reference;
use crate::vm::value::Value;
use crate::vm::value::{error, procedure::Arity};
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

pub fn positional_and_rest_procedure1(
    args: &Vec<Value>,
) -> FunctionResult<(&Value, Vec<Reference<Value>>)> {
    match &args[..] {
        [first, Value::ProperList(rest)] => Ok((first, rest.to_vector())),

        other => {
            println!("other: {:?}", other);
            Err(error::arity_mismatch(Arity::AtLeast(1), args.len()))
        }
    }
}

pub fn positional_and_rest_procedure2(
    args: &Vec<Value>,
) -> FunctionResult<(&Value, &Value, &[Reference<Value>])> {
    match &args[..] {
        [first, second, Value::Vector(rest)] => Ok((first, second, &rest.slice())),
        _ => Err(error::arity_mismatch(Arity::AtLeast(2), args.len())),
    }
}

pub fn positional_and_rest_procedure3(
    args: &Vec<Value>,
) -> FunctionResult<(&Value, &Value, &Value, &[Reference<Value>])> {
    match &args[..] {
        [first, second, third, Value::Vector(rest)] => Ok((first, second, third, &rest.slice())),
        _ => Err(error::arity_mismatch(Arity::AtLeast(3), args.len())),
    }
}
