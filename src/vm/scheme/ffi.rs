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

#[derive(Debug)]
pub struct VmContext {
    symbol_counter: u64,
    values: Factory,
}

impl VmContext {
    pub fn new() -> Self {
        Self {
            values: Factory::default(),
            symbol_counter: 180,
        }
    }
    pub fn gen_sym(&mut self) -> Value {
        let next_count = self.symbol_counter;
        let sym = self.values.symbol(format!("#:G{}", next_count));
        self.symbol_counter += 1;
        sym
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
