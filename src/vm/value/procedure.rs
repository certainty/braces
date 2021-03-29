use super::Value;
use crate::vm::byte_code::chunk;
use crate::vm::printer::Print;
use crate::vm::value::symbol;
use std::fmt;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ApplicationError {
    #[error("An exception occured")]
    Exception,
}

pub type LambdaResult = std::result::Result<Value, ApplicationError>;
pub type ForeignLambdaArgs = Vec<Value>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Arity {
    Fixed(u16),
    //Varying(u16),
}

pub type ForeignLambdaFunc = fn(&ForeignLambdaArgs) -> LambdaResult;

#[derive(Clone)]
pub struct ForeignLambda {
    pub arity: Arity,
    pub operation: fn(&ForeignLambdaArgs) -> LambdaResult,
}

impl<'a> fmt::Debug for ForeignLambda {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("ForeignLambda")
            .field("arity", &self.arity)
            .field("operation", &"fn(ForeignLambaArgs) -> LambdaResult")
            .finish()
    }
}

impl<'a> PartialEq for ForeignLambda {
    fn eq(&self, _: &ForeignLambda) -> bool {
        false
    }
}

impl<'a> Eq for ForeignLambda {}

impl<'a> Print for ForeignLambda {
    fn print(&self, _: &symbol::SymbolTable) -> Option<String> {
        Some(String::from("#<foreign-procedure>"))
    }
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub arity: Arity,
    pub chunk: chunk::Chunk,
}

impl Lambda {
    pub fn new(arity: Arity, chunk: chunk::Chunk) -> Self {
        Lambda { arity, chunk }
    }

    pub fn thunk(chunk: chunk::Chunk) -> Self {
        Self::new(Arity::Fixed(0), chunk)
    }
}

impl PartialEq for Lambda {
    fn eq(&self, _: &Lambda) -> bool {
        false
    }
}

impl Eq for Lambda {}

impl Print for Lambda {
    fn print(&self, _: &symbol::SymbolTable) -> Option<String> {
        Some(String::from("#<procedure>"))
    }
}
