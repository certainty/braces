use super::lambda;
use super::Value;
use thiserror::Error;
#[derive(Error, Debug)]
pub enum Error {
    #[error("Error in foreign function call")]
    ForeignError,
}

pub type Result<T> = std::result::Result<T, Error>;

pub type ProcedureImpl = dyn Fn(Vec<Value>) -> Result<Value>;

pub struct Procedure {
    pub name: String,
    pub arity: lambda::Arity,
    proc: Box<ProcedureImpl>,
}

impl Procedure {
    pub fn new<S, I>(name: S, op: I, arity: lambda::Arity) -> Self
    where
        S: Into<String>,
        I: 'static + Fn(Vec<Value>) -> Result<Value>,
    {
        Self {
            name: name.into(),
            arity,
            proc: Box::new(op),
        }
    }

    pub fn call(&self, arguments: Vec<Value>) -> Result<Value> {
        (self.proc)(arguments)
    }
}

impl std::fmt::Debug for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("ForeignProcedure({})", self.name))
    }
}

impl PartialEq for Procedure {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
