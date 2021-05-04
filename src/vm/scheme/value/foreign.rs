use super::lambda;
use super::Value;
use thiserror::Error;
#[derive(Error, Debug)]
pub enum Error {
    #[error("Error in foreign function call")]
    ForeignError,
}

type Result<T> = std::result::Result<Error, T>;

pub type ProcedureImpl = std::rc::Rc<dyn Fn(Vec<Value>) -> Result<Value>>;

#[derive(Clone)]
pub struct Procedure {
    pub name: String,
    pub arity: lambda::Arity,
    proc: ProcedureImpl,
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
