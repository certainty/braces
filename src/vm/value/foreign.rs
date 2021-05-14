use super::equality::SchemeEqual;
use super::procedure;
use super::procedure::{Arity, HasArity};
use super::Value;
use crate::vm::scheme::ffi::FunctionResult;

pub type ProcedureImpl = dyn Fn(Vec<Value>) -> FunctionResult<Value>;

pub struct Procedure {
    pub name: String,
    pub arity: Arity,
    proc: Box<ProcedureImpl>,
}

impl Procedure {
    pub fn new<S, I>(name: S, op: I, arity: Arity) -> Self
    where
        S: Into<String>,
        I: 'static + Fn(Vec<Value>) -> FunctionResult<Value>,
    {
        Self {
            name: name.into(),
            arity,
            proc: Box::new(op),
        }
    }

    pub fn call(&self, arguments: Vec<Value>) -> FunctionResult<Value> {
        (self.proc)(arguments)
    }
}

impl HasArity for Procedure {
    fn arity<'a>(&'a self) -> &'a Arity {
        &self.arity
    }
}

impl HasArity for std::rc::Rc<Procedure> {
    fn arity<'a>(&'a self) -> &'a Arity {
        &self.arity
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

impl SchemeEqual<Procedure> for Procedure {
    fn is_eq(&self, other: &Procedure) -> bool {
        (&*self.proc as *const _) == (&*other.proc as *const _)
    }

    fn is_eqv(&self, other: &Procedure) -> bool {
        (&*self.proc as *const _) == (&*other.proc as *const _)
    }

    fn is_equal(&self, other: &Procedure) -> bool {
        (&*self.proc as *const _) == (&*other.proc as *const _)
    }
}
