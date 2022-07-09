use super::{Arity, HasArity};
use crate::vm::instance::Instance;
use crate::vm::scheme::ffi::FunctionResult;
use crate::vm::value::access::Access;
use crate::vm::value::equality::SchemeEqual;
use crate::vm::value::Value;

pub type ProcedureImpl = dyn Fn(&mut Instance, Vec<Value>) -> FunctionResult<Access<Value>>;

pub struct Procedure {
    pub name: String,
    pub arity: Arity,
    proc: Box<ProcedureImpl>,
}

impl Procedure {
    pub fn new<S, I>(name: S, op: I, arity: Arity) -> Self
    where
        S: Into<String>,
        I: 'static + Fn(&mut Instance, Vec<Value>) -> FunctionResult<Access<Value>>,
    {
        Self {
            name: name.into(),
            arity,
            proc: Box::new(op),
        }
    }

    pub fn call(&self, vm: &mut Instance, arguments: Vec<Value>) -> FunctionResult<Access<Value>> {
        (self.proc)(vm, arguments)
    }
}

impl HasArity for Procedure {
    fn arity(&self) -> &Arity {
        &self.arity
    }
}

impl HasArity for std::rc::Rc<Procedure> {
    fn arity(&self) -> &Arity {
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
