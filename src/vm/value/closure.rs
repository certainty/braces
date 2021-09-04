use super::procedure;
use crate::vm::byte_code::chunk::Chunk;
use crate::vm::value::equality::SchemeEqual;
use crate::vm::value::RefValue;
use crate::vm::Value;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub up_values: Vec<RefValue>,
    pub proc: Rc<procedure::native::Procedure>,
}

impl Closure {
    pub fn new(proc: procedure::native::Procedure) -> Self {
        Self::from_rc(Rc::new(proc), vec![])
    }

    pub fn from_rc(proc: Rc<procedure::native::Procedure>, up_values: Vec<RefValue>) -> Self {
        Self { proc, up_values }
    }

    pub fn code(&self) -> &Chunk {
        self.procedure().code()
    }

    pub fn procedure_rc(&self) -> Rc<procedure::native::Procedure> {
        self.proc.clone()
    }

    pub fn procedure(&self) -> &procedure::native::Procedure {
        &self.proc
    }

    pub fn name(&self) -> &Option<String> {
        &self.procedure().name()
    }
}

impl Closure {
    pub fn get_up_value(&self, addr: usize) -> RefValue {
        self.up_values[addr].clone()
    }

    pub fn set_up_value(&mut self, addr: usize, v: Value) {
        self.up_values[addr].set(v);
    }
}

impl From<Rc<procedure::native::Procedure>> for Closure {
    fn from(v: Rc<procedure::native::Procedure>) -> Self {
        Closure {
            proc: v.clone(),
            up_values: Vec::with_capacity(v.up_value_count),
        }
    }
}

impl From<procedure::native::Procedure> for Closure {
    fn from(v: procedure::native::Procedure) -> Self {
        let proc = Rc::new(v);
        let up_values = Vec::with_capacity(proc.up_value_count);

        Closure { proc, up_values }
    }
}

impl SchemeEqual<Closure> for Closure {
    fn is_eq(&self, other: &Closure) -> bool {
        Rc::ptr_eq(&self.proc, &other.proc)
    }
    fn is_eqv(&self, other: &Closure) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &Closure) -> bool {
        self.is_eq(other)
    }
}
