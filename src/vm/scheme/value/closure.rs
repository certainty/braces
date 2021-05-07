use super::procedure;
use crate::vm::{byte_code::chunk::ConstAddressType, scheme::value::Value};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub up_values: Vec<Rc<Value>>,
    pub proc: Rc<procedure::Procedure>,
}

impl Closure {
    pub fn new(proc: Rc<procedure::Procedure>, up_values: Vec<Rc<Value>>) -> Self {
        Self { proc, up_values }
    }
}

impl Closure {
    pub fn get_up_value(&self, addr: ConstAddressType) -> Rc<Value> {
        self.up_values[addr as usize].clone()
    }

    pub fn set_up_value(&mut self, addr: ConstAddressType, v: Value) {
        self.up_values[addr as usize] = Rc::new(v);
    }
}

impl From<Rc<procedure::Procedure>> for Closure {
    fn from(v: Rc<procedure::Procedure>) -> Self {
        Closure {
            proc: v.clone(),
            up_values: Vec::with_capacity(v.up_value_count()),
        }
    }
}

impl From<procedure::Procedure> for Closure {
    fn from(v: procedure::Procedure) -> Self {
        let proc = Rc::new(v);
        let up_values = Vec::with_capacity(proc.up_value_count());
        Closure { proc, up_values }
    }
}
