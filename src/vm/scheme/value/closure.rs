use super::procedure;
use crate::vm::byte_code::chunk::Chunk;
use crate::vm::{byte_code::chunk::ConstAddressType, scheme::value::Value};
use std::{cell::RefCell, rc::Rc};

pub type RuntimeUpValue = Rc<RefCell<Value>>;
//pub type RuntimeUpValue = Rc<ReValue>;

pub fn new_up_value(v: Value) -> RuntimeUpValue {
    Rc::new(RefCell::new(v))
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub up_values: Vec<RuntimeUpValue>,
    pub proc: Rc<procedure::Procedure>,
}

impl Closure {
    pub fn new(proc: Rc<procedure::Procedure>, up_values: Vec<RuntimeUpValue>) -> Self {
        Self { proc, up_values }
    }
}

impl Closure {
    pub fn get_up_value(&self, addr: ConstAddressType) -> RuntimeUpValue {
        self.up_values[addr as usize].clone()
    }

    pub fn set_up_value(&mut self, addr: ConstAddressType, v: Value) {
        self.up_values[addr as usize].replace(v);
    }

    pub fn code<'a>(&'a self) -> &'a Chunk {
        &self.proc.code()
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
