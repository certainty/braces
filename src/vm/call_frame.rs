use super::byte_code::chunk::{Chunk, ConstAddressType, LineNumber};
use super::byte_code::Instruction;
use super::scheme::value::lambda::Procedure;
use super::scheme::value::Value;
use super::stack::Frame;
use std::rc::Rc;

#[derive(Debug)]
pub struct CallFrame {
    pub proc: Rc<Procedure>,
    pub ip: usize,
    slots: Frame<Value>,
}

impl CallFrame {
    pub fn new(slots: Frame<Value>, proc: Rc<Procedure>) -> Self {
        Self { ip: 0, slots, proc }
    }

    pub fn next_instruction(&mut self) -> &Instruction {
        let inst = self.proc.code().at(self.ip);
        self.ip += 1;
        inst
    }

    #[inline]
    pub fn code(&self) -> &Chunk {
        self.proc.code()
    }

    #[inline]
    pub fn get_slot(&self, address: ConstAddressType) -> &Value {
        self.slots.get(address as usize)
    }

    #[inline]
    pub fn set_slot(&mut self, address: ConstAddressType, value: Value) {
        self.slots.set(address as usize, value)
    }

    #[inline]
    pub fn line_number_for_current_instruction(&self) -> Option<LineNumber> {
        self.proc.code().find_line(self.ip).map(|e| e.2)
    }
}
