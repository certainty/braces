use super::byte_code::chunk::{AddressType, Chunk, ConstAddressType};
use super::byte_code::Instruction;
use super::scheme::value::lambda::Procedure;
use super::scheme::value::Value;

#[derive(Debug)]
pub struct CallFrame {
    pub proc: Procedure,
    pub ip: *const Instruction,
    pub slots: *mut Value,
}

impl CallFrame {
    pub fn new(slots: *mut Value, proc: Procedure) -> Self {
        CallFrame {
            ip: proc.code().as_ptr(),
            slots,
            proc,
        }
    }

    pub fn code(&self) -> &Chunk {
        self.proc.code()
    }

    pub fn get_slot(&self, address: ConstAddressType) -> &Value {
        unsafe { &*(self.slots.add(address as usize)) }
    }

    pub fn set_slot(&mut self, address: ConstAddressType, value: Value) {
        unsafe { *(self.slots.add(address as usize)) = value }
    }
}
