use super::value::closure::Closure;
use crate::vm::byte_code::chunk::{Chunk, LineNumber};

// A callframe is a piece of control data
// that is associated with every live-function
#[derive(Debug)]
pub struct CallFrame {
    pub closure: Closure,
    pub ip: usize,
    pub stack_base: usize,
}

impl CallFrame {
    pub fn new(closure: Closure, stack_base: usize) -> Self {
        Self {
            ip: 0,
            stack_base,
            closure,
        }
    }

    #[inline]
    pub fn set_ip(&mut self, address: usize) {
        self.ip = address
    }

    #[inline]
    pub fn code(&self) -> &Chunk {
        self.closure.code()
    }

    #[inline]
    pub fn line_number_for_current_instruction(&self) -> Option<LineNumber> {
        self.closure.code().find_line(self.ip).map(|e| e.2)
    }
}
