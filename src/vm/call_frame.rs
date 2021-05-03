use super::byte_code::chunk::{Chunk, LineNumber};
use super::scheme::value::lambda::Procedure;
use std::rc::Rc;

#[derive(Debug)]
pub struct CallFrame {
    pub proc: Rc<Procedure>,
    pub ip: usize,
    pub stack_base: usize,
}

impl CallFrame {
    pub fn new(proc: Rc<Procedure>, stack_base: usize) -> Self {
        Self {
            ip: 0,
            stack_base,
            proc,
        }
    }

    #[inline]
    pub fn code(&self) -> &Chunk {
        self.proc.code()
    }

    #[inline]
    pub fn line_number_for_current_instruction(&self) -> Option<LineNumber> {
        self.proc.code().find_line(self.ip).map(|e| e.2)
    }
}
