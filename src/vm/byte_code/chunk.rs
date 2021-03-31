use crate::vm::byte_code::Instruction;
use crate::vm::scheme::value::Value;
use std::cmp::Ordering;

pub type LineNumber = usize;
pub type AddressType = usize;
pub type LineInfo = (AddressType, AddressType, LineNumber);

#[derive(Clone, Debug)]
pub struct Chunk {
    pub(crate) lines: Vec<LineInfo>,
    pub(crate) constants: Vec<Value>,
    pub(crate) code: Vec<Instruction>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            lines: vec![],
            constants: vec![],
            code: vec![],
        }
    }
}
