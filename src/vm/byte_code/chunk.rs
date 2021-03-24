use super::*;

type Value = u64;

pub struct Chunk {
    pub(crate) code: Vec<OpCode>,
    pub(crate) constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
        }
    }

    pub fn write_constant(&mut self, value: Value) -> ConstAddressType {
        self.constants.push(value);
        (self.constants.len() - 1) as u16
    }

    pub fn write_opcode(&mut self, op_code: OpCode) {
        self.code.push(op_code)
    }
}
