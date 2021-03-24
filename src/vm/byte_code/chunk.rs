use super::*;

type Value = u64;
// start address, end address, line number
type LineInfo = (usize, usize, u64);
type AddressType = usize;

pub struct Chunk {
    pub(crate) lines: Vec<LineInfo>,
    pub(crate) constants: Vec<Value>,
    // TODO: find more efficient encoding of lines
    pub(crate) code: Vec<OpCode>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write_line(&mut self, from: usize, to: usize, line: u64) {
        self.lines.push((from, to, line));
    }

    pub fn write_constant(&mut self, value: Value) -> ConstAddressType {
        self.constants.push(value);
        (self.constants.len() - 1) as u16
    }

    pub fn write_opcode(&mut self, op_code: OpCode) -> AddressType {
        self.code.push(op_code);
        self.code.len() - 1
    }
}
