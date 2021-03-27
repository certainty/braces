use super::*;
use crate::vm::value::Value;
use std::cmp::Ordering;

pub(crate) type LineNumber = u64;
pub(crate) type AddressType = usize;
// start address, end address, line number
pub(crate) type LineInfo = (AddressType, AddressType, LineNumber);

pub struct Chunk {
    pub(crate) lines: Vec<LineInfo>,
    pub(crate) constants: Vec<Value>,
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

    pub fn write_line(&mut self, from: AddressType, to: AddressType, line: LineNumber) {
        self.lines.push((from, to, line));
    }

    pub fn write_constant(&mut self, value: Value) -> ConstAddressType {
        self.constants.push(value);
        (self.constants.len() - 1) as ConstAddressType
    }

    pub fn read_constant(&self, addr: ConstAddressType) -> Value {
        self.constants[addr as usize]
    }

    pub fn write_opcode(&mut self, op_code: OpCode) -> AddressType {
        self.code.push(op_code);
        self.code.len() - 1
    }

    pub fn read_opcode(&self, addr: AddressType) -> &OpCode {
        &self.code[addr]
    }

    pub fn find_line(&self, address: AddressType) -> Option<LineInfo> {
        let index = self.lines.binary_search_by(|&(begin, end, _)| {
            if begin <= address && end >= address {
                Ordering::Equal
            } else if end < address {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });

        match index {
            Ok(idx) => Some(self.lines[idx]),
            Err(_) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Chunk;

    #[test]
    fn find_line_works() {
        let mut chunk = Chunk::new();
        chunk.write_line(0, 10, 123);
        chunk.write_line(11, 20, 124);

        assert_matches!(chunk.find_line(5), Some((0, 10, 123)));
        assert_matches!(chunk.find_line(0), Some((0, 10, 123)));
        assert_matches!(chunk.find_line(10), Some((0, 10, 123)));
        assert_matches!(chunk.find_line(12), Some((11, 20, 124)));
        assert_matches!(chunk.find_line(23), None);
    }
}
