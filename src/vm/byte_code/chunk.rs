use crate::vm::byte_code::Instruction;
use crate::vm::value::Value;
use std::cmp::Ordering;

pub type ConstAddressType = u16;
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

    pub fn as_ptr(&self) -> *const Instruction {
        self.code.as_ptr()
    }

    pub fn at(&self, index: usize) -> &Instruction {
        &self.code[index]
    }

    pub fn patch(&mut self, index: usize, inst: Instruction) {
        self.code[index] = inst;
    }

    pub fn size(&self) -> usize {
        self.code.len()
    }

    //TODO: make this work when called multiple time with the same addresses
    //in that case it shouldn't just push but expand the interval it matches

    pub fn write_line(&mut self, from: AddressType, to: AddressType, line: LineNumber) {
        self.lines.push((from, to, line));
    }

    // TODO: deduplicate constants
    // Since they're constant it's fine to have multiple references to the same constant
    // The idea here is that we don't use the chunk in code generation directly.
    // Instead we use a chunk-builder, that in the end creates the chunk.
    // The chunk-builder, doesn't store the constants in a vector but rather in a map<value, offset>.
    // When the chunk is created, the builder creates a vector with the correct data in it then.
    pub fn add_constant(&mut self, value: Value) -> ConstAddressType {
        self.constants.push(value);
        (self.constants.len() - 1) as ConstAddressType
    }

    pub fn read_constant(&self, addr: ConstAddressType) -> &Value {
        &self.constants[addr as usize]
    }

    pub fn write_instruction(&mut self, instr: Instruction) -> AddressType {
        self.code.push(instr);
        self.code.len() - 1
    }

    pub fn read_instruction(&self, addr: AddressType) -> &Instruction {
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
