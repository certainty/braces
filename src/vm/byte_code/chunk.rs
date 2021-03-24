use super::*;

type Value = u64;

pub struct Chunk {
    code: Vec<OpCode>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
        }
    }

    pub fn disassemble(chunk: &Chunk, context: &str) {
        Disassembler::new(&chunk).disassemble(context);
    }

    pub fn write_constant(&mut self, value: Value) {
        self.constants.push(value)
    }

    pub fn write_opcode(&mut self, op_code: OpCode) {
        self.code.push(op_code)
    }
}

struct Disassembler<'a> {
    chunk: &'a Chunk,
}

impl<'a> Disassembler<'a> {
    pub fn new(chunk: &'a Chunk) -> Disassembler<'a> {
        Disassembler { chunk }
    }

    pub fn disassemble(&self, context: &str) {
        let mut address: usize = 0;

        println!("== {} ==", context);

        while address < self.chunk.code.len() {
            address = self.disassemble_instruction(address);
        }
    }

    fn disassemble_instruction(&self, address: usize) -> usize {
        print!("{:04} ", address);

        match self.chunk.code[address] {
            OP_RETURN => self.disassemble_simple("OP_RETURN", address),
            OP_CONSTANT => self.disassemble_constant("OP_CONSTANT", address),
        }
    }

    fn disassemble_simple(&self, name: &str, address: usize) -> usize {
        println!("{}", name);
        address + 1
    }

    fn disassemble_constant(&self, name: &str, address: usize) -> usize {
        let constant_address = self.chunk.code[address + 1];

        println!(
            "{} {:04}  '{}'",
            name, constant_address, self.chunk.constants[constant_address as usize]
        );

        address + 2
    }
}
