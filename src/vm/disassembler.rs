use super::byte_code;
use super::byte_code::chunk::{AddressType, Chunk};
use super::byte_code::OpCode;

pub fn disassemble(chunk: &Chunk, context: &str) {
    let disass = Disassembler { chunk };
    disass.disassemble(context)
}

struct Disassembler<'a> {
    chunk: &'a Chunk,
}

impl<'a> Disassembler<'a> {
    fn disassemble(&self, context: &str) {
        let mut address: usize = 0;

        println!("== {} ==", context);

        while address < self.chunk.code.len() {
            address = self.disassemble_instruction(address);
        }
    }

    pub(crate) fn disassemble_instruction(&self, address: usize) -> usize {
        if let Some((begin, _, _)) = self.chunk.find_line(address) {
            if address > 0 && begin <= address - 1 {
                print!("   | ")
            } else {
                print!("{:04} ", address);
            }
        } else {
            print!("{:04} ", address);
        }

        match self.chunk.code[address] {
            OpCode::Return => self.disassemble_simple("OP_RETURN", address),
            OpCode::Const(const_address) => {
                self.disassemble_constant("OP_CONSTANT", address, const_address)
            }
        }
    }

    fn disassemble_simple(&self, name: &str, address: usize) -> usize {
        println!("{}", name);
        address + 1
    }

    fn disassemble_constant(
        &self,
        name: &str,
        address: AddressType,
        constant_address: byte_code::ConstAddressType,
    ) -> usize {
        println!(
            "{:<16} {:04}  '{}'",
            name, constant_address, self.chunk.constants[constant_address as usize]
        );

        address + 1
    }
}
