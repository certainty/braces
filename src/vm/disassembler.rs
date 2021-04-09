use super::byte_code::chunk::{AddressType, Chunk, ConstAddressType};
use super::byte_code::Instruction;
use std::io::Write;

pub struct Disassembler<T: Write> {
    writer: T,
}

impl<T: Write> Disassembler<T> {
    pub fn new(writer: T) -> Disassembler<T> {
        Disassembler { writer }
    }

    pub fn disassemble(&mut self, chunk: &Chunk, context: &str) {
        let mut address: usize = 0;

        self.writer
            .write_fmt(format_args!("== {} ==\n", context))
            .unwrap();

        while address < chunk.code.len() {
            address = self.disassemble_instruction(chunk, address);
        }
        self.writer.write("\n".as_bytes()).unwrap();
    }

    pub fn disassemble_instruction(&mut self, chunk: &Chunk, address: usize) -> usize {
        if let Some((begin, _, _)) = chunk.find_line(address) {
            if address > 0 && begin <= address - 1 {
                self.writer.write_all("   | ".as_bytes()).unwrap();
            } else {
                self.writer
                    .write_fmt(format_args!("{:04} ", address))
                    .unwrap();
            }
        } else {
            self.writer
                .write_fmt(format_args!("{:04} ", address))
                .unwrap();
        }

        match &chunk.code[address] {
            &Instruction::Halt => self.disassemble_simple("OP_HALT", address),
            &Instruction::True => self.disassemble_simple("OP_TRUE", address),
            &Instruction::False => self.disassemble_simple("OP_FALSE", address),
            &Instruction::Nil => self.disassemble_simple("OP_NIL", address),
            &Instruction::Const(const_address) => {
                self.disassemble_constant(chunk, "OP_CONST", address, const_address)
            }
        }
    }

    fn disassemble_simple(&mut self, name: &str, address: usize) -> usize {
        self.writer.write_fmt(format_args!("{}\n", name)).unwrap();
        address + 1
    }

    fn disassemble_constant(
        &mut self,
        chunk: &Chunk,
        name: &str,
        address: AddressType,
        constant_address: ConstAddressType,
    ) -> usize {
        self.writer
            .write_fmt(format_args!(
                "{:<16} {:04}        '{:?}'\n",
                name, constant_address, &chunk.constants[constant_address as usize]
            ))
            .unwrap();

        address + 1
    }
}
