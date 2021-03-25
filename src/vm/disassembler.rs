use super::byte_code;
use super::byte_code::chunk::{AddressType, Chunk};
use super::byte_code::OpCode;
use std::io::Write;

pub fn disassemble(chunk: &Chunk, context: &str) {
    let mut disass = Disassembler {
        chunk,
        out: &mut std::io::stdout(),
    };
    disass.disassemble(context)
}

struct Disassembler<'a, Out: Write> {
    chunk: &'a Chunk,
    out: &'a mut Out,
}

impl<'a, Out: Write> Disassembler<'a, Out> {
    fn disassemble(&mut self, context: &str) {
        let mut address: usize = 0;

        self.out
            .write_fmt(format_args!("== {} ==", context))
            .unwrap();

        while address < self.chunk.code.len() {
            address = self.disassemble_instruction(address);
        }
    }

    pub(crate) fn disassemble_instruction(&mut self, address: usize) -> usize {
        if let Some((begin, _, _)) = self.chunk.find_line(address) {
            if address > 0 && begin <= address - 1 {
                self.out.write_all("   | ".as_bytes()).unwrap();
            } else {
                self.out.write_fmt(format_args!("{:04} ", address)).unwrap();
            }
        } else {
            self.out.write_fmt(format_args!("{:04} ", address)).unwrap();
        }

        match self.chunk.code[address] {
            OpCode::Return => self.disassemble_simple("OP_RETURN", address),
            OpCode::Const(const_address) => {
                self.disassemble_constant("OP_CONSTANT", address, const_address)
            }
        }
    }

    fn disassemble_simple(&mut self, name: &str, address: usize) -> usize {
        self.out.write_all(name.as_bytes()).unwrap();
        address + 1
    }

    fn disassemble_constant(
        &mut self,
        name: &str,
        address: AddressType,
        constant_address: byte_code::ConstAddressType,
    ) -> usize {
        self.out
            .write_fmt(format_args!(
                "{:<16} {:04}  '{}'",
                name, constant_address, self.chunk.constants[constant_address as usize]
            ))
            .unwrap();

        address + 1
    }
}
