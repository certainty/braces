use super::byte_code::chunk::{AddressType, Chunk, ConstAddressType};
use super::byte_code::Instruction;
use super::scheme::value;
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

        //println!("{:?}", chunk.code);
        while address < chunk.code.len() {
            //println!("address: {}, len: {}", address, chunk.code.len());
            //std::thread::sleep_ms(2000);
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
            &Instruction::Return => self.disassemble_simple("OP_RET", address),
            &Instruction::Call(args) => {
                self.disassemble_simple("OP_CALL", address)
                // This creates an endless loop
                //self.disassemble_code_at(chunk, "OP_CALL", address - (args as usize) - 1)
            }
            &Instruction::Nop => self.disassemble_simple("OP_NOP", address),
            &Instruction::Break => self.disassemble_simple("OP_BREAK", address),
            &Instruction::Pop => self.disassemble_simple("OP_POP", address),
            &Instruction::True => self.disassemble_simple("OP_TRUE", address),
            &Instruction::False => self.disassemble_simple("OP_FALSE", address),
            &Instruction::Nil => self.disassemble_simple("OP_NIL", address),
            &Instruction::GetGlobal(const_address) => {
                self.disassemble_constant(chunk, "OP_GET_GLOBAL", address, const_address)
            }
            &Instruction::GetLocal(_const_address) => {
                self.disassemble_code_at(chunk, "OP_GET_LOCAL", address)
            }
            &Instruction::SetGlobal(const_address) => {
                self.disassemble_constant(chunk, "OP_SET_GLOBAL", address, const_address)
            }
            &Instruction::SetLocal(_const_address) => {
                self.disassemble_code_at(chunk, "OP_SET_LOCAL", address)
            }

            &Instruction::Define(const_address) => {
                self.disassemble_constant(chunk, "OP_DEFINE", address, const_address)
            }
            &Instruction::Const(const_address) => {
                self.disassemble_constant(chunk, "OP_CONST", address, const_address)
            }
        }
    }

    fn disassemble_simple(&mut self, name: &str, address: usize) -> usize {
        self.writer.write_fmt(format_args!("{}\n", name)).unwrap();
        address + 1
    }

    fn disassemble_code_at(&mut self, chunk: &Chunk, name: &str, address: usize) -> usize {
        let code = &chunk.code[address];
        self.writer
            .write_fmt(format_args!("{:<16} {:?} \n", name, code))
            .unwrap();
        address + 1
    }

    fn disassemble_constant(
        &mut self,
        chunk: &Chunk,
        name: &str,
        address: AddressType,
        constant_address: ConstAddressType,
    ) -> usize {
        let constant = &chunk.constants[constant_address as usize];
        self.writer
            .write_fmt(format_args!(
                "{:<16} {:04}        '{}' mem[{:p}]\n",
                name,
                constant_address,
                &self.disassemble_value(constant),
                &constant
            ))
            .unwrap();

        address + 1
    }

    fn disassemble_value(&self, value: &value::Value) -> String {
        match value {
            value::Value::Procedure(_proc) => String::from("<procedure>"),
            _ => format!("{:?}", value),
        }
    }
}
