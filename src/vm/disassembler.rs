use super::byte_code::chunk::{AddressType, Chunk, ConstAddressType};
use super::byte_code::Instruction;
use super::value;
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
            &Instruction::Return => self.disassemble_simple("OP_RET", address),
            &Instruction::Save => self.disassemble_simple("OP_SAVE", address),
            &Instruction::Restore => self.disassemble_simple("OP_RESTORE", address),
            &Instruction::Call(_args) => self.disassemble_simple("OP_CALL", address),
            &Instruction::Closure(addr) => {
                self.disassemble_closure(chunk, "OP_CLOSURE", address, addr)
            }
            &Instruction::UpValue(addr, is_local) => {
                self.disassemble_variable_access("OP_UP_VALUE", address, addr, is_local)
            }
            &Instruction::CloseUpValue => self.disassemble_simple("OP_CLOSE_UP_VALUE", address),
            &Instruction::Nop => self.disassemble_simple("OP_NOP", address),
            &Instruction::Break => self.disassemble_simple("OP_BREAK", address),
            &Instruction::Pop => self.disassemble_simple("OP_POP", address),
            &Instruction::True => self.disassemble_simple("OP_TRUE", address),
            &Instruction::False => self.disassemble_simple("OP_FALSE", address),
            &Instruction::Nil => self.disassemble_simple("OP_NIL", address),
            &Instruction::JumpIfFalse(addr) => {
                self.disassemble_jump("OP_JUMP_IF_FALSE", addr, address)
            }
            &Instruction::Jump(addr) => self.disassemble_jump("OP_JUMP", addr, address),
            &Instruction::GetGlobal(const_address) => {
                self.disassemble_constant(chunk, "OP_GET_GLOBAL", address, const_address)
            }
            &Instruction::GetUpValue(addr) => {
                self.disassemble_variable_access("OP_GET_UP_VALUE", address, addr, true)
            }
            &Instruction::GetLocal(addr) => {
                self.disassemble_variable_access("OP_GET_LOCAL", address, addr, true)
            }
            &Instruction::SetGlobal(const_address) => {
                self.disassemble_constant(chunk, "OP_SET_GLOBAL", address, const_address)
            }
            &Instruction::SetUpValue(addr) => {
                self.disassemble_variable_access("OP_SET_UP_VALUE", address, addr, false)
            }
            &Instruction::SetLocal(addr) => {
                self.disassemble_variable_access("OP_SET_LOCAL", address, addr, true)
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

    fn disassemble_jump(&mut self, name: &str, offset: usize, address: usize) -> usize {
        self.writer
            .write_fmt(format_args!(
                "{:<16} {:04} -> {:04}\n",
                name, address, offset
            ))
            .unwrap();

        address + 1
    }

    fn disassemble_closure(
        &mut self,
        chunk: &Chunk,
        name: &str,
        address: AddressType,
        constant_address: ConstAddressType,
    ) -> usize {
        let proc = &chunk.constants[constant_address as usize];
        self.writer
            .write_fmt(format_args!(
                "{:<16} {:04}        {}\n",
                name,
                constant_address,
                &self.disassemble_value(proc)
            ))
            .unwrap();
        address + 1
    }

    fn disassemble_variable_access(
        &mut self,
        name: &str,
        address: AddressType,
        constant_address: ConstAddressType,
        is_local: bool,
    ) -> usize {
        let label = if is_local { "local" } else { "upvalue" };

        self.writer
            .write_fmt(format_args!(
                "{:<16} {:04}         {}\n",
                name, constant_address, label
            ))
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
                "{:<16} {:04}        {}    @ {:p}\n",
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
            value::Value::Closure(closure) => format!("#<procedure {}>", closure.proc.name()),
            value::Value::Procedure(proc) => format!("#<procedure {}>", proc.name()),
            value::Value::Symbol(sym) => sym.as_str().to_string(),
            _ => format!("{:?}", value),
        }
    }
}
