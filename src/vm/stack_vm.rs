use super::byte_code::chunk::Value;
use super::byte_code::{chunk, OpCode};
use thiserror::Error;

#[cfg(feature = "debug_vm")]
use super::disassembler;

const STACK_CAPACITY: usize = 255;

#[derive(Error, Debug)]
pub enum VmError {
    #[error("Failed to run")]
    RuntimeError,
    #[error("Failed to compile")]
    CompileError,
}

pub struct StackVM<'a> {
    ip: chunk::AddressType,
    stack: Vec<Value>,
    chunk: &'a chunk::Chunk,
}

impl<'a> StackVM<'a> {
    pub fn interprete(chunk: &'a chunk::Chunk) -> Result<Option<Value>, VmError> {
        StackVM {
            chunk: &chunk,
            ip: 0,
            stack: Vec::with_capacity(STACK_CAPACITY),
        }
        .run()
    }

    fn run(&mut self) -> Result<Option<Value>, VmError> {
        loop {
            #[cfg(feature = "debug_vm")]
            self.debug_cycle();

            match self.read_op_code() {
                &OpCode::Exit => {
                    let v = self.stack.pop();
                    return Ok(v);
                }
                &OpCode::Const(addr) => {
                    let val = self.chunk.read_constant(addr);
                    self.stack.push(val);
                }
                &OpCode::FxAdd => {
                    let lhs = self.stack.pop().unwrap();
                    let rhs = self.stack.pop().unwrap();
                    self.stack.push(lhs + rhs)
                }
            }
        }
    }

    fn read_op_code(&mut self) -> &OpCode {
        let code = self.chunk.read_opcode(self.ip);
        self.ip = self.ip + 1;
        code
    }

    #[cfg(feature = "debug_vm")]
    fn debug_cycle(&self) {
        self.print_stack_trace();
        disassembler::disassemble_instruction(&mut std::io::stdout(), self.chunk, self.ip);
    }

    #[cfg(feature = "debug_vm")]
    fn print_stack_trace(&self) {
        print!("     ");
        for value in self.stack.iter() {
            print!("[{}]", value);
        }
        println!("")
    }
}
