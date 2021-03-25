use super::byte_code::{chunk, OpCode};
use log;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum VmError {
    #[error("Failed to run")]
    RuntimeError,
    #[error("Failed to compile")]
    CompileError,
}

pub struct StackVM<'a> {
    chunk: &'a chunk::Chunk,
    ip: chunk::AddressType,
}

impl<'a> StackVM<'a> {
    pub fn interprete(chunk: &'a chunk::Chunk) -> Result<(), VmError> {
        StackVM {
            chunk: &chunk,
            ip: 0,
        }
        .run()
    }

    fn run(&mut self) -> Result<(), VmError> {
        loop {
            match self.read_op_code() {
                &OpCode::Return => break,
                &OpCode::Const(addr) => {
                    let val = self.chunk.read_constant(addr);
                    println!("{:?}", val);
                }
                _ => return Err(VmError::RuntimeError),
            }
        }

        Ok(())
    }

    fn read_op_code(&mut self) -> &OpCode {
        let code = self.chunk.read_opcode(self.ip);
        self.ip = self.ip + 1;
        code
    }
}
