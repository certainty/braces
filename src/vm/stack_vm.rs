use super::byte_code::{chunk, OpCode};
use super::disassembler;
use log;
use std::io::Write;
use std::str;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum VmError {
    #[error("Failed to run")]
    RuntimeError,
    #[error("Failed to compile")]
    CompileError,
}

struct TracingWriter;

impl Write for TracingWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let s = str::from_utf8(buf).unwrap();
        log::trace!("{}", s);
        Ok(s.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub struct StackVM<'a> {
    chunk: &'a chunk::Chunk,
    ip: chunk::AddressType,
    tracing_write: &'a mut TracingWriter,
}

impl<'a> StackVM<'a> {
    pub fn interprete(chunk: &'a chunk::Chunk) -> Result<(), VmError> {
        StackVM {
            chunk: &chunk,
            ip: 0,
            tracing_write: &mut TracingWriter,
        }
        .run()
    }

    fn run(&mut self) -> Result<(), VmError> {
        loop {
            disassembler::disassemble_instruction(self.tracing_write, self.chunk, self.ip);

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
