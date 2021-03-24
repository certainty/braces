use super::byte_code::chunk;
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

    fn run(&self) -> Result<(), VmError> {
        Ok(())
    }
}
