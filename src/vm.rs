pub mod byte_code;
pub mod disassembler;
pub mod instance;
pub mod scheme;

use crate::compiler;
use crate::compiler::source::*;
use crate::compiler::Compiler;
use byte_code::chunk::Chunk;
use instance::Instance;
use scheme::value::Value;
use scheme::writer::{ExternalRepresentation, Writer};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    CompilerError(#[from] compiler::Error),
}

type Result<T> = std::result::Result<T, Error>;

pub struct VM {
    stack_size: usize,
    writer: Writer,
}

impl VM {
    pub fn new() -> VM {
        VM {
            stack_size: 256,
            writer: Writer {},
        }
    }

    pub fn write<T: ExternalRepresentation>(&self, value: &T) -> String {
        self.writer.write(value)
    }

    pub fn run_string(&mut self, inp: &str) -> Result<Value> {
        let mut source = StringSource::new(inp, "run_string");
        let mut compiler = Compiler::new();

        if let Some(chunk) = compiler.compile(&mut source)? {
            self.interprete(&chunk)
        } else {
            Ok(Value::Unspecified)
        }
    }

    fn interprete(&mut self, chunk: &Chunk) -> Result<Value> {
        Instance::interprete(chunk, self.stack_size)
    }
}
