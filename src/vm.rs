pub mod byte_code;
pub mod scheme;

use crate::compiler;
use crate::compiler::source::*;
use crate::compiler::Compiler;
use byte_code::chunk::Chunk;
use scheme::value;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    CompilerError(#[from] compiler::Error),
}

type Result<T> = std::result::Result<T, Error>;

pub struct VM {}

impl VM {
    pub fn run_string(inp: &str) -> Result<value::Value> {
        let mut source = StringSource::new(inp, "run_string");
        let mut compiler = Compiler::new();
        let mut vm = VM {};

        if let Some(chunk) = compiler.compile(&mut source)? {
            vm.interprete(&chunk)
        } else {
            Ok(value::Value::Unspecified)
        }
    }

    fn interprete(&mut self, chunk: &Chunk) -> Result<value::Value> {
        Ok(value::Value::Bool(true))
    }
}
