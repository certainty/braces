pub mod byte_code;
pub mod disassembler;
pub mod instance;
pub mod scheme;

use crate::compiler;
use crate::compiler::source::*;
use crate::compiler::CompilationUnit;
use crate::compiler::Compiler;
use instance::{Instance, TopLevel};
use scheme::value;
use scheme::value::Value;
use scheme::writer::Writer;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    CompilerError(#[from] compiler::Error),
    #[error("RuntimeError: {0} at line {1}")]
    RuntimeError(String, usize),
    #[error("CompilerBug: {}", 0)]
    CompilerBug(String),
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct VM {
    stack_size: usize,
    values: value::Factory,
    toplevel: TopLevel,
    writer: Writer,
}

impl VM {
    pub fn new() -> VM {
        VM {
            stack_size: 256,
            values: value::Factory::default(),
            toplevel: TopLevel::new(),
            writer: Writer::new(),
        }
    }

    pub fn write(&self, value: &Value) -> String {
        println!("{:?}", value);
        self.writer.write(value, &self.values).to_string()
    }

    pub fn run_string(&mut self, inp: &str, context: &str) -> Result<Value> {
        let mut source = StringSource::new(inp, context);
        let mut compiler = Compiler::new();
        let unit = compiler.compile_expression(&mut source)?;
        self.interprete(unit)
    }

    fn interprete(&mut self, unit: CompilationUnit) -> Result<Value> {
        self.values.absorb(unit.values);

        Instance::interprete(
            &unit.code,
            self.stack_size,
            &mut self.toplevel,
            &mut self.values,
        )
    }
}
