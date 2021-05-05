pub mod byte_code;
pub mod debug;
pub mod disassembler;
pub mod global;
pub mod instance;
pub mod scheme;
pub mod stack;

use crate::compiler;
use crate::compiler::source::*;
use crate::compiler::CompilationUnit;
use crate::compiler::Compiler;
use global::TopLevel;
use instance::Instance;
use scheme::value;
use scheme::value::Value;
use scheme::writer::Writer;
use std::path::PathBuf;
use thiserror::Error;

use self::scheme::value::{foreign, lambda::Arity};

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
    pub fn new(stack_size: usize) -> VM {
        VM {
            stack_size,
            values: value::Factory::default(),
            toplevel: TopLevel::new(),
            writer: Writer::new(),
        }
    }

    pub fn write(&self, value: &Value) -> String {
        self.writer.write(value, &self.values).to_string()
    }

    pub fn run_file(&mut self, path: PathBuf) -> Result<Value> {
        let mut source = FileSource::new(path);
        let mut compiler = Compiler::new();
        let unit = compiler.compile_program(&mut source)?;
        self.interprete(unit)
    }

    pub fn run_string(&mut self, inp: &str, context: &str) -> Result<Value> {
        let mut source = StringSource::new(inp, context);
        let mut compiler = Compiler::new();
        let unit = compiler.compile_program(&mut source)?;
        self.interprete(unit)
    }

    pub fn register_foreign(&mut self, proc: foreign::Procedure) -> Result<()> {
        let name = self.values.sym(proc.name.clone());
        let proc_value = self.values.foreign_procedure(proc);
        self.toplevel.set(name, proc_value);
        Ok(())
    }

    fn interprete(&mut self, unit: CompilationUnit) -> Result<Value> {
        self.values.absorb(unit.values);

        Instance::interprete(
            unit.proc,
            self.stack_size,
            &mut self.toplevel,
            &mut self.values,
        )
    }
}

pub fn hello_world(_args: Vec<Value>) -> foreign::Result<Value> {
    println!("Hello world from native!");
    Ok(Value::Unspecified)
}

pub fn scheme_inspect(args: Vec<Value>) -> foreign::Result<Value> {
    println!("Debug: {:?}", args.first().unwrap());
    Ok(Value::Unspecified)
}

impl Default for VM {
    fn default() -> Self {
        let mut vm = Self::new(64);
        vm.register_foreign(foreign::Procedure::new(
            "hello-world",
            hello_world,
            Arity::Exactly(0),
        ))
        .unwrap();

        vm.register_foreign(foreign::Procedure::new(
            "inspect",
            scheme_inspect,
            Arity::Exactly(1),
        ))
        .unwrap();

        vm
    }
}
