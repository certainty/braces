pub mod byte_code;
pub mod debug;
pub mod disassembler;
pub mod error;
pub mod global;
pub mod instance;
pub mod scheme;
pub mod settings;
pub mod stack;
pub mod stack_trace;
pub mod value;

use self::value::procedure::foreign;
use self::value::procedure::native;
use crate::compiler::CompilationUnit;
use crate::compiler::Compiler;
use crate::vm::disassembler::Disassembler;
use global::TopLevel;
use instance::Instance;
use scheme::core;
use scheme::writer::Writer;
use std::io::stdout;
use thiserror::Error;
use value::Value;

pub use error::Error;
pub use settings::{Setting, Settings};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct VM {
    stack_size: usize,
    pub values: value::Factory,
    pub toplevel: TopLevel,
    writer: Writer,
    pub settings: Settings,
}

impl VM {
    pub fn new(stack_size: usize) -> VM {
        VM {
            stack_size,
            values: value::Factory::default(),
            toplevel: TopLevel::new(),
            writer: Writer::new(),
            settings: Settings::default(),
        }
    }

    pub fn binding_names(&self) -> Vec<String> {
        self.toplevel.binding_names()
    }

    pub fn write(&self, value: &Value) -> String {
        self.writer.write(value, &self.values).to_string()
    }

    pub fn register_foreign(&mut self, proc: foreign::Procedure) -> Result<()> {
        let name = self.values.sym(proc.name.clone());
        let proc_value = self.values.foreign_procedure(proc);
        self.toplevel.set(name, proc_value);
        Ok(())
    }

    pub fn disassemble(&self, proc: &native::Procedure) -> Result<()> {
        let mut disassembler = Disassembler::new(stdout());

        disassembler.disassemble(&proc.chunk, &proc.name.clone().unwrap_or(String::from("")));
        Ok(())
    }

    pub fn interpret(&mut self, unit: CompilationUnit) -> Result<Value> {
        let debug_mode = self.settings.is_enabled(&Setting::Debug);
        self.values.absorb(unit.values);

        Instance::interprete(
            unit.closure,
            self.stack_size,
            &mut self.toplevel,
            &mut self.values,
            debug_mode,
        )
    }

    pub fn print_error(&self, _e: &Error, _compiler: &Compiler) {
        todo!()
    }
}

impl Default for VM {
    fn default() -> Self {
        let mut vm = Self::new(64);
        core::register(&mut vm);
        vm
    }
}
