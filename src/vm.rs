pub mod byte_code;
pub mod debug;
pub mod disassembler;
pub mod error;
pub mod global;
pub mod instance;
pub mod scheme;
pub mod stack;
pub mod stack_trace;
pub mod value;
use self::value::procedure::foreign;
use self::value::procedure::native;
use crate::compiler::source::*;
use crate::compiler::CompilationUnit;
use crate::compiler::Compiler;
use crate::vm::disassembler::Disassembler;
use error::Error;
use global::TopLevel;
use instance::Instance;
use rustc_hash::FxHashMap;
use scheme::core;
use scheme::writer::Writer;
use std::io::stdout;
use std::path::PathBuf;
use thiserror::Error;
use value::Value;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct VM {
    stack_size: usize,
    pub values: value::Factory,
    pub toplevel: TopLevel,
    writer: Writer,
    pub settings: Settings,
}

#[derive(Debug)]
pub struct Settings {
    inner: FxHashMap<Setting, bool>,
}

impl Settings {
    pub fn new() -> Self {
        Self {
            inner: FxHashMap::default(),
        }
    }

    pub fn enable(&mut self, setting: Setting) {
        self.inner.insert(setting, true);
    }

    pub fn disable(&mut self, setting: Setting) {
        self.inner.insert(setting, false);
    }

    pub fn is_enabled(&self, setting: &Setting) -> bool {
        match self.inner.get(setting) {
            Some(v) => *v,
            _ => false,
        }
    }

    pub fn as_vec(&self) -> Vec<(Setting, bool)> {
        self.inner
            .iter()
            .map(|p| (p.0.clone(), *p.1))
            .collect::<Vec<_>>()
    }
}

impl Default for Settings {
    fn default() -> Settings {
        let mut settings = Settings::new();

        settings.disable(Setting::Debug);

        settings
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Setting {
    Debug,
}

impl std::fmt::Display for Setting {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let name = match self {
            Setting::Debug => "debug",
        };

        fmt.write_str(name)
    }
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

    // convenience function to compile and run a single file (which might refer to other files)
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

    pub fn disassemble(&self, proc: &native::Procedure) -> Result<()> {
        let mut dissassembler = Disassembler::new(stdout());

        dissassembler.disassemble(&proc.chunk, &proc.name.clone().unwrap_or(String::from("")));
        Ok(())
    }

    fn interprete(&mut self, unit: CompilationUnit) -> Result<Value> {
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
}

impl Default for VM {
    fn default() -> Self {
        let mut vm = Self::new(64);
        core::register(&mut vm);
        vm
    }
}
