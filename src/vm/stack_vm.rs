pub mod call_frame;
pub mod instance;

use super::{
    environment::{self, Environment},
    runtime,
    value::symbol::SymbolTable,
    BracesVM, VMResult,
};
use crate::compiler::jit_compile;
use crate::compiler::source::StringSource;
use crate::vm::disassembler::disassemble;
use crate::vm::printer;

const FRAMES_MAX: usize = 64;
const STACK_CAPACITY: usize = 255;
const STACK_MAX: usize = FRAMES_MAX + STACK_CAPACITY;

// will be configurable later
pub struct VM {
    /// the environment to start the VM with
    env: Environment,
    symbols: SymbolTable,

    /// Some limits for the VM
    max_stack: usize,
    max_frames: usize,
}

impl VM {
    pub fn interactive() -> Self {
        let mut symbols = SymbolTable::new();
        VM {
            env: runtime::default_environment(&mut symbols),
            symbols: symbols,
            max_stack: STACK_MAX,
            max_frames: FRAMES_MAX,
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        VM {
            env: environment::Environment::empty(),
            symbols: SymbolTable::new(),
            max_stack: STACK_MAX,
            max_frames: FRAMES_MAX,
        }
    }
}

impl BracesVM for VM {
    fn run_string(&mut self, input: &String) -> VMResult {
        let mut source: StringSource = StringSource::from(input.clone());
        if let Some(chunk) = jit_compile(&mut source)? {
            disassemble(&mut std::io::stdout(), &chunk, "REPL");
            instance::Instance::interprete(chunk, &mut self.symbols, &mut self.env, self.max_stack)
        } else {
            Ok(None)
        }
    }

    fn print(&mut self, value: super::value::Value) -> String {
        printer::print(&value, &self.symbols)
    }
}
