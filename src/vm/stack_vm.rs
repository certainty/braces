pub mod call_frame;
pub mod instance;

use super::{BracesVM, VMResult};
use crate::compiler::jit_compile;
use crate::compiler::source::StringSource;
use crate::vm::disassembler::disassemble;

// will be configurable later
pub struct VM {}
impl Default for VM {
    fn default() -> Self {
        VM {}
    }
}

impl BracesVM for VM {
    fn run_string(&self, input: &String) -> VMResult {
        let mut source: StringSource = StringSource::from(input.clone());
        if let Some(chunk) = jit_compile(&mut source)? {
            disassemble(&mut std::io::stdout(), &chunk, "REPL");
            instance::Instance::interprete(chunk)
        } else {
            Ok(None)
        }
    }
}
