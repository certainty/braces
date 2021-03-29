pub mod byte_code;
pub mod disassembler;
pub mod environment;
pub mod error;
pub mod hash_map;
pub mod printer;
pub mod runtime;
pub mod stack_vm;
pub mod value;

use stack_vm::VM;

pub type VMResult = std::result::Result<Option<value::Value>, error::VmError>;

pub trait BracesVM {
    fn run_string(&mut self, source: &String) -> VMResult;
    fn print(&mut self, value: value::Value) -> String;

    // fn set(name, value)
}

pub fn default() -> impl BracesVM {
    VM::default()
}

pub fn interactive() -> impl BracesVM {
    VM::interactive()
}
