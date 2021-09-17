use braces::compiler::source::StringSource;
use braces::compiler::Compiler;
use braces::vm::value::Value;
pub use braces::vm::Error;
pub use braces::vm::Result;
use braces::vm::VM;
pub use matches::assert_matches;

pub fn run_code(vm: &mut VM, code: &str) -> Result<Value> {
    let mut source = StringSource::new(code);
    let mut compiler = Compiler::new();
    let unit = compiler.compile(&mut source)?;
    vm.interpret(unit)
}

pub fn assert_result_eq(vm: &mut VM, code: &str, expected: Value) {
    assert_eq!(run_code(vm, code).unwrap(), expected)
}
