use braces::vm::scheme::value::Value;
use braces::vm::VM;

#[test]
fn test_vm_full_cycle() {
    let mut vm = VM::new();
    assert_eq!(vm.run_string("#t").unwrap(), Value::Bool(true))
}
