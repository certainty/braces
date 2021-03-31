use braces::vm;
use braces::vm::scheme::value::Value;

#[test]
fn test_vm_full_cycle() {
    assert_eq!(vm::VM::run_string("#t").unwrap(), Value::Bool(true))
}
