use braces::vm::scheme::value::Value;
use braces::vm::VM;

#[test]
fn test_vm_full_cycle() {
    let mut vm = VM::new();
    assert_eq!(vm.run_string("#t").unwrap(), Value::Bool(true));
    assert_eq!(vm.run_string("#false").unwrap(), Value::Bool(false));
    assert_eq!(vm.run_string("'#false").unwrap(), Value::Bool(false));
    assert_eq!(vm.run_string("'#true").unwrap(), Value::Bool(true));
    assert_eq!(vm.run_string("'foo").unwrap(), Value::symbol("foo"));
}
