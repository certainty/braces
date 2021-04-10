use braces::vm::scheme::value::Value;
use braces::vm::VM;

#[test]
fn test_vm_full_cycle() {
    let mut vm = VM::new();
    assert_eq!(vm.run_string("#t", "test").unwrap(), Value::Bool(true));
    assert_eq!(vm.run_string("#false", "test").unwrap(), Value::Bool(false));
    assert_eq!(
        vm.run_string("'#false", "test").unwrap(),
        Value::Bool(false)
    );
    assert_eq!(vm.run_string("'#true", "test").unwrap(), Value::Bool(true));
    assert_eq!(vm.run_string("'foo", "test").unwrap(), Value::symbol("foo"));
    assert_eq!(
        vm.run_string("#\\a", "test").unwrap(),
        Value::character('a')
    );
    assert_eq!(
        vm.run_string("'#\\a", "test").unwrap(),
        Value::character('a')
    );
}
