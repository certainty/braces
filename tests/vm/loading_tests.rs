use crate::helpers::*;
use braces::vm::VM;

#[test]
fn test_vm_load_simple() {
    let mut vm = VM::default();
    let result = vm.values.real(3);

    assert_result_eq(&mut vm, "(load \"../fixtures/loaded_file.scm\")", result)
}

// add tests for failed loading

// test that loaded definitions are available afterwards
