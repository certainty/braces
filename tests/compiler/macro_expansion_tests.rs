use crate::helpers::*;
use braces::vm::VM;

#[test]
fn lowlevel_macro_transformer() {
    let mut vm = VM::default();

    // the parameter called `if` shadows the core if form
    let result = run_code(
        &mut vm,
        "(define-syntax my-macro (lowlevel-macro-transformer (lambda (form) `(cons 1 2)))) (my-macro))"
    ).unwrap();

    assert_eq!(
        result,
        vm.values
            .improper_list(vec![vm.values.real(1)], vm.values.real(2))
    );
}
