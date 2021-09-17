use crate::helpers::*;
use braces::vm::VM;

#[test]
fn vm_bugs() {
    let mut vm = VM::default();

    // results in arity error
    let result = run_code(
        &mut vm,
        r#"
        (define ls (lambda x x))
        (define test (lambda () #t))
        (ls (test) #f (test))
        "#,
    )
    .unwrap();

    assert_eq!(
        result,
        vm.values.proper_list(vec![
            vm.values.bool_true(),
            vm.values.bool_false(),
            vm.values.bool_true()
        ])
    );
}
