use crate::helpers::*;
use braces::vm::value::{error, procedure::Arity};
use braces::vm::VM;

#[test]
fn test_vm_lambda() {
    let mut vm = VM::default();
    let bool_false = vm.values.bool_false();
    let bool_true = vm.values.bool_true();

    assert_result_eq(&mut vm, "((lambda (x) x) #f)", bool_false);
    assert_result_eq(&mut vm, "((lambda () #t))", bool_true.clone());
    assert_result_eq(&mut vm, "(define id (lambda (x) x)) (id #t)", bool_true);

    let result = run_code(&mut vm, "((lambda (x) #t))");
    assert_matches!(
        result,
        Err(Error::RuntimeError(
            error::RuntimeError::ArityError(Arity::Exactly(1), _),
            _,
            _,
            _
        ))
    );
}

#[test]
fn test_vm_lambda_formals() {
    let mut vm = VM::default();
    let bool_false = vm.values.bool_false();
    let bool_true = vm.values.bool_true();

    assert_result_eq(
        &mut vm,
        "(define test (lambda (x) x)) (test #f)",
        bool_false.clone(),
    );

    let value = run_code(&mut vm, "(define test (lambda x x)) (test)").unwrap();
    assert_eq!(value, vm.values.proper_list(vec![]));

    let value = run_code(&mut vm, "(define test (lambda x x)) (test #f)").unwrap();
    assert_eq!(value, vm.values.proper_list(vec![bool_false.clone()]));

    let value = run_code(&mut vm, "(define test (lambda x x)) (test #f #t)").unwrap();
    assert_eq!(
        value,
        vm.values
            .proper_list(vec![bool_false.clone(), bool_true.clone()]),
    );

    assert_result_eq(
        &mut vm,
        "(define test (lambda (x y z . rest) x)) (test #t #f #f)",
        bool_true.clone(),
    );

    assert_result_eq(
        &mut vm,
        "(define test (lambda (x y z . rest) y)) (test #f #t #f)",
        bool_true.clone(),
    );

    assert_result_eq(
        &mut vm,
        "(define test (lambda (x y z . rest) z)) (test #f #f #t)",
        bool_true.clone(),
    );

    let value = run_code(
        &mut vm,
        "(define test (lambda (x y z . rest) rest)) (test #f #t #t)",
    )
    .unwrap();
    assert_eq!(value, vm.values.proper_list(vec![]));

    let value = run_code(
        &mut vm,
        "(define test (lambda (x y z . rest) rest)) (test #f #f #f #t #t)",
    )
    .unwrap();

    assert_eq!(
        value,
        vm.values
            .proper_list(vec![bool_true.clone(), bool_true.clone()])
    );
}
