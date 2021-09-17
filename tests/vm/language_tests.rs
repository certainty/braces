use braces::vm::value::{error, Value};
use braces::vm::VM;

use crate::helpers::*;

#[test]
fn if_works() {
    let mut vm = VM::default();

    assert_result_eq(&mut vm, "(if #f #t #f)", Value::Bool(false));
    assert_result_eq(&mut vm, "(if #t #t #f)", Value::Bool(true));
    assert_result_eq(&mut vm, "(if #f #t)", Value::Unspecified);
    assert_result_eq(&mut vm, "(if #t #t)", Value::Bool(true));
}

#[test]
fn set_works() {
    let mut vm = VM::default();

    let result = run_code(&mut vm, "(begin (set! x #t) x)");
    assert_matches!(
        result,
        Err(Error::RuntimeError(
            error::RuntimeError::UndefinedVariable(_),
            _,
            _,
            _
        ))
    );

    let result = run_code(&mut vm, r#"((lambda  (foo) (set! foo 'bar) foo) #t)"#).unwrap();
    assert_eq!(result, vm.values.symbol("bar"));
}
