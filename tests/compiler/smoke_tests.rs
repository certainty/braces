use crate::helpers::*;
use braces::vm::value::error;
use braces::vm::VM;

#[test]
fn shadow_bindings() {
    let mut vm = VM::default();

    // the parameter called `if` shadows the core if form
    let result = run_code(&mut vm, "(define (foo if) (if #t 'yes 'no)) (foo 3)");

    assert_matches!(
        result,
        Err(Error::RuntimeError(
            error::RuntimeError::NoncallableError(_),
            _,
            _,
            _
        ))
    )
}

#[test]
fn lexical_scope() {
    let mut vm = VM::default();

    let mut result = run_code(
        &mut vm,
        r#"
      ((lambda (x)
        ((lambda (x) x) 'foo)
      ) 'bar)
    "#,
    )
    .unwrap();

    assert_eq!(result, vm.values.symbol("foo"));

    result = run_code(
        &mut vm,
        r#"
      ((lambda (x) ((lambda (x) x) 'foo) x) 'bar)
    "#,
    )
    .unwrap();

    assert_eq!(result, vm.values.symbol("bar"));

    result = run_code(
        &mut vm,
        "(begin (define x 'foo) (define id (lambda (x) x)) (id #t))",
    )
    .unwrap();
    assert_eq!(result, vm.values.bool_true());
}
