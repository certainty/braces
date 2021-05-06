use braces::vm::scheme::value::Value;
use braces::vm::VM;

#[test]
fn test_vm_literal() {
    let mut vm = VM::default();

    assert_eq!(vm.run_string("#t", "test").unwrap(), Value::Bool(true));
    assert_eq!(vm.run_string("#false", "test").unwrap(), Value::Bool(false));
    assert_eq!(
        vm.run_string("'#false", "test").unwrap(),
        Value::Bool(false)
    );
    assert_eq!(vm.run_string("'#true", "test").unwrap(), Value::Bool(true));
}

#[test]
fn test_vm_lexical_scope() {
    let mut vm = VM::default();

    let mut result = vm
        .run_string(
            r#"
      (let ((x 'bar))
        (let ((x 'foo))
          x))
    "#,
            "",
        )
        .unwrap();

    assert_eq!(result, vm.values.symbol("foo"));

    result = vm
        .run_string(
            r#"
      (let ((x 'bar))
        (let ((x 'foo))
          x)
        x)
    "#,
            "",
        )
        .unwrap();

    assert_eq!(result, vm.values.symbol("bar"));

    result = vm
        .run_string(
            "(begin (define x 'foo) (define id (lambda (x) x)) (id #t))",
            "",
        )
        .unwrap();
    assert_eq!(result, vm.values.bool_true());
}

#[test]
fn test_vm_set() {
    let mut vm = VM::default();

    let mut result = vm.run_string("(begin (set! x #t) x)", "").unwrap();
    assert_eq!(result, vm.values.bool_true());

    result = vm
        .run_string(r#"(let ((foo #t)) (set! foo 'bar) foo) "#, "")
        .unwrap();

    assert_eq!(result, vm.values.symbol("bar"));
}

#[test]
fn test_vm_lambda() {
    let mut vm = VM::default();

    let mut result = vm.run_string("((lambda (x) x) #f)", "").unwrap();
    assert_eq!(result, vm.values.bool_false());

    result = vm.run_string("((lambda () #t))", "").unwrap();
    assert_eq!(result, vm.values.bool_true());

    result = vm
        .run_string("(begin (define id (lambda (x) x)) (id #t))", "")
        .unwrap();
    assert_eq!(result, vm.values.bool_true());
}
#[test]
fn test_vm_conditional() {
    let mut vm = VM::default();

    let mut result = vm.run_string("(if #f #t #f)", "").unwrap();
    assert_eq!(result, vm.values.bool_false());

    result = vm.run_string("(if #t #t #f)", "").unwrap();
    assert_eq!(result, vm.values.bool_true());

    result = vm.run_string("(if #f #t)", "").unwrap();
    assert_eq!(result, vm.values.unspecified());

    result = vm.run_string("(if #t #t)", "").unwrap();
    assert_eq!(result, vm.values.bool_true());
}
