use braces::vm::value::{error, procedure::Arity, Value};
use braces::vm::Error;
use braces::vm::VM;
use matches::assert_matches;

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
      ((lambda (x)
        ((lambda (x) x) 'foo)
      ) 'bar)
    "#,
            "",
        )
        .unwrap();

    assert_eq!(result, vm.values.symbol("foo"));

    result = vm
        .run_string(
            r#"
      ((lambda (x) ((lambda (x) x) 'foo) x) 'bar)
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

    let result = vm.run_string("(begin (set! x #t) x)", "");
    assert_matches!(
        result,
        Err(Error::RuntimeError(
            error::RuntimeError::UndefinedVariable(_),
            _,
            _,
            _
        ))
    );

    let result = vm
        .run_string(r#"((lambda  (foo) (set! foo 'bar) foo) #t)"#, "")
        .unwrap();
    assert_eq!(result, vm.values.symbol("bar"));
}

#[test]
fn test_vm_lambda() {
    let mut vm = VM::default();

    let result = vm.run_string("((lambda (x) x) #f)", "").unwrap();
    assert_eq!(result, vm.values.bool_false());

    let result = vm.run_string("((lambda () #t))", "").unwrap();
    assert_eq!(result, vm.values.bool_true());

    let result = vm
        .run_string("(define id (lambda (x) x)) (id #t)", "")
        .unwrap();
    assert_eq!(result, vm.values.bool_true());

    let result = vm.run_string("((lambda (x) #t))", "");
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

    let result = vm
        .run_string("(define test (lambda (x) x)) (test #f)", "")
        .unwrap();
    assert_eq!(result, vm.values.bool_false());

    let result = vm
        .run_string("(define test (lambda x x)) (test)", "")
        .unwrap();
    assert_eq!(result, vm.values.proper_list(vec![]));

    let result = vm
        .run_string("(define test (lambda x x)) (test #f)", "")
        .unwrap();
    assert_eq!(result, vm.values.proper_list(vec![vm.values.bool_false()]));

    let result = vm
        .run_string("(define test (lambda x x)) (test #f #t)", "")
        .unwrap();
    assert_eq!(
        result,
        vm.values
            .proper_list(vec![vm.values.bool_false(), vm.values.bool_true()])
    );

    let result = vm
        .run_string(
            "(define test (lambda (x y z . rest) x)) (test #t #f #f)",
            "",
        )
        .unwrap();
    assert_eq!(result, vm.values.bool_true());

    let result = vm
        .run_string(
            "(define test (lambda (x y z . rest) y)) (test #f #t #f)",
            "",
        )
        .unwrap();
    assert_eq!(result, vm.values.bool_true());

    let result = vm
        .run_string(
            "(define test (lambda (x y z . rest) z)) (test #f #f #t)",
            "",
        )
        .unwrap();
    assert_eq!(result, vm.values.bool_true());

    let result = vm
        .run_string(
            "(define test (lambda (x y z . rest) rest)) (test #f #t #t)",
            "",
        )
        .unwrap();
    assert_eq!(result, vm.values.proper_list(vec![]));

    let result = vm
        .run_string(
            "(define test (lambda (x y z . rest) rest)) (test #f #f #f #t #t)",
            "",
        )
        .unwrap();
    assert_eq!(
        result,
        vm.values
            .proper_list(vec![vm.values.bool_true(), vm.values.bool_true()])
    );
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

#[test]
fn test_vm_simple_closures() {
    let mut vm = VM::default();
    let result = vm
        .run_string(
            "
               (define test
                 ((lambda (x y) (lambda () (set! x (not x)) x)) #t 'ignored)
               )
               (define ls (lambda x x))
               (ls (test) (test) (test))
",
            "",
        )
        .unwrap();

    assert_eq!(
        result,
        vm.values.proper_list(vec![
            vm.values.bool_false(),
            vm.values.bool_true(),
            vm.values.bool_false()
        ])
    );

    let result = vm
        .run_string(
            "
               (define test
                 ((lambda (x y) (lambda () x)) #t 'ignored)
               )
               (test)
",
            "",
        )
        .unwrap();
    assert_eq!(result, vm.values.bool_true());

    let result = vm
        .run_string(
            "
               (define test
                  ((lambda (x)
                     ((lambda (proc) (set! x #f) proc)  (lambda () x)))
                   #t))
               (test)
",
            "",
        )
        .unwrap();
    assert_eq!(result, vm.values.bool_false());
}

#[test]
fn test_vm_complex_closures() {
    let mut vm = VM::default();
    let result = vm
        .run_string(
            "
               (define list (lambda ls ls))
               (define set-x #t)
               (define get-x #t)
               (define make-closures (lambda (value)
                 ((lambda (x)
                   (set! get-x (lambda () x))
                   (set! set-x (lambda (new) (set! x new)))) value)))

              (make-closures #t)
              (list (get-x) (set-x 'foo) (get-x))
",
            "",
        )
        .unwrap();

    let foo_sym = vm.values.symbol("foo");
    assert_eq!(
        result,
        vm.values.proper_list(vec![
            vm.values.bool_true(),
            vm.values.unspecified(),
            foo_sym
        ])
    );
}

#[test]
fn test_vm_bugs() {
    let mut vm = VM::default();

    // results in arity error
    let result = vm
        .run_string(
            "
               (define ls (lambda x x))
               (define test (lambda () #t))
               (ls (test) #f (test))
",
            "",
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
