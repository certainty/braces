use crate::helpers::*;
use braces::vm::VM;

// TODO: split up
#[test]
fn test_vm_simple_closures() {
    let mut vm = VM::default();
    let result = run_code(
        &mut vm,
        r#"
        (define (list . rest) rest)
        (define test 
           (let ((x #t)
                 (y 'ignored))
             (lambda () (set! x (not x)) x)))
        (list (test) (test) (test))
        "#,
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

    let result = run_code(
        &mut vm,
        r#"
         (define test
           ((lambda (x y) (lambda () x)) #t 'ignored)
         )
         (test)
        "#,
    )
    .unwrap();
    assert_eq!(result, vm.values.bool_true());

    let result = run_code(
        &mut vm,
        r#"
        (define test
          ((lambda (x)
            ((lambda (proc) (set! x #f) proc)  (lambda () x)))
                   #t))
        (test)
        "#,
    )
    .unwrap();
    assert_eq!(result, vm.values.bool_false());
}

#[test]
fn test_vm_complex_closures() {
    let mut vm = VM::default();
    let result = run_code(
        &mut vm,
        r#"
        (define list (lambda ls ls))
        (define set-x #t)
        (define get-x #t)
        (define make-closures (lambda (value)
           ((lambda (x)
              (set! get-x (lambda () x))
              (set! set-x (lambda (new) (set! x new)))) value)))

        (make-closures #t)
        (list (get-x) (set-x 'foo) (get-x))
        "#,
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
