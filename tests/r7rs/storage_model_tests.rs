use crate::helpers::*;
use braces::vm::VM;

#[test]
fn locals_modified_locally() {
    let mut vm = VM::default();
    let result = run_code(
        &mut vm,
        r#"
        (define (list . x) x)
        
        (define (foo x) (set! x #t) x)
        (define y #f)
        (list (foo y) y)
        "#,
    )
    .unwrap();
    assert_eq!(
        result,
        vm.values
            .proper_list(vec![vm.values.bool_true(), vm.values.bool_false()]),
    );
}
#[test]
fn set_cons_cell() {
    let mut vm = VM::default();
    let result = run_code(
        &mut vm,
        r#"
        (define ls '(#t #f #f))
        (set! (car ls) #f)
        ls
        "#,
    )
    .unwrap();
    assert_eq!(
        result,
        vm.values.proper_list(vec![
            vm.values.bool_false(),
            vm.values.bool_false(),
            vm.values.bool_false(),
        ]),
    );
}

#[test]
fn set_references_to_cons_cells() {
    let mut vm = VM::default();
    let result = run_code(
        &mut vm,
        r#"
        (define (ls . args) args)
        (define x '(1 2 3 4))
        (define y '(5 6 7))
        (set! (car x) (car y))
        (set! (car y) 0)
        (ls x y)
        "#,
    )
    .unwrap();
    let expected = vm.values.proper_list(vec![
        vm.values.proper_list(vec![
            vm.values.real(5),
            vm.values.real(2),
            vm.values.real(3),
            vm.values.real(4),
        ]),
        vm.values.proper_list(vec![
            vm.values.real(0),
            vm.values.real(6),
            vm.values.real(7),
        ]),
    ]);

    assert_eq!(result, expected);
}

#[test]
fn set_improper_list() {
    let mut vm = VM::default();
    let result = run_code(
        &mut vm,
        r#"
        (define ls '(1 2 3 . 4))
        (set! (car ls) 3)
        (car ls)
        "#,
    )
    .unwrap();

    assert_eq!(result, vm.values.real(3));
}

#[test]
fn set_vector_ref() {
    let mut vm = VM::default();
    let result = run_code(
        &mut vm,
        r#"
        (define v #(1 2 3))
        (set! (vector-ref v 0) 5)
        (vector-ref v 0)
        "#,
    )
    .unwrap();

    assert_eq!(result, vm.values.real(5));
}
