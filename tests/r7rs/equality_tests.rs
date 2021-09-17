use crate::helpers::*;
use braces::vm::value::Value;
use braces::vm::VM;

#[test]
fn list_is_eq_to_itself() {
    let mut vm = VM::default();

    assert_result_eq(
        &mut vm,
        r#"
    (define x '(1 2 3))
    (eq? x x)
    "#,
        Value::Bool(true),
    )
}

#[test]
fn list_is_not_eq_to_other() {
    let mut vm = VM::default();

    assert_result_eq(
        &mut vm,
        r#"
    (define x '(1 2 3))
    (define y '(1 2 3))
    (eq? x y)
    "#,
        Value::Bool(false),
    );

    assert_result_eq(
        &mut vm,
        r#"
        (eq? '(1 2 3) '(1 2 3))
    "#,
        Value::Bool(false),
    )
}

#[test]
fn list_is_equal_to_itself() {
    let mut vm = VM::default();

    assert_result_eq(
        &mut vm,
        r#"
    (define x '(1 2 3))
    (equal? x x)
    "#,
        Value::Bool(true),
    )
}

#[test]
fn list_is_equal_to_other_with_equal_elements() {
    let mut vm = VM::default();

    assert_result_eq(
        &mut vm,
        r#"
    (define x '(1 2 3))
    (define y '(1 2 3))
    (equal? x y)
    "#,
        Value::Bool(false),
    );

    assert_result_eq(
        &mut vm,
        r#"
        (equal? '(1 2 3) '(1 2 3))
    "#,
        Value::Bool(false),
    )
}
