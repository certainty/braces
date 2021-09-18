use crate::helpers::*;
use braces::vm::value::Value;
use braces::vm::VM;

fn assert_eqv(lhs: &str, rhs: &str) {
    let mut vm: VM = VM::default();
    let code = format!("(eqv? {} {})", lhs, rhs);

    assert_eq!(
        run_code(&mut vm, &code).unwrap(),
        Value::Bool(true),
        "expected (eqv? {} {}) to be true",
        lhs,
        rhs
    );
}

fn assert_neqv(lhs: &str, rhs: &str) {
    let mut vm: VM = VM::default();
    let code = format!("(eqv? {} {})", lhs, rhs);

    assert_eq!(
        run_code(&mut vm, &code).unwrap(),
        Value::Bool(false),
        "expected (eqv? {} {}) to be false",
        lhs,
        rhs
    );
}

#[test]
fn eqv_as_specified() {
    // see r7rs: 6.1

    // bool
    assert_eqv("#t", "#t");
    assert_eqv("#f", "#f");
    assert_neqv("#f", "#t");

    // symbols
    assert_eqv("'foo", "'foo");
    assert_neqv("'foo", "'bar");

    //exact numbers
    assert_eqv("1", "1");
    assert_eqv("100000000", "100000000");
    assert_eqv("3/4", "3/4");
    assert_neqv("2", "2.0");

    //inexact numbers
    assert_eqv("2.0", "2.0");
    assert_neqv("2.0", "3.1");
    assert_neqv("0.0", "+nan.0");

    // chars
    assert_eqv("#\\a", "#\\a");
    assert_neqv("#\\a", "#\\b");

    //lists
    let mut vm: VM = VM::default();
    assert_eqv("'()", "'()");
    assert_result_eq(&mut vm, "(define x '(1 2 3)) (eqv? x x)", Value::Bool(true));
    assert_result_eq(
        &mut vm,
        "(define x '(1 2 3)) (define y '(1 2 3)) (eqv? x y)",
        Value::Bool(false),
    );

    // pairs
    assert_neqv("(cons 1 2)", "(cons 1 2)");
    assert_result_eq(
        &mut vm,
        "(define x (cons 2 3)) (eqv? x x)",
        Value::Bool(true),
    );
    assert_result_eq(
        &mut vm,
        "(define x (cons 1 2)) (define y (cons 1 2)) (eqv? x y)",
        Value::Bool(false),
    );

    // vectors
    assert_neqv("#(1 2)", "#(1 2)");
    assert_result_eq(&mut vm, "(define x #(1 2 3)) (eqv? x x)", Value::Bool(true));
    assert_result_eq(
        &mut vm,
        "(define x #(1 2 3)) (define y #(1 2 3)) (eqv? x y)",
        Value::Bool(false),
    );

    // bytevectors
    assert_neqv("#u8(1 1)", "#u8(1 1)");
    assert_result_eq(
        &mut vm,
        "(define x #u8(1 2 3)) (eqv? x x)",
        Value::Bool(true),
    );
    assert_result_eq(
        &mut vm,
        "(define x #u8(1 2 3)) (define y #u8(1 2 3)) (eqv? x y)",
        Value::Bool(false),
    );

    // strings
    assert_neqv(r#""foo""#, r#""foo""#);
}

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
        Value::Bool(true),
    );

    assert_result_eq(
        &mut vm,
        r#"
        (equal? '(1 2 3) '(1 2 3))
    "#,
        Value::Bool(true),
    )
}
