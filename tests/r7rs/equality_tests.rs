use crate::helpers::*;
use braces::vm::value::Value;
use braces::vm::VM;

fn assert_relation(op: &str, lhs: &str, rhs: &str) {
    test_relation(op, lhs, rhs, true)
}

fn refute_relation(op: &str, lhs: &str, rhs: &str) {
    test_relation(op, lhs, rhs, false)
}

fn test_relation(op: &str, lhs: &str, rhs: &str, result: bool) {
    let mut vm: VM = VM::default();
    let code = format!("({} {} {})", op, lhs, rhs);

    assert_eq!(
        run_code(&mut vm, &code).unwrap(),
        Value::Bool(result),
        "expected {} to be {}",
        code,
        result
    );
}

#[test]
fn eqv_as_specified() {
    // see r7rs: 6.1

    // bool
    assert_relation("eqv?", "#t", "#t");
    assert_relation("eqv?", "#f", "#f");
    refute_relation("eqv?", "#f", "#t");

    // symbols
    assert_relation("eqv?", "'foo", "'foo");
    refute_relation("eqv?", "'foo", "'bar");

    //exact numbers
    assert_relation("eqv?", "1", "1");
    assert_relation("eqv?", "100000000", "100000000");
    assert_relation("eqv?", "3/4", "3/4");
    refute_relation("eqv?", "2", "2.0");

    //inexact numbers
    assert_relation("eqv?", "2.0", "2.0");
    refute_relation("eqv?", "2.0", "3.1");
    refute_relation("eqv?", "0.0", "+nan.0");

    // chars
    assert_relation("eqv?", "#\\a", "#\\a");
    refute_relation("eqv?", "#\\a", "#\\b");

    //lists
    let mut vm: VM = VM::default();
    assert_relation("eqv?", "'()", "'()");
    assert_result_eq(&mut vm, "(define x '(1 2 3)) (eqv? x x)", Value::Bool(true));
    assert_result_eq(
        &mut vm,
        "(define x '(1 2 3)) (define y '(1 2 3)) (eqv? x y)",
        Value::Bool(false),
    );

    // pairs
    refute_relation("eqv?", "(cons 1 2)", "(cons 1 2)");
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
    refute_relation("eqv?", "#(1 2)", "#(1 2)");
    assert_result_eq(&mut vm, "(define x #(1 2 3)) (eqv? x x)", Value::Bool(true));
    assert_result_eq(
        &mut vm,
        "(define x #(1 2 3)) (define y #(1 2 3)) (eqv? x y)",
        Value::Bool(false),
    );

    // bytevectors
    refute_relation("eqv?", "#u8(1 1)", "#u8(1 1)");
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
    refute_relation("eqv?", r#""foo""#, r#""foo""#);
    assert_result_eq(&mut vm, "(define x \"foo\") (eqv? x x)", Value::Bool(true));
    assert_result_eq(
        &mut vm,
        "(define x \"foo\") (define y \"foo\") (eqv? x y)",
        Value::Bool(false),
    );

    // procedures
    refute_relation("eqv?", "(lambda (x) x)", "(lambda (x) x)");
    assert_result_eq(
        &mut vm,
        "(let ((p (lambda (x) x))) (eqv? p p))",
        Value::Bool(true),
    );
}

#[test]
fn eq_as_specified() {
    //On symbols, booleans, the empty list, pairs, and records,
    //and also on non-empty strings, vectors, and bytevectors,
    //eq? and eqv? are guaranteed to have the same behavior.

    // bool
    assert_relation("eq?", "#t", "#t");
    assert_relation("eq?", "#f", "#f");
    refute_relation("eq?", "#f", "#t");

    // symbols
    assert_relation("eq?", "'foo", "'foo");
    refute_relation("eq?", "'foo", "'bar");

    //lists
    let mut vm: VM = VM::default();
    assert_relation("eq?", "'()", "'()");

    // pairs
    refute_relation("eq?", "(cons 1 2)", "(cons 1 2)");
    assert_result_eq(
        &mut vm,
        "(define x (cons 2 3)) (eq? x x)",
        Value::Bool(true),
    );
    assert_result_eq(
        &mut vm,
        "(define x (cons 1 2)) (define y (cons 1 2)) (eq? x y)",
        Value::Bool(false),
    );

    // vectors
    refute_relation("eq?", "#(1 2)", "#(1 2)");
    assert_result_eq(&mut vm, "(define x #(1 2 3)) (eq? x x)", Value::Bool(true));
    assert_result_eq(
        &mut vm,
        "(define x #(1 2 3)) (define y #(1 2 3)) (eq? x y)",
        Value::Bool(false),
    );

    // bytevectors
    refute_relation("eq?", "#u8(1 1)", "#u8(1 1)");
    assert_result_eq(
        &mut vm,
        "(define x #u8(1 2 3)) (eq? x x)",
        Value::Bool(true),
    );
    assert_result_eq(
        &mut vm,
        "(define x #u8(1 2 3)) (define y #u8(1 2 3)) (eq? x y)",
        Value::Bool(false),
    );

    // strings
    assert_result_eq(&mut vm, "(define x \"foo\") (eq? x x)", Value::Bool(true));
    assert_result_eq(
        &mut vm,
        "(define x \"foo\") (define y \"foo\") (eq? x y)",
        Value::Bool(false),
    );

    assert_result_eq(
        &mut vm,
        "(let ((p (lambda (x) x))) (eq? p p))",
        Value::Bool(true),
    );

    // the rest is implementation dependent
    assert_result_eq(&mut vm, "(define x #(1 2 3)) (eq? x x)", Value::Bool(true));
    assert_result_eq(
        &mut vm,
        "(define x #(1 2 3)) (define y #(1 2 3)) (eq? x y)",
        Value::Bool(false),
    );

    //exact numbers
    assert_relation("eq?", "1", "1");
    assert_relation("eq?", "100000000", "100000000");
    assert_relation("eq?", "3/4", "3/4");
    refute_relation("eq?", "2", "2.0");

    //inexact numbers
    assert_relation("eq?", "2.0", "2.0");
    refute_relation("eq?", "2.0", "3.1");
    refute_relation("eq?", "0.0", "+nan.0");

    // chars
    assert_relation("eq?", "#\\a", "#\\a");
    refute_relation("eq?", "#\\a", "#\\b");
}

#[test]
fn equal_as_specified() {
    // bool
    assert_relation("equal?", "#t", "#t");
    assert_relation("equal?", "#f", "#f");
    refute_relation("equal?", "#f", "#t");

    // symbols
    assert_relation("equal?", "'foo", "'foo");
    refute_relation("equal?", "'foo", "'bar");

    //exact numbers
    assert_relation("equal?", "1", "1");
    assert_relation("equal?", "100000000", "100000000");
    assert_relation("equal?", "3/4", "3/4");
    refute_relation("equal?", "2", "2.0");

    //inexact numbers
    assert_relation("equal?", "2.0", "2.0");
    refute_relation("equal?", "2.0", "3.1");
    refute_relation("equal?", "0.0", "+nan.0");

    // chars
    assert_relation("equal?", "#\\a", "#\\a");
    refute_relation("equal?", "#\\a", "#\\b");

    //lists
    let mut vm: VM = VM::default();
    assert_relation("equal?", "'()", "'()");
    assert_relation("equal?", "'(1 2 (1 2 3))", "'(1 2 (1 2 3))");
    refute_relation("equal?", "'(1 2 3)", "'(1 2 3 4 5)");

    assert_result_eq(
        &mut vm,
        "(define x '(1 2 3)) (equal? x x)",
        Value::Bool(true),
    );
    assert_result_eq(
        &mut vm,
        "(define x '(1 2 3)) (define y '(1 2 3)) (equal? x y)",
        Value::Bool(true),
    );
    assert_result_eq(
        &mut vm,
        "(define x '(1 2 3 4)) (define y '(1 2 3)) (equal? x y)",
        Value::Bool(false),
    );

    // pairs
    assert_relation("equal?", "(cons 1 2)", "(cons 1 2)");
    assert_relation("equal?", "(cons 1 (cons 1 2))", "(cons 1 (cons 1 2))");
    refute_relation("equal?", "(cons  1 2)", "(cons 1 3)");
    assert_result_eq(
        &mut vm,
        "(define x (cons 2 3)) (equal? x x)",
        Value::Bool(true),
    );
    assert_result_eq(
        &mut vm,
        "(define x (cons 1 2)) (define y (cons 1 2)) (equal? x y)",
        Value::Bool(true),
    );

    // vectors
    assert_relation("equal?", "#(1 2)", "#(1 2)");
    assert_result_eq(
        &mut vm,
        "(define x #(1 2 3)) (equal? x x)",
        Value::Bool(true),
    );
    assert_result_eq(
        &mut vm,
        "(define x #(1 2 3)) (define y #(1 2 3)) (equal? x y)",
        Value::Bool(true),
    );

    // bytevectors
    assert_relation("equal?", "#u8(1 1)", "#u8(1 1)");
    assert_result_eq(
        &mut vm,
        "(define x #u8(1 2 3)) (equal? x x)",
        Value::Bool(true),
    );
    assert_result_eq(
        &mut vm,
        "(define x #u8(1 2 3)) (define y #u8(1 2 3)) (equal? x y)",
        Value::Bool(true),
    );

    // strings
    assert_relation("equal?", r#""foo""#, r#""foo""#);
    assert_result_eq(
        &mut vm,
        "(define x \"foo\") (equal? x x)",
        Value::Bool(true),
    );
    assert_result_eq(
        &mut vm,
        "(define x \"foo\") (define y \"foo\") (equal? x y)",
        Value::Bool(true),
    );

    // procedures
    refute_relation("equal?", "(lambda (x) x)", "(lambda (x) x)");
    assert_result_eq(
        &mut vm,
        "(let ((p (lambda (x) x))) (equal? p p))",
        Value::Bool(true),
    );
}
