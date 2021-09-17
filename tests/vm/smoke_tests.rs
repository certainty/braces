use crate::helpers::*;
use braces::vm::VM;

#[test]
fn test_vm_literal() {
    let mut vm = VM::default();
    let mut values = braces::vm::value::Factory::default();

    assert_result_eq(&mut vm, "#t", values.bool_true());
    assert_result_eq(&mut vm, "#f", values.bool_false());
    assert_result_eq(&mut vm, "#\\c", values.character('c'));
    assert_result_eq(&mut vm, "\"foo\"", values.interned_string("foo"));
    assert_result_eq(&mut vm, "'foo", values.symbol("foo"));
    assert_result_eq(&mut vm, "3", values.real(3));
    assert_result_eq(
        &mut vm,
        "'(#t #f)",
        values.proper_list(vec![values.bool_true(), values.bool_false()]),
    );
    assert_result_eq(
        &mut vm,
        "'(#t #t . #f)",
        values.improper_list(
            vec![values.bool_true(), values.bool_true()],
            values.bool_false(),
        ),
    );
    assert_result_eq(
        &mut vm,
        "#(#t #f)",
        values.vector(vec![values.bool_true(), values.bool_false()]),
    );
    assert_result_eq(&mut vm, "#u8(255 10)", values.byte_vector(vec![255, 10]));
}

#[test]
fn test_vm_smoke_test() {
    let mut vm = VM::default();

    let result = run_code(
        &mut vm,
        r#"
        (define (fib-tc n)
        (fib-iter 1 0 n))

        (define (fib-iter a b count)
            (if (= count 0)
                b
              (fib-iter (+ a b) a (- count 1))))

        (define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

        (fib 20)
        "#,
    )
    .unwrap();
    assert_eq!(result, vm.values.real(6765));
}
