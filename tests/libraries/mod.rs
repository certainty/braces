use crate::helpers::*;
use braces::vm::VM;

#[test]
fn library_definition_smoke_test() {
    let mut vm = VM::default();
    let result = run_code(
        &mut vm,
        r#"
        (define-library (example grid)
          (export make rows cols ref each (rename put! set!))
          (import (scheme base))

          (begin
            (define (rows) 'rows)
            (define (cols) 'cols)
            (define (each) 'each)
            (define (put!) 'put!)
          ))

        (import (example))
        (rows)
        "#,
    )
    .unwrap();
    assert_eq!(result, vm.values.symbol("rows"));
}
