use crate::helpers::*;
use braces::vm::VM;

#[test]
fn lowlevel_macro_transformer() {
    let mut vm = VM::default();

    let result = run_code(
        &mut vm,
        r#"
        (define-syntax my-cons 
          (lowlevel-macro-transformer 
            (lambda (form) 
               `(cons ,(car form) ,(cadr form))))) 
        (my-cons 1 2)
        "#,
    )
    .unwrap();

    assert_eq!(
        result,
        vm.values
            .improper_list(vec![vm.values.real(1)], vm.values.real(2))
    );
}
