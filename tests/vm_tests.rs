use braces::compiler::source::StringSource;
use braces::compiler::Compiler;
use braces::vm::value::{error, procedure::Arity, Value};
use braces::vm::Error;
use braces::vm::Result;
use braces::vm::VM;
use matches::assert_matches;

fn run_code(vm: &mut VM, code: &str) -> Result<Value> {
    let mut source = StringSource::new(code);
    let mut compiler = Compiler::new();
    let unit = compiler.compile(&mut source)?;
    vm.interpret(unit)
}

#[inline]
fn assert_result_eq(vm: &mut VM, code: &str, expected: Value) {
    assert_eq!(run_code(vm, code).unwrap(), expected)
}

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
fn test_vm_lexical_scope() {
    let mut vm = VM::default();

    let mut result = run_code(
        &mut vm,
        r#"
      ((lambda (x)
        ((lambda (x) x) 'foo)
      ) 'bar)
    "#,
    )
    .unwrap();

    assert_eq!(result, vm.values.symbol("foo"));

    result = run_code(
        &mut vm,
        r#"
      ((lambda (x) ((lambda (x) x) 'foo) x) 'bar)
    "#,
    )
    .unwrap();

    assert_eq!(result, vm.values.symbol("bar"));

    result = run_code(
        &mut vm,
        "(begin (define x 'foo) (define id (lambda (x) x)) (id #t))",
    )
    .unwrap();
    assert_eq!(result, vm.values.bool_true());
}

#[test]
fn test_vm_set() {
    let mut vm = VM::default();

    /*
    let result = run_code(&mut vm, "(begin (set! x #t) x)");
    assert_matches!(
        result,
        Err(Error::RuntimeError(
            error::RuntimeError::UndefinedVariable(_),
            _,
            _,
            _
        ))
    ); */

    let result = run_code(&mut vm, r#"((lambda  (foo) (set! foo 'bar) foo) #t)"#).unwrap();
    assert_eq!(result, vm.values.symbol("bar"));
}

#[test]
fn test_vm_lambda() {
    let mut vm = VM::default();
    let bool_false = vm.values.bool_false();
    let bool_true = vm.values.bool_true();

    assert_result_eq(&mut vm, "((lambda (x) x) #f)", bool_false);
    assert_result_eq(&mut vm, "((lambda () #t))", bool_true.clone());
    assert_result_eq(&mut vm, "(define id (lambda (x) x)) (id #t)", bool_true);

    let result = run_code(&mut vm, "((lambda (x) #t))");
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
fn test_vm_shadow_bindings() {
    let mut vm = VM::default();

    // the parameter called `if` shadows the core if form
    let result = run_code(&mut vm, "(define (foo if) (if #t 'yes 'no)) (foo 3)");

    assert_matches!(
        result,
        Err(Error::RuntimeError(
            error::RuntimeError::NoncallableError(_),
            _,
            _,
            _
        ))
    )
}

#[test]
fn test_vm_lambda_formals() {
    let mut vm = VM::default();
    let bool_false = vm.values.bool_false();
    let bool_true = vm.values.bool_true();

    assert_result_eq(
        &mut vm,
        "(define test (lambda (x) x)) (test #f)",
        bool_false.clone(),
    );

    let value = run_code(&mut vm, "(define test (lambda x x)) (test)").unwrap();
    assert_eq!(value, vm.values.proper_list(vec![]));

    let value = run_code(&mut vm, "(define test (lambda x x)) (test #f)").unwrap();
    assert_eq!(value, vm.values.proper_list(vec![bool_false.clone()]));

    let value = run_code(&mut vm, "(define test (lambda x x)) (test #f #t)").unwrap();
    assert_eq!(
        value,
        vm.values
            .proper_list(vec![bool_false.clone(), bool_true.clone()]),
    );

    assert_result_eq(
        &mut vm,
        "(define test (lambda (x y z . rest) x)) (test #t #f #f)",
        bool_true.clone(),
    );

    assert_result_eq(
        &mut vm,
        "(define test (lambda (x y z . rest) y)) (test #f #t #f)",
        bool_true.clone(),
    );

    assert_result_eq(
        &mut vm,
        "(define test (lambda (x y z . rest) z)) (test #f #f #t)",
        bool_true.clone(),
    );

    let value = run_code(
        &mut vm,
        "(define test (lambda (x y z . rest) rest)) (test #f #t #t)",
    )
    .unwrap();
    assert_eq!(value, vm.values.proper_list(vec![]));

    let value = run_code(
        &mut vm,
        "(define test (lambda (x y z . rest) rest)) (test #f #f #f #t #t)",
    )
    .unwrap();

    assert_eq!(
        value,
        vm.values
            .proper_list(vec![bool_true.clone(), bool_true.clone()])
    );
}

#[test]
fn test_vm_conditional() {
    let mut vm = VM::default();

    assert_result_eq(&mut vm, "(if #f #t #f)", Value::Bool(false));

    assert_result_eq(&mut vm, "(if #t #t #f)", Value::Bool(true));

    assert_result_eq(&mut vm, "(if #f #t)", Value::Unspecified);

    assert_result_eq(&mut vm, "(if #t #t)", Value::Bool(true));
}

#[test]
fn test_vm_simple_closures() {
    let mut vm = VM::default();
    let result = run_code(
        &mut vm,
        "
               (define test
                 ((lambda (x y) (lambda () (set! x (not x)) x)) #t 'ignored)
               )
               (define ls (lambda x x))
               (ls (test) (test) (test))
",
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
        "
               (define test
                 ((lambda (x y) (lambda () x)) #t 'ignored)
               )
               (test)
",
    )
    .unwrap();
    assert_eq!(result, vm.values.bool_true());

    let result = run_code(
        &mut vm,
        "
               (define test
                  ((lambda (x)
                     ((lambda (proc) (set! x #f) proc)  (lambda () x)))
                   #t))
               (test)
",
    )
    .unwrap();
    assert_eq!(result, vm.values.bool_false());
}

#[test]
fn test_vm_complex_closures() {
    let mut vm = VM::default();
    let result = run_code(
        &mut vm,
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
    let result = run_code(
        &mut vm,
        "
               (define ls (lambda x x))
               (define test (lambda () #t))
               (ls (test) #f (test))
",
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
