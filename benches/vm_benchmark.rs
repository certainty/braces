use braces::compiler::source::*;
use braces::compiler::Compiler;
use braces::vm::VM;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn vm_fibonacci_benchmark(c: &mut Criterion) {
    let mut source = StringSource::new(
        r#"
        (define (fib-tc n)
        (fib-iter 1 0 n))

        (define (fib-iter a b count)
            (if (= count 0)
                b
              (fib-iter (+ a b) a (- count 1))))

        (define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

        (fib 15)
    "#,
    );
    let mut vm = VM::default();
    let mut compiler = Compiler::new();
    let unit = compiler.compile(&mut source).unwrap();

    c.bench_function("interpret_fibonacci", |b| {
        b.iter(|| vm.interpret(unit.clone()))
    });
}

criterion_group!(benches, vm_fibonacci_benchmark);
criterion_main!(benches);
