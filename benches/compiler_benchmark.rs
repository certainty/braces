use braces::compiler::source::*;
use braces::compiler::Compiler;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn compiler_benchmark(c: &mut Criterion) {
    let mut compiler = Compiler::new();
    let mut source = StringSource::new(
        r#"
      (define (add x y) (+ x y))
      (define (minus x y) (- x y))
      
      (add 10 20)
      ; this is a comment
      (sub 20 10)
    "#,
    );

    c.bench_function("compile_expression", |b| {
        b.iter(|| compiler.compile(black_box(&mut source)))
    });
}

criterion_group!(benches, compiler_benchmark);
criterion_main!(benches);
