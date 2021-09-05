use braces::compiler::source::*;
use braces::compiler::Compiler;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn full_compiler_benchmark(c: &mut Criterion) {
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

    c.bench_function("Compiler#compile", |b| {
        b.iter(|| black_box(compiler.compile(&mut source)))
    });
}

fn frontend_benchmark(c: &mut Criterion) {
    let source = Source::new(
        SourceId::synthetic(),
        r#"
      (define (add x y) (+ x y))
      (define (minus x y) (- x y))
      
      (add 10 20)
      ; this is a comment
      (sub 20 10)
    "#,
    );

    let mut frontend = braces::compiler::frontend::Frontend::new();
    let datum = frontend.read(&source).unwrap();

    c.bench_function("Frontend#pass", |b| {
        b.iter(|| black_box(frontend.pass(&source)))
    });
    c.bench_function("Frontend#read", |b| {
        b.iter(|| black_box(frontend.read(&source)))
    });
    c.bench_function("Frontend#parse", |b| {
        b.iter(|| black_box(frontend.parse(&datum)))
    });
}

criterion_group!(benches, full_compiler_benchmark, frontend_benchmark);
criterion_main!(benches);
