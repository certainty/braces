use braces::compiler::source::*;
use braces::compiler::Compiler;
use braces::vm::stack::Stack;
use braces::vm::value;
use braces::vm::VM;
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

fn stack_benchmark(c: &mut Criterion) {
    let mut stack: Stack<bool> = Stack::new(100);
    stack.push(true);
    stack.push(false);
    stack.push(false);
    stack.push(true);

    c.bench_function("Stack#pop_push", |b| {
        b.iter(|| {
            stack.push(black_box(false));
            black_box(stack.pop())
        })
    });
    c.bench_function("Stack#at", |b| b.iter(|| black_box(stack.at(3))));
    c.bench_function("Stack#peek", |b| b.iter(|| black_box(stack.peek(2))));
    c.bench_function("Stack#top", |b| b.iter(|| black_box(stack.top())));
}

fn writer_benchmark(c: &mut Criterion) {
    let writer = braces::vm::scheme::writer::Writer::new();
    let mut factory = value::Factory::default();
    let list = factory.proper_list(vec![
        factory.bool_true(),
        value::Value::UninternedString(String::from("test")),
    ]);

    c.bench_function("Writer#write_list", |b| {
        b.iter(|| black_box(writer.write(&list, &factory)))
    });

    let vector_input = factory.vector(vec![value::Value::Bool(true), value::Value::Char('z')]);
    c.bench_function("Writer#write_vector", |b| {
        b.iter(|| black_box(writer.write(&vector_input, &factory)))
    });

    let symbol_input = factory.symbol(" a weird symbol  ");
    c.bench_with_input(
        BenchmarkId::new("Writer#write_symbol", format!("{:?}", symbol_input)),
        &symbol_input,
        |b, i| b.iter(|| black_box(writer.write(i, &factory))),
    );
}

fn vm_benchmark(c: &mut Criterion) {
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

    c.bench_function("VM#interpret", |b| {
        b.iter(|| black_box(vm.interpret(unit.clone())))
    });
}

criterion_group!(benches, vm_benchmark, stack_benchmark, writer_benchmark);
criterion_main!(benches);
