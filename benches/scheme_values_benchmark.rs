use braces::vm::value::list::List;
use braces::vm::value::vector::Vector;
use braces::vm::value::{Factory, Value};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand::Rng;

fn fill_vector(size: usize) -> Vec<Value> {
    let mut rng = rand::thread_rng();
    let values = Factory::default();
    let mut result = Vec::with_capacity(size);
    for i in 0..size {
        match rng.gen_range(1..4) {
            1 => result.push(Value::Bool(i % 2 == 0)),
            2 => result.push(values.string("benchmarks \\o/")),
            3 => result.push(values.real(i)),
            _ => (),
        }
    }
    result
}

fn as_list(v: Value) -> List {
    match v {
        Value::ProperList(ls) => ls,
        _ => panic!("Not a list"),
    }
}

fn list_benchmark(c: &mut Criterion) {
    let values = Factory::default();
    let ls = as_list(values.proper_list(fill_vector(30)));

    c.bench_function("List#cons", |b| {
        b.iter(|| black_box(ls.cons(values.bool_true())))
    });

    let lhs = as_list(values.proper_list(fill_vector(30)));
    let rhs = as_list(values.proper_list(fill_vector(30)));

    c.bench_function("List#append", |b| {
        b.iter(|| black_box(List::append(&lhs, &rhs)))
    });

    let ls = as_list(values.proper_list(fill_vector(30)));
    c.bench_function("List#at", |b| b.iter(|| black_box(ls.at(10))));
    c.bench_function("List#car", |b| b.iter(|| black_box(ls.head())));
}

fn as_vector(v: Value) -> Vector {
    match v {
        Value::Vector(vector) => vector,
        _ => panic!("Not a list"),
    }
}

fn vector_benchmark(c: &mut Criterion) {
    let values = Factory::default();
    let ls = as_vector(values.vector(fill_vector(30)));

    c.bench_function("Vector#cons", |b| {
        b.iter(|| black_box(ls.cons(values.bool_true())))
    });

    let lhs = as_vector(values.vector(fill_vector(30)));
    let rhs = as_vector(values.vector(fill_vector(30)));

    c.bench_function("Vector#append", |b| {
        b.iter(|| black_box(Vector::append(&lhs, &rhs)))
    });

    let ls = as_vector(values.vector(fill_vector(30)));
    c.bench_function("Vector#at", |b| b.iter(|| black_box(ls.at(10))));
}

criterion_group!(benches, list_benchmark, vector_benchmark);
criterion_main!(benches);
