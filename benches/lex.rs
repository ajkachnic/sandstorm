use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sandstorm::{cache, lex};

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("lex sample", |b| {
        b.iter(|| {
            let mut file_cache = cache::FileCache::new();
            lex::Lexer::from_file(&mut file_cache, "samples/bf.storm")
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
