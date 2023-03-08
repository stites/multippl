use criterion::{criterion_group, criterion_main, Criterion, SamplingMode};
use yodel::grids::*;
use yodel::inference::*;
use yodel::*;

macro_rules! grid_benchmarks {
    (sizes: $($size:expr, )*) => {
        pub fn all_benchmarks(c: &mut Criterion) {
            $(
                let mk_probability = |_ix, _p| Probability::new(0.5);
                let schema = GridSchema::new_from_fn($size, false, None, &mk_probability);
                let grid = make::grid(schema);
                let approx = importance_weighting;
                let mut group = c.benchmark_group(format!("grid-{}x{}", $size, $size));
                group.sampling_mode(SamplingMode::Flat);
                group.sample_size(10);
                group.measurement_time(std::time::Duration::new($size * $size * 100, 0));

                group.bench_function("exact", |b| b.iter(|| exact(&grid)));
                group.bench_function("approx/1", |b| b.iter(|| approx(1, &grid)));
                // group.bench_function("approx/10", |b| b.iter(|| approx(10, &grid)));
                // group.bench_function("approx/100", |b| b.iter(|| approx(100, &grid)));
                group.finish();
            )*
        }
    }
}

grid_benchmarks! {
  size: 2, 3, 4, 5, 10,
}

criterion_group!(benches, all_benchmarks);
criterion_main!(benches);
