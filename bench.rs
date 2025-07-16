use criterion::*;
use numfmt::*;

fn black_box<T>(x: T) {
    criterion::black_box((x, ()).1)
}

// always does; default, cached, std
macro_rules! repbench {
    ($fn:ident, $val:literal) => {
        fn $fn(c: &mut Criterion) {
            c.bench_function(stringify!(numfmt default $fn), |b| {
                b.iter(|| black_box(Formatter::default().fmt($val)))
            });

            c.bench_function(stringify!(numfmt cached $fn), |b| {
                let mut f = Formatter::default();
                b.iter(|| black_box(f.fmt($val)))
            });

            c.bench_function(stringify!(numfmt cached to string $fn), |b| {
                let mut f = Formatter::default();
                b.iter(|| black_box(f.fmt($val)))
            });

            c.bench_function(stringify!(std $fn), |b| {
                b.iter(|| black_box($val.to_string()))
            });
        }
    };
}

repbench!(zero, 0.0f64);
repbench!(_1234, 0.1234f64);
repbench!(lrgdec, 2.718281828459045f64);
repbench!(sn, 1.797693148623157e307f64);

criterion_group!(benches, zero, _1234, lrgdec, sn);
criterion_main!(benches);
