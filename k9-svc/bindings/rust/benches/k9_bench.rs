// SPDX-License-Identifier: MPL-2.0
// (MPL-2.0 required for crates.io)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Criterion benchmarks for the k9-svc crate.
//
// Baseline categories covered:
//   - Small K9 document parse throughput (single component)
//   - Medium K9 document parse throughput (multi-component with contracts)
//   - Render throughput (components -> text)
//   - Round-trip (parse then render) throughput
//
// Run with: cargo bench (from k9-svc/bindings/rust/)

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};

use k9_svc::parser::parse;
use k9_svc::renderer::render;
use k9_svc::types::{Component, Contract, Pedigree, Recipe, SecurityLevel};

// ---------------------------------------------------------------------------
// Fixture strings
// ---------------------------------------------------------------------------

/// Minimal single-component K9 document.
const SMALL_K9: &str = r#"component: my-svc
  version: 0.1.0
  pedigree:
    origin: https://github.com/example/svc
    author: Alice
  security: kennel
"#;

/// Medium K9 document with recipe, contract, and metadata.
const MEDIUM_K9: &str = r#"component: parser-lib
  version: 1.2.3
  description: A robust parsing library
  pedigree:
    origin: https://github.com/example/parser
    author: Bob
    license: MPL-2.0
  security: yard
  recipe:
    tool: cargo
    command: cargo build --release
  contract: no-unsafe
    description: Forbid unsafe blocks in this crate
    check: cargo clippy -- -D unsafe-code
    severity: error
  contract: no-panic
    description: No unwrap/expect calls in library code
    check: cargo clippy -- -D clippy::unwrap_used
    severity: warning
  metadata:
    codeql: true
    scorecard: passed
"#;

/// Multi-component K9 document (3 components).
const MULTI_K9: &str = r#"component: frontend
  version: 0.5.0
  pedigree:
    origin: https://github.com/example/frontend
    author: Carol
  security: hunt

component: backend
  version: 1.0.0
  pedigree:
    origin: https://github.com/example/backend
    author: Dave
    license: PMPL-1.0-or-later
  security: yard
  recipe:
    tool: cargo
    command: cargo build --release

component: database
  version: 2.1.0
  pedigree:
    origin: https://github.com/example/db
    author: Eve
  security: kennel
"#;

// ---------------------------------------------------------------------------
// Parse benchmarks
// ---------------------------------------------------------------------------

/// Benchmark: parse small K9 document.
fn bench_parse_small(c: &mut Criterion) {
    let input = black_box(SMALL_K9);
    let mut group = c.benchmark_group("k9_parse");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("small", |b| {
        b.iter(|| {
            let components = parse(black_box(input)).expect("parse should not fail");
            black_box(components)
        });
    });
    group.finish();
}

/// Benchmark: parse medium K9 document (with recipe + contracts).
fn bench_parse_medium(c: &mut Criterion) {
    let input = black_box(MEDIUM_K9);
    let mut group = c.benchmark_group("k9_parse");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("medium", |b| {
        b.iter(|| {
            let components = parse(black_box(input)).expect("parse should not fail");
            black_box(components)
        });
    });
    group.finish();
}

/// Benchmark: parse multi-component K9 document.
fn bench_parse_multi(c: &mut Criterion) {
    let input = black_box(MULTI_K9);
    let mut group = c.benchmark_group("k9_parse");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("multi_component", |b| {
        b.iter(|| {
            let components = parse(black_box(input)).expect("parse should not fail");
            black_box(components)
        });
    });
    group.finish();
}

// ---------------------------------------------------------------------------
// Render benchmarks
// ---------------------------------------------------------------------------

/// Benchmark: render a pre-built medium document.
fn bench_render_medium(c: &mut Criterion) {
    let components = parse(MEDIUM_K9).expect("setup: parse must succeed");
    let mut group = c.benchmark_group("k9_render");
    group.bench_function("medium", |b| {
        b.iter(|| {
            let text = render(black_box(&components)).expect("render should not fail");
            black_box(text)
        });
    });
    group.finish();
}

// ---------------------------------------------------------------------------
// Round-trip benchmarks
// ---------------------------------------------------------------------------

/// Benchmark: full parse-then-render round-trip.
fn bench_roundtrip(c: &mut Criterion) {
    let inputs = [
        ("small", SMALL_K9),
        ("medium", MEDIUM_K9),
        ("multi", MULTI_K9),
    ];

    let mut group = c.benchmark_group("k9_roundtrip");
    for (name, input) in &inputs {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::new("parse_render", name), input, |b, src| {
            b.iter(|| {
                let components = parse(black_box(src)).expect("parse");
                let text = render(black_box(&components)).expect("render");
                black_box(text)
            });
        });
    }
    group.finish();
}

// ---------------------------------------------------------------------------
// SecurityLevel micro benchmark
// ---------------------------------------------------------------------------

/// Benchmark: SecurityLevel::from_str (used in hot parse path).
fn bench_security_level_from_str(c: &mut Criterion) {
    let names = ["kennel", "yard", "hunt", "KENNEL", "Yard", "HUNT"];
    c.bench_function("security_level/from_str", |b| {
        b.iter(|| {
            let mut count = 0u32;
            for name in &names {
                if SecurityLevel::from_str(black_box(name)).is_some() {
                    count += 1;
                }
            }
            black_box(count)
        });
    });
}

criterion_group!(
    benches,
    bench_parse_small,
    bench_parse_medium,
    bench_parse_multi,
    bench_render_medium,
    bench_roundtrip,
    bench_security_level_from_str,
);
criterion_main!(benches);
