// SPDX-License-Identifier: MPL-2.0
// (MPL-2.0 required for crates.io)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Criterion benchmarks for the a2ml crate.
//
// Baseline categories covered:
//   - Small document parse throughput
//   - Medium document parse throughput
//   - Large/realistic manifest parse throughput
//   - Render throughput (document -> text)
//   - Round-trip (parse then render) throughput
//
// Run with: cargo bench (from a2ml/bindings/rust/)

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};

use a2ml::parser::parse;
use a2ml::renderer::render;
use a2ml::types::{Attestation, Block, Directive, Document, Inline, TrustLevel};

// ---------------------------------------------------------------------------
// Fixture strings
// ---------------------------------------------------------------------------

/// Minimal single-heading document (~25 bytes).
const SMALL_DOC: &str = "# Hello\n\nA paragraph.";

/// Medium document with directives, headings, code blocks, and paragraphs.
const MEDIUM_DOC: &str = r##"@version 1.0
@require trust-level:reviewed

# Introduction

This document describes the A2ML format.

## Syntax

A2ML is a lightweight markup language.

```rust
use a2ml::parser::parse;
let doc = parse(input).unwrap();
```

## Directives

Directives begin with `@` and carry machine-readable metadata.

!attest identity=Alice role=author trust=reviewed

---

## Conclusion

A2ML supports provenance tracking.
"##;

/// Large realistic AI manifest document (~800 bytes).
const LARGE_MANIFEST: &str = r#"@version 2.0
@schema https://a2ml.hyperpolymath.dev/schema/manifest/v2

# 0-AI-MANIFEST

Canonical entry point for all AI agents.

## Canonical File Locations

- SCM files: `.machine_readable/`
- Bot directives: `.bot_directives/`
- Agent instructions: `.claude/CLAUDE.md`

## Critical Invariants

@invariant scm-files-location: SCM files MUST be in .machine_readable/ only
@invariant no-root-scm: Never create STATE.a2ml in the repository root
@invariant license-pmpl: All original code uses PMPL-1.0-or-later

## On-Enter Hooks

1. Read this manifest
2. Acknowledge canonical locations
3. Read .machine_readable/STATE.a2ml

## On-Exit Hooks

1. Update .machine_readable/STATE.a2ml with session summary
2. Commit changes

!attest identity=hypatia role=security-scanner trust=automated timestamp=2026-04-04
!attest identity=Alice role=reviewer trust=reviewed timestamp=2026-04-04

---

@hash sha256:abc123def456
"#;

// ---------------------------------------------------------------------------
// Parse benchmarks
// ---------------------------------------------------------------------------

/// Benchmark: parse small document.
fn bench_parse_small(c: &mut Criterion) {
    let input = black_box(SMALL_DOC);
    let mut group = c.benchmark_group("parse");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("small", |b| {
        b.iter(|| {
            let doc = parse(black_box(input)).expect("parse should not fail");
            black_box(doc)
        });
    });
    group.finish();
}

/// Benchmark: parse medium document.
fn bench_parse_medium(c: &mut Criterion) {
    let input = black_box(MEDIUM_DOC);
    let mut group = c.benchmark_group("parse");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("medium", |b| {
        b.iter(|| {
            let doc = parse(black_box(input)).expect("parse should not fail");
            black_box(doc)
        });
    });
    group.finish();
}

/// Benchmark: parse large manifest document.
fn bench_parse_large(c: &mut Criterion) {
    let input = black_box(LARGE_MANIFEST);
    let mut group = c.benchmark_group("parse");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("large_manifest", |b| {
        b.iter(|| {
            let doc = parse(black_box(input)).expect("parse should not fail");
            black_box(doc)
        });
    });
    group.finish();
}

// ---------------------------------------------------------------------------
// Render benchmarks
// ---------------------------------------------------------------------------

/// Benchmark: render a pre-built medium document.
fn bench_render_medium(c: &mut Criterion) {
    // Build a document once; benchmark only the render step.
    let doc = parse(MEDIUM_DOC).expect("setup: parse must succeed");
    let mut group = c.benchmark_group("render");
    group.bench_function("medium", |b| {
        b.iter(|| {
            let text = render(black_box(&doc)).expect("render should not fail");
            black_box(text)
        });
    });
    group.finish();
}

// ---------------------------------------------------------------------------
// Round-trip benchmark
// ---------------------------------------------------------------------------

/// Benchmark: full parse-then-render round-trip.
fn bench_roundtrip(c: &mut Criterion) {
    let inputs = [
        ("small", SMALL_DOC),
        ("medium", MEDIUM_DOC),
        ("large", LARGE_MANIFEST),
    ];

    let mut group = c.benchmark_group("roundtrip");
    for (name, input) in &inputs {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::new("parse_render", name), input, |b, src| {
            b.iter(|| {
                let doc = parse(black_box(src)).expect("parse");
                let text = render(black_box(&doc)).expect("render");
                black_box(text)
            });
        });
    }
    group.finish();
}

// ---------------------------------------------------------------------------
// TrustLevel ordering benchmark (micro)
// ---------------------------------------------------------------------------

/// Benchmark: TrustLevel comparison (used in attestation filtering hot paths).
fn bench_trust_level_cmp(c: &mut Criterion) {
    use a2ml::types::TrustLevel::*;
    let levels = [Unverified, Automated, Reviewed, Verified];
    c.bench_function("trust_level/cmp_all_pairs", |b| {
        b.iter(|| {
            let mut count = 0u32;
            for a in &levels {
                for b in &levels {
                    if black_box(a) >= black_box(b) {
                        count += 1;
                    }
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
    bench_parse_large,
    bench_render_medium,
    bench_roundtrip,
    bench_trust_level_cmp,
);
criterion_main!(benches);
