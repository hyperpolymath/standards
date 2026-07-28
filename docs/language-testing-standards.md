<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# Language Testing Standards

**Version:** 2.0.0
**Date:** 2026-07-03
**Status:** Active (supersedes v1.0.0, 2024-04-14)

This document establishes the estate's canonical, **conformance-graded** testing
standards for every programming language in the CCCP language policy. Keywords
**MUST / SHOULD / MAY** are RFC-2119.

It sits above the per-language guides: this document says *what every language's
testing story MUST provide*; each per-language guide (built from
`templates/language-testing-guide-TEMPLATE.md`) says *which concrete tools
provide it*. The requirement categories align with the CRG test taxonomy in
`testing-and-benchmarking/TESTING-TAXONOMY.adoc`, so a language's testing
maturity maps onto its Component/Toolchain Readiness Grade.

## Conformance requirements (every approved language)

A language's testing story is **conformant** when its per-language guide names a
concrete, CI-runnable tool for each MUST row, and records `none` *visibly* where
it genuinely cannot (never a silent gap).

| # | Requirement | Level | CRG category |
|---|---|---|---|
| R1 | A **unit test** runner MUST exist and run in CI on every push/PR. | MUST | Unit tests |
| R2 | A **formatter** MUST exist and be checkable in CI (fail on unformatted). | MUST | hygiene |
| R3 | A **linter / static analyser** MUST exist and run in CI. | MUST | Aspect (correctness) |
| R4 | A **coverage** tool SHOULD run in CI and report a number. | SHOULD | Unit tests |
| R5 | A **property-based / fuzz** facility SHOULD exist for parsers and pure logic. | SHOULD | Property-based (P2P) |
| R6 | A **benchmark** facility SHOULD exist; regressions SHOULD gate for perf-critical code. | SHOULD | Benchmarks |
| R7 | A **security / dependency audit** MUST run for languages with a package ecosystem. | MUST | Aspect (security) |
| R8 | **Contract / pre-post** checks MAY be expressed where the language supports them. | MAY | Contract |
| R9 | For formally-verifiable languages, **proofs** MUST be checked in CI (no hollow proof claims). | MUST\* | proof gate |

`MUST*` = applies only to languages whose role includes formal verification
(Idris2, Agda, Rust/SPARK). The `spark-theatre-gate.yml` workflow already
enforces "no hollow SPARK proof claims"; R9 generalises that stance.

**Anti-theatre rule (all requirements):** a testing job that cannot fail is not
a test. A MUST check MUST NOT sit behind `continue-on-error` without a
documented, blocking equivalent elsewhere. Coverage numbers MUST be *reported
with an artifact*, never merely asserted. (See the Wave-0/1 false-green
remediation.)

## Per-language guides (required set)

Each approved language SHOULD publish a guide from
`templates/language-testing-guide-TEMPLATE.md`. Priority tracks estate centrality:

| Language | Guide | Status |
|---|---|---|
| Rust/SPARK | this document §Rust + SPARK proof gate | present |
| Julia | `julia-testing-tools-guide.md` | present (v1.0.0 — R1–R9 refresh tracked) |
| **AffineScript** | `affinescript-testing-guide.md` | **present** — canonical SSOT migrates to `hyperpolymath/affinescript` prospectively |
| Zig | — | charter |
| Elixir + Gleam (BEAM) | — | charter |
| Idris2 / Agda (proofs) | — | charter (ties to proof-debt epic #124) |

New guides MUST pass `scripts/check-language-guide.sh` (wired into `just
validate`), which fails if a guide omits a required section.

## Rust/SPARK

| Requirement | Tool | CI invocation |
|---|---|---|
| R1 unit | `cargo test` | `cargo test --all` |
| R2 format | `rustfmt` | `cargo fmt --all -- --check` |
| R3 lint | `clippy` | `cargo clippy --all-targets --all-features -- -D warnings` |
| R4 coverage | `cargo tarpaulin` / `cargo llvm-cov` | reports % in CI |
| R5 property | `proptest` / `quickcheck` | in the test suite |
| R6 bench | `criterion` | `cargo bench` |
| R7 audit | `cargo audit` | weekly minimum |
| R9 proof | Rust/SPARK | `spark-theatre-gate.yml` (no hollow proof claims) |

Reusable workflow: `rust-ci-reusable.yml`. Warnings are errors (`-D warnings`);
coverage SHOULD be ≥ 80%.

## Julia

Concrete tools live in `julia-testing-tools-guide.md`. Requirement mapping:

| Requirement | Tool |
|---|---|
| R1 unit | `Pkg.test()` |
| R2 format | `JuliaFormatter.format("."; overwrite=false)` |
| R3 lint | `JET.test_package(".")` |
| R4 coverage | `Coverage.jl` |
| R6 bench | `BenchmarkTools.@benchmark` |
| R7 audit | `Aqua.test_all(deps=true)` (ambiguities, deps-compat, project-extras) |

> The Julia guide is v1.0.0 (2024) and predates this RFC-2119 framing; refreshing
> it to the R1–R9 mapping (and flipping its "not yet integrated" rows) is tracked
> estate work.

## AffineScript

The estate's primary application language. See `affinescript-testing-guide.md`
for the full guide; requirement mapping summarised there. Canonical SSOT will
move to `hyperpolymath/affinescript` prospectively (charter) — until then this
repo carries the guide.

## Version control & CI hygiene (all languages)

- All CI actions MUST be SHA-pinned (governance workflow-lint +
  `hooks/validate-sha-pins.sh`).
- Commit messages SHOULD follow Conventional Commits; SemVer 2.0.0 for releases.
- Pre-commit/pre-push hooks are installed via `just hooks-install`.

## Resources

- `testing-and-benchmarking/TESTING-TAXONOMY.adoc` — the CRG test taxonomy.
- `templates/language-testing-guide-TEMPLATE.md` — the per-language skeleton.
- `component-readiness-grades/` · `toolchain-readiness-grades/` — testing → grade.

## Changelog

- **2.0.0 (2026-07-03)**: RFC-2119 conformance requirements (R1–R9) mapped to the
  CRG taxonomy; per-language guide template + required set; AffineScript added;
  anti-theatre rule; removed the stale 2024 roadmap/duplicate snapshot.
- **1.0.0 (2024-04-14)**: Initial release (Rust + Julia).
