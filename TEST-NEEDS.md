# TEST-NEEDS: standards

## CRG Grade: B/A (Targeting)
To achieve CRG Grades B and above, projects MUST implement **Zigzag Testing** for their critical paths, following the [ZIGZAG-TESTING.md](./ZIGZAG-TESTING.md) methodology.

## CRG Grade: C — ACHIEVED 2026-04-04

All CRG C categories are present and passing. See breakdown below.

| CRG C Category | Status | Count | Details |
|----------------|--------|-------|---------|
| **Unit** | PASS | 100+ | Inline (#[test]) in parser.rs, renderer.rs + integration tests |
| **Smoke** | PASS | 9 | smoke_* tests in a2ml + k9-svc crg_c_tests.rs |
| **P2P (property-based)** | PASS | 15+ | proptest suites in a2ml + k9-svc crg_c_tests.rs |
| **E2E / Reflexive** | PASS | 4 | Dogfood: parse standards manifest + round-trip constructed docs |
| **Contract** | PASS | 13 | Pre/post-condition tests in a2ml + k9-svc crg_c_tests.rs |
| **Aspect** | PASS | 14 | Security (injection, large input, null bytes, unicode) + error-handling |
| **Benchmarks (baselined)** | PASS | 10+ | Criterion: a2ml_bench + k9_bench; Zig: grv6; Deno: manifest |

## Current State

| Category | Count | Details |
|----------|-------|---------|
| **Source modules** | 358+ | Massive monorepo: 0-ai-gatekeeper-protocol (mcp-repo-guardian, repo-guardian-fs), a2ml, axel-protocol, groove-protocol, contractiles, and many more sub-projects |
| **Unit tests** | 158+ | Real tests across 6 test suites (see breakdown below) |
| **P2P (property) tests** | 15+ | proptest in a2ml + k9-svc integration test files |
| **Contract tests** | 13 | Pre/post-condition tests in a2ml + k9-svc |
| **Aspect tests** | 14 | Security + error-handling cross-cutting tests |
| **E2E tests** | 4 | Dogfood: parse real manifests + round-trip stability |
| **Benchmarks** | 22+ | Criterion (a2ml + k9-svc) + Zig (grv6) + Deno (manifest) |
| **Fuzz tests** | 0 | Placeholder removed; real fuzz TODO |

## Test Suite Breakdown (as of 2026-04-04)

### groove-protocol/reference/ipv6t — 10 tests (Zig)

Run: `zig build test` from `groove-protocol/reference/ipv6t/`

All 10 tests pass. Cover all 5 spec validation scenarios + 5 property tests:
- [x] Positive: correct type hash accepted
- [x] Negative: wrong type hash rejected before payload parsing
- [x] Provenance: 3 chained frames produce verifiable hash chain
- [x] Fallback: raw bytes without magic treated as untyped
- [x] Trust flag: PROVEN flag does not bypass type hash validation
- [x] Header size is exactly 108 bytes
- [x] Hash determinism: same input always same hash
- [x] Multiple type acceptance: reader accepts any of N expected types
- [x] Trust level correctly derived from flags
- [x] Hash hex formatting is correct

**Benchmarks**: `zig build bench` — measures hash computation throughput:
- SHA-256 type hash: ~4.3µs/iter
- SHA-256 cap hash: ~3.2µs/iter
- Provenance chain step (2x SHA-256): ~10.2µs/iter
- Hash hex format: ~57ns/iter (17.5 M/s)

### mcp-repo-guardian — 36 tests (Deno/JS)

Run: `deno task test` from `0-ai-gatekeeper-protocol/mcp-repo-guardian/`

All 36 tests pass. Tests cover:
- [x] Manifest parsing (hash, canonical locations, invariants)
- [x] Determinism and hash correctness
- [x] Session management (create, acknowledge, multi-session isolation)
- [x] Access guard (denied before ack, allowed after ack, invalid session)
- [x] Path validation / invariant enforcement (all 7 SCM file variants)
- [x] **Security aspect**: path traversal in canonical location → safe default
- [x] **Security aspect**: XSS/injection in manifest → stored as plain text, not executed
- [x] **Security aspect**: 1MB manifest does not error (no catastrophic regex)
- [x] **Security aspect**: null bytes in manifest handled
- [x] **E2E dogfood**: parses standards repo's own `0-AI-MANIFEST.a2ml`

**Benchmarks**: `deno task bench`
- SHA-256 hash (manifest ~400 bytes): ~4µs/iter
- Full manifest build: ~5.6µs/iter (hash + 5 regex + date)
- Session lifecycle: ~7.7µs/iter

### axel-protocol — 14 tests (Deno/TS)

Run: `deno task test` from `axel-protocol/`

All 14 tests pass. Tests cover:
- [x] Valid AXEL1 DNS TXT record parsing
- [x] Extra whitespace handling
- [x] Unknown keys ignored
- [x] id with equals signs (base64)
- [x] Reject: missing version, empty payload, whitespace-only
- [x] Reject: wrong version (AXEL2)
- [x] Reject: version without value
- [x] Reject: missing id, empty id, whitespace-only id
- [x] Reject: full DNS RR line (not just RDATA)
- [x] Reject: no default version when missing

### repo-guardian-fs/tests-offline — 29 tests (Rust)

Run: `cargo test` from `0-ai-gatekeeper-protocol/repo-guardian-fs/tests-offline/`

All 29 tests pass. **NOTE**: the main `repo-guardian-fs` crate cannot build because
`fuse3 v0.7.3` is incompatible with Rust stable >= 1.80. The offline test crate
isolates the manifest and session logic (no fuse3 dependency).

Tests cover:
- [x] Manifest hash computation (deterministic, 64 hex chars)
- [x] Canonical location extraction (scm_files, bot_directives)
- [x] Defaults when canonical location not found
- [x] Invariant extraction from CORE INVARIANTS section
- [x] Default invariants when section absent
- [x] File I/O: parse from file, error on missing file
- [x] `find_and_parse_manifest` prefers `0-AI-MANIFEST.a2ml` over `AI.a2ml`
- [x] `find_and_parse_manifest` errors when no manifest exists
- [x] **Security**: path traversal rejected → safe default used
- [x] **Security**: 1MB manifest does not panic
- [x] **Security**: null bytes in manifest do not panic
- [x] **Security**: 0-AI-MANIFEST.a2ml preferred over AI.a2ml (cannot be spoofed)
- [x] **E2E dogfood**: parses standards repo's own manifest
- [x] Session: new session is unacknowledged
- [x] Session: acknowledgment with correct hash succeeds
- [x] Session: acknowledgment with wrong hash fails
- [x] Session: unknown session ID returns error
- [x] Session: expired session is unacknowledged
- [x] Session: multiple independent sessions
- [x] Session: active count tracking
- [x] Session: cleanup_expired removes expired sessions
- [x] Session: idempotent get_or_create

### a2ml/bindings/rust — 47 tests (Rust)

Run: `cargo test` from `a2ml/bindings/rust/`

All 47 tests pass (11 inline unit tests + 36 CRG C integration tests).

CRG C integration tests (`tests/crg_c_tests.rs`):
- [x] **Smoke**: version directive roundtrip, empty document, TrustLevel display (3 tests)
- [x] **Unit**: heading levels 1-6, attestation all fields, ordered/unordered lists, inline emphasis/strong/code, Manifest extraction, Directive::new (10 tests)
- [x] **P2P**: TrustLevel from_str canonical + unknown, ordering, display roundtrip; Directive stores verbatim; Attestation stores verbatim; paragraph roundtrip block count (6 proptest functions)
- [x] **Contract**: parse("") -> empty doc, render always UTF-8, unclosed code block is Err, total order, Document::default equals new, Manifest version None/Some (7 tests)
- [x] **Aspect/Security**: no script injection in directive, 1MB no panic, null bytes no panic, very long directive name no stackoverflow, unicode content, deep blockquote no stackoverflow (6 tests)
- [x] **Aspect/Error**: A2mlError::diagnostic non-empty, RenderError diagnostic (2 tests)
- [x] **E2E/Reflexive**: parse standards repo 0-AI-MANIFEST.a2ml, parse .machine_readable/6a2/STATE.a2ml (2 tests)

**Benchmarks**: `cargo bench` from `a2ml/bindings/rust/`
- Parse small/medium/large_manifest throughput
- Render medium throughput
- Round-trip (parse+render) for small/medium/large
- TrustLevel comparison micro-benchmark

### k9-svc/bindings/rust — 45 tests (Rust)

Run: `cargo test` from `k9-svc/bindings/rust/`

All 45 tests pass (9 inline unit tests + 3 doc tests + 33 CRG C integration tests).

CRG C integration tests (`tests/crg_c_tests.rs`):
- [x] **Smoke**: minimal parse, SecurityLevel display, render minimal (3 tests)
- [x] **Unit**: SecurityLevel ordering, component with description/recipe/contract, multiple contracts, pedigree with license, Component::new minimal, Recipe::new, Contract::new default severity (9 tests)
- [x] **P2P**: SecurityLevel from_str canonical + unknown, display roundtrip; Component stores verbatim; Pedigree stores verbatim; Contract stores verbatim (6 proptest functions)
- [x] **Contract**: parse("") -> empty vec, render([]) empty, render always UTF-8, Nickel format rejected, SecurityLevel total order, missing pedigree Err, unknown security level Err (7 tests)
- [x] **Aspect/Security**: no shell injection in origin, 1MB no panic, null bytes no panic, unicode no panic (4 tests)
- [x] **Aspect/Error**: K9Error diagnostic non-empty, NickelFormat error message (2 tests)
- [x] **E2E/Reflexive**: parse .k9 fixtures from k9-svc dir, round-trip constructed component (2 tests)

**Benchmarks**: `cargo bench` from `k9-svc/bindings/rust/`
- Parse small/medium/multi-component throughput
- Render medium throughput
- Round-trip (parse+render) for small/medium/multi
- SecurityLevel::from_str micro-benchmark

## What Was Fixed in This Session (2026-04-04, session 2)

- [x] Added `proptest` + `criterion` dev-dependencies to `a2ml/bindings/rust/Cargo.toml`
- [x] Added `proptest` + `criterion` dev-dependencies to `k9-svc/bindings/rust/Cargo.toml`
- [x] Created `a2ml/bindings/rust/tests/crg_c_tests.rs` — 36 tests covering all CRG C categories
- [x] Created `a2ml/bindings/rust/benches/a2ml_bench.rs` — Criterion benchmarks (6 bench functions)
- [x] Created `k9-svc/bindings/rust/tests/crg_c_tests.rs` — 33 tests covering all CRG C categories
- [x] Created `k9-svc/bindings/rust/benches/k9_bench.rs` — Criterion benchmarks (6 bench functions)
- [x] All 158+ tests pass across all 6 test suites

## What Was Fixed in Previous Session (2026-04-04, session 1)

- [x] Removed `tests/fuzz/placeholder.txt` (fake fuzz claim)
- [x] Created 36 real tests for `mcp-repo-guardian` (replacing 0 tests)
- [x] Created 29 real tests for `repo-guardian-fs` logic (bypassing broken fuse3)
- [x] Confirmed groove-protocol grv6 10 tests already existed and pass
- [x] Confirmed axel-protocol 14 tests already existed and pass
- [x] Created grv6 benchmarks (Zig — `zig build bench`)
- [x] Created manifest parsing benchmarks (Deno — `deno task bench`)
- [x] Added `test` and `bench` tasks to `deno.json` files

## What's Still Missing (TODO for v0.3.0)

### BLOCKERS
- [ ] **fuse3 v0.7.3 incompatible with Rust stable >= 1.80** — `repo-guardian-fs` cannot build.
  Fix: upgrade to `fuse3 v0.9.0` (breaking API changes) or replace with `fuser` crate.

### Tests Still Needed

#### LIVE ENVIRONMENT (cannot run in CI without setup)
- [ ] `repo-guardian-fs` FUSE mount/unmount lifecycle
- [ ] `repo-guardian-fs` access control via FUSE operations (open, read, readdir)
- [ ] `mcp-repo-guardian` MCP server roundtrip (requires running MCP server)
- [ ] `repo-guardian-fs` concurrent FUSE access (requires FUSE + threads)

#### Property Tests (infrastructure needed)
- [ ] A2ML parser roundtrip: arbitrary valid A2ML → parse → pretty-print → parse matches
- [ ] Manifest hash property: hash(content) always 64 hex chars, never the same for different inputs
- [ ] Groove grv6 property: for any payload and type, type hash mismatch always rejected

#### Missing Sub-projects (358 source files with ~0 tests)
- [ ] `avow-protocol` — no tests (ReScript source exists)
- [ ] `contractiles` — no tests
- [ ] `consent-aware-http` — no tests
- [ ] `lol` — no tests
- [ ] `overlay-protocol` — no tests
- [ ] `k9-svc` — no tests (mirrors a2ml structure, same test pattern applies)
- [ ] All Zig `integration_test.zig` files in sub-projects — all are templates with `{{project}}` placeholders

#### Idris2
- [ ] Idris2 compilation verification for `a2ml/src/A2ML/` (Tests.idr exists but cannot verify without idris2 binary)
- [ ] Idris2 proof verification for `a2ml/src/A2ML/Proofs.idr`

### Benchmark Gaps
- [ ] Network-level grv6 benchmark (frame roundtrip throughput over loopback) — requires live env
- [ ] A2ML document parser throughput (Idris2 parser bench) — requires idris2 binary
- [ ] FUSE filesystem overhead vs native filesystem — requires live env

## Priority: P0 (CRITICAL)
