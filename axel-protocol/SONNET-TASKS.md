# AXEL Protocol — Compiler Strategy & Compilation Targets

## Compilation Pipeline (3 Stages)

### Stage 1 — Core Logic (ReScript → JavaScript)

Already partially in place. ReScript gives type-safe parsing (DNS TXT, policy JSON) that compiles to clean ESM. This remains the **rapid development** layer — iterate on protocol semantics here first, then promote hot paths to Rust.

### Stage 2 — Performance & Portability (Rust → WASM + Native)

The real workhorse. Rust gives a single codebase that cross-compiles to every target that matters. Policy validation, schema enforcement, DNS caching logic, and the enforcement daemon all belong here.

### Stage 3 — Formal Verification (Idris2 ABI + Zig FFI)

Per hyperpolymath standard. Idris2 proves the protocol invariants (e.g., "L0 policies MUST NOT appear in AXEL-N gateway rules", "expired cache entries fail closed"). Zig FFI provides C-compatible bindings for integration into existing infrastructure (nginx modules, firewalld plugins, DNS resolver hooks).

## Compilation Targets (Priority Order)

| Priority | Target | Rationale |
|----------|--------|-----------|
| **1** | **WASM (wasm32-wasi)** | Universal. Runs on Cloudflare Workers, Fastly Compute@Edge, Vercel Edge, Deno Deploy, browsers, wasmtime/wasmer. One binary, every CDN. Primary enforcement deployment target. |
| **2** | **JavaScript (ESM)** | Already working via ReScript. Widest ecosystem reach for npm/deno distribution. Developer-facing SDK and rapid prototyping layer. Fallback for environments without WASM. |
| **3** | **Native x86_64-linux** | CLI validator (`axel-validate`), network gateway daemon, CI/CD integration. Rust `cargo build --release`. Infrastructure target — ISPs and firewall operators won't deploy WASM. |
| **4** | **Native aarch64-linux** | ARM servers (AWS Graviton, Ampere). Second native target. Trivial with Rust cross-compilation. |
| **5** | **Cloudflare Workers (wasm32-unknown-unknown)** | Specialized WASM variant with Workers KV/D1 bindings. Most likely first real CDN deployment. Worth having as a named target with platform-specific glue. |
| **6** | **WASM Component Model (wasm32-wasip2)** | Future-facing. WASI Preview 2 + Component Model gives composable, sandboxed modules. Not production-ready today but this is where the ecosystem is going. |

## Architecture

```
┌─────────────────────────────────────────────┐
│  Idris2 ABI (formal proofs)                 │
│  - Protocol invariants                       │
│  - Policy object type safety                 │
│  - Isolation level enforcement rules         │
└──────────────┬──────────────────────────────┘
               │ generates C headers
┌──────────────▼──────────────────────────────┐
│  Rust core library (axel-core)              │
│  - Policy parsing + validation              │
│  - DNS TXT record parser                    │
│  - Schema enforcement (no Ajv dependency)   │
│  - Cache logic (max_age, stale_if_error)    │
│  - Enforcement decisions (303/403 routing)  │
├─────────────────────────────────────────────┤
│  Compile targets:                           │
│  ├── wasm32-wasi     → CDN edge workers     │
│  ├── wasm32-unknown  → browsers, CF Workers │
│  ├── x86_64-linux    → CLI, daemon, CI      │
│  └── aarch64-linux   → ARM servers          │
└──────────────┬──────────────────────────────┘
               │ wasm-bindgen / wasm-pack
┌──────────────▼──────────────────────────────┐
│  ReScript SDK (axel-sdk)                    │
│  - Developer-facing API (imports WASM)      │
│  - Policy builder / validator               │
│  - Deno-native tooling                      │
│  - Falls back to pure JS if no WASM         │
└─────────────────────────────────────────────┘
               │
┌──────────────▼──────────────────────────────┐
│  Zig FFI (platform integration)             │
│  - nginx module (axel-nginx)                │
│  - firewalld plugin                         │
│  - DNS resolver hooks (knot, unbound)       │
│  - Zero runtime dependencies                │
└─────────────────────────────────────────────┘
```

## Why WASM First

CDN edge is where AXEL enforcement actually happens at scale. A Cloudflare Worker running the WASM policy validator at 300+ edge locations is orders of magnitude more impactful than a native binary running on one origin server. The CDN intercepts the request, checks the `_axel` DNS record (cached), fetches the policy (cached), and gates with 303/403 — all before the origin even sees traffic. WASM makes that a single deployable artifact across every edge platform.

## Implementation Order

### Task 1: Rust crate `axel-core`

Policy parser, schema validator, DNS TXT parser, enforcement decision engine. No external dependencies for the core. Replaces Ajv and the `--no-check` workaround.

- `axel-core/src/lib.rs` — public API
- `axel-core/src/policy.rs` — AXEL Policy Object parsing + validation
- `axel-core/src/dns.rs` — DNS TXT record parser (strict, fail-closed)
- `axel-core/src/enforce.rs` — enforcement decision engine (303/403 routing)
- `axel-core/src/cache.rs` — cache logic (max_age_seconds, stale_if_error_seconds)
- `axel-core/src/schema.rs` — built-in schema validation (no Ajv)

### Task 2: WASM bundle

`wasm-pack build --target web` — browser + edge WASM bundle from the same crate.

- `axel-core/Cargo.toml` with `crate-type = ["cdylib", "rlib"]`
- `wasm-bindgen` exports for policy validation, DNS parsing, enforcement decisions
- Bundle size target: <100KB gzipped

### Task 3: Native CLI

`cargo build --release` — native CLI (`axel-validate <domain>`) for CI/CD and local testing.

- `axel-validate/src/main.rs` — CLI binary
- Subcommands: `validate <domain>`, `check-policy <file>`, `parse-txt <record>`
- Exit codes: 0 (valid), 1 (invalid), 2 (network error)

### Task 4: ReScript bindings

Thin wrapper importing the WASM module, with JS fallback.

- `src/AxelCore.res` — ReScript bindings to WASM exports
- `src/AxelCoreFallback.res` — pure JS implementation (current code)
- Runtime detection: use WASM if available, fall back to JS

### Task 5: Idris2 ABI proofs

Formal verification of protocol invariants.

- `src/abi/PolicyTypes.idr` — dependent types for policy objects
- `src/abi/IsolationLevel.idr` — proofs about isolation level constraints
- `src/abi/CacheInvariants.idr` — proofs about cache behavior
- `src/abi/EnforcementRules.idr` — proofs about enforcement correctness

### Task 6: Zig FFI platform bindings

C-compatible bindings for infrastructure integration.

- `ffi/zig/src/axel.zig` — core FFI functions
- `ffi/zig/src/nginx.zig` — nginx module integration
- `ffi/zig/src/firewalld.zig` — firewalld plugin
- `ffi/zig/src/dns_resolver.zig` — knot/unbound hooks
