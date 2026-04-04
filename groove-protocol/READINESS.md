<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# groove-protocol Component Readiness Assessment

**Standard:** [Component Readiness Grades (CRG) v1.0](https://github.com/hyperpolymath/standards/tree/main/component-readiness-grades)
**Assessed:** 2026-04-04
**Assessor:** Jonathan D.A. Jewell + Claude Sonnet 4.6

**Current Grade:** B

## Summary

| Component          | Grade | Release Stage      | Evidence Summary                                                                                      |
|--------------------|-------|--------------------|------------------------------------------------------------------------------------------------------|
| `groove validate`  | B     | Release Candidate  | Manifests deployed on 65+ repos across diverse service types; validated in CI via dogfood-gate groove-check job. |
| `groove init`      | B     | Release Candidate  | Initialises Groove manifests on new repos; used as part of rsr-template-repo onboarding for all new repos. |
| `groove check`     | B     | Release Candidate  | CI enforcement subcommand; wired in dogfood-gate groove-check job on 65+ repos. |
| `groove-core`      | C     | Beta               | Core Idris2 ABI layer with formal proofs of protocol correctness; type-checks with zero believe_me; no runtime tests yet. |
| `groove-ffi`       | C     | Beta               | Zig FFI implementation; 10 language binding targets; builds clean; integration tests partial. |
| `groove-browser`   | C     | Beta               | Firefox-first browser extension; Groove protocol in browser context; Chrome MV3 workarounds implemented. |

## Overall Project Readiness

- **Components at B or above:** 3/6 (50%) — groove validate, groove init, groove check
- **Components at C (Beta) or above:** 6/6 (100%)
- **Components at D (Alpha):** 0/6 (0%)
- **Weighted assessment:** The primary use case (universal plug-and-play inter-service protocol with CI enforcement) is **Grade B**. Core ABI/FFI layers are Beta-quality.

## Detailed Assessment

### `groove validate` — Manifest Validation (Grade: B)

**Evidence:**
- Groove manifests deployed on 65+ hyperpolymath repos
- Diverse service type coverage confirmed:
  1. Rust CLI tools (panic-attacker, januskey, conflow) — native binary services
  2. Elixir/Phoenix services (burble, oblibeny) — BEAM web services
  3. Gleam services (k9_gleam, a2ml_gleam) — typed BEAM services
  4. Julia batch services (statistease, developer-ecosystem) — data pipeline services
  5. ReScript/Deno frontends (idaptik, nafa-app) — browser-facing services
  6. Idris2 proof services (ephapax, stapeln) — formal verification services
  7. Infrastructure services (hypatia, gitbot-fleet) — CI/CD services
  8. Container services (boj-server, idaptik containers) — Stapeln/Podman services
- Validation covers: protocol version, endpoint schema, capability declarations, authentication requirements

**Known limitations:**
- Groove v2 manifest format not yet deployed on all repos (v1/v2 coexistence in progress)
- Some legacy repos have partial manifests (missing optional capability sections)

**Promotion path to A:** External services outside hyperpolymath adopt Groove manifests; IANA service type registration.

### `groove init` — Manifest Initialisation (Grade: B)

**Evidence:**
- Used as part of rsr-template-repo onboarding process for all new repos
- Applied to 65+ existing repos during Groove deployment
- Generates correct v1/v2 manifests for all supported service types
- Idempotent: safe to run on repos that already have manifests

**Known limitations:**
- v2 manifest generation requires `--v2` flag (default still v1 for compatibility)
- Custom capability templates not yet supported

**Promotion path to A:** External adopters confirm init workflow is non-breaking.

### `groove check` — CI Enforcement (Grade: B)

**Evidence:**
- Deployed as `groove-check` job in dogfood-gate CI workflow on 65+ repos
- Blocks merge when manifest is absent or malformed
- Runs on diverse CI environments: GitHub Actions (primary), GitLab CI (mirror)
- SHA-pinned, `permissions: read-all`, SPDX headers present

**Known limitations:**
- Periodic SHA pin refresh required for CI workflow
- Some legacy repos not yet enrolled in groove-check enforcement

**Promotion path to A:** External maintainers adopt groove-check; no harmful false-positives in wild.

### `groove-core` — Idris2 ABI Layer (Grade: C)

**Evidence:**
- Formal proofs of protocol correctness in Idris2 with dependent types
- Zero `believe_me`, zero `assert_total`, zero `Admitted`
- Type-checks clean with `%default total`
- Protocol invariants proved: message ordering, capability negotiation, connection state machine

**Known limitations:**
- Proof checking not wired in CI (requires idris2 binary in CI image)
- No runtime tests yet (proofs cover specification, not implementation)

**Promotion path to B:** Proof check wired in CI; runtime integration tests on 6+ target language pairs.

### `groove-ffi` — Zig FFI Implementation (Grade: C)

**Evidence:**
- 10 language binding targets: Rust, Elixir, Gleam, Julia, ReScript, OCaml, Haskell, Ada, Idris2, Zig-native
- Builds clean for all 10 targets
- C headers generated from Idris2 ABI
- Integration tests passing for Rust and Elixir bindings

**Known limitations:**
- Integration tests only complete for Rust and Elixir (8/10 targets need integration tests)
- Cross-compilation for non-Linux targets untested

**Promotion path to B:** Integration tests complete for all 10 binding targets.

### `groove-browser` — Browser Extension (Grade: C)

**Evidence:**
- Firefox-first implementation; Chrome MV3 workarounds implemented
- Groove protocol available in browser context for web-facing services
- Dogfooded on idaptik and nafa-app frontend integrations

**Known limitations:**
- Chrome MV3 service worker constraints limit some Groove capabilities
- Browser extension store submission not yet made (Firefox + Chrome)

**Promotion path to B:** Published on Firefox Add-ons and Chrome Web Store; 6+ external users.

## Architecture (Idris2 ABI + Zig FFI Standard)

```
groove-protocol/
├── spec/                  # Groove protocol specification
├── src/abi/               # Idris2 ABI definitions (formal proofs)
│   ├── Types.idr          # Protocol types with dependent proofs
│   ├── Layout.idr         # Memory layout verification
│   └── Foreign.idr        # FFI declarations
├── ffi/zig/               # Zig FFI implementation
│   ├── build.zig
│   ├── src/main.zig       # C-ABI compatible implementation
│   └── test/              # Integration tests
├── generated/abi/         # Auto-generated C headers
├── bindings/              # Language-specific wrappers
│   ├── rust/
│   ├── elixir/
│   ├── gleam/
│   ├── julia/
│   ├── rescript/
│   ├── ocaml/
│   ├── haskell/
│   ├── ada/
│   └── idris2/
├── cli/                   # groove CLI (validate, init, check)
├── harness/               # Test harness for CI integration
└── examples/              # Usage examples
```

## Recipes

```
just validate <path>     # Validate Groove manifest
just init <path>         # Initialise new manifest
just check <path>        # CI check (pass/fail)
just test                # All tests including FFI integration
just build               # Build CLI + all bindings
just prove               # Run Idris2 proof checks
```

## Known Debt

- Groove v2 manifest migration not complete (v1/v2 coexistence)
- Proof checking not wired in CI
- 8/10 FFI binding integration tests incomplete
- Browser extension store submissions pending
- IANA service type registration pending
