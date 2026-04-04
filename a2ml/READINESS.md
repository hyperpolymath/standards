<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# a2ml Component Readiness Assessment

**Standard:** [Component Readiness Grades (CRG) v1.0](https://github.com/hyperpolymath/standards/tree/main/component-readiness-grades)
**Assessed:** 2026-04-04
**Assessor:** Jonathan D.A. Jewell + Claude Sonnet 4.6

**Current Grade:** B

## Summary

| Component          | Grade | Release Stage      | Evidence Summary                                                                               |
|--------------------|-------|--------------------|-----------------------------------------------------------------------------------------------|
| `a2ml-validator`   | B     | Release Candidate  | Deployed via dogfood-gate on 105+ repos; validates STATE.a2ml, META.a2ml, ECOSYSTEM.a2ml, AGENTIC.a2ml, NEUROSYM.a2ml, PLAYBOOK.a2ml across Rust, Elixir, Gleam, Julia, ReScript contexts. |
| `a2ml-rs`          | B     | Release Candidate  | Rust implementation; used as reference validator; CLI and library API both validated on 105+ repos. |
| `a2ml_ex`          | C     | Beta               | Elixir implementation; integrated into mix pipeline on Elixir repos; dogfooded on burble, oblibeny, boj-server adapter layer. |
| `a2ml_gleam`       | C     | Beta               | Gleam implementation; wired on BEAM/Gleam repos; dogfooded on k9_gleam, a2ml_gleam, polyglot-formalisms-gleam. |
| `a2ml-haskell`     | C     | Beta               | Haskell implementation; validated on Haskell repos in the estate. |
| `a2ml-deno`        | C     | Beta               | Deno/TypeScript-free JS implementation; used on ReScript/Deno frontend repos (idaptik, nafa-app). |
| `schema`           | B     | Release Candidate  | Core A2ML schema definition; stable since v1.0; referenced by all 6 language implementations and 105+ repos. |
| `dogfood-gate`     | B     | Release Candidate  | CI enforcement workflow; deployed on all repos requiring A2ML compliance; diverse language targets confirmed. |

## Overall Project Readiness

- **Components at B or above:** 4/8 (50%) — a2ml-validator, a2ml-rs, schema, dogfood-gate
- **Components at C (Beta) or above:** 8/8 (100%)
- **Components at D (Alpha):** 0/8 (0%)
- **Weighted assessment:** The A2ML standard and its primary validator are **Grade B**. Language-specific implementations are Beta-quality with real dogfooding.

## Detailed Assessment

### `a2ml-validator` — Core A2ML Validation Engine (Grade: B)

**Evidence:**
- Deployed via `dogfood-gate` CI workflow on 105+ hyperpolymath repos
- Validates 6 canonical file types: STATE.a2ml, META.a2ml, ECOSYSTEM.a2ml, AGENTIC.a2ml, NEUROSYM.a2ml, PLAYBOOK.a2ml
- Language context diversity confirmed:
  1. Rust repos (panic-attacker, januskey, conflow, a2ml-rs, ephapax) — .machine_readable/6a2/ paths
  2. Elixir/Phoenix repos (burble, oblibeny, boj-server adapters) — BEAM ecosystem
  3. Gleam repos (k9_gleam, a2ml_gleam, polyglot-formalisms-gleam) — typed BEAM
  4. Julia repos (7-tentacles, statistease, developer-ecosystem) — scientific computing
  5. ReScript/Deno repos (idaptik, nafa-app) — web frontend
  6. Idris2 repos (ephapax, stapeln) — formal verification
  7. Multi-language monorepos (developer-ecosystem, nextgen-languages) — polyglot
  8. Standards repos (standards, rsr-template-repo) — meta-validation
- Findings: 41 repos flagged for SCM→A2ML migration (tracked in memory file scm-to-a2ml-migration.md)

**Known limitations:**
- A2ML parser is strict; minor formatting issues cause validation failure rather than warning
- PLAYBOOK.a2ml schema not yet finalised (v0.9)
- Cross-reference validation between A2ML files not yet implemented

**Promotion path to A:** External users outside hyperpolymath adopt A2ML and confirm validator is non-blocking for their workflows.

### `schema` — A2ML Schema Definition (Grade: B)

**Evidence:**
- Core schema stable since v1.0
- Referenced by 6 language implementations (Rust, Elixir, Gleam, Haskell, Deno, Julia)
- Deployed on 105+ repos as the canonical AI manifest format
- IANA media type submission in progress (`application/vnd.a2ml+text`)

**Known limitations:**
- PLAYBOOK.a2ml schema at v0.9 (not yet stable)
- No formal grammar (EBNF/PEG) published yet

**Promotion path to A:** IANA media type approved; grammar published; external adopters.

### `dogfood-gate` — CI Enforcement Workflow (Grade: B)

**Evidence:**
- Deployed on all RSR-compliant repos requiring A2ML compliance (105+)
- Blocks merge on validation failure
- Targets confirmed across all primary hyperpolymath languages
- SHA-pinned, `permissions: read-all`, SPDX header present

**Known limitations:**
- Some repos have partial A2ML files (missing PLAYBOOK.a2ml) — gate configured to warn only for optional files
- Periodic SHA pin refresh required

**Promotion path to A:** External maintainers adopt dogfood-gate; no harmful false-positives in wild.

### `a2ml-rs` — Rust Implementation (Grade: B)

**Evidence:**
- Reference implementation; CLI and library API
- Used as validator on 105+ repos via dogfood-gate
- Extensive test suite; CI passing

**Known limitations:**
- Some edge cases in UTF-8 boundary handling
- Library API not yet stabilised (semver pre-1.0)

### `a2ml_ex` — Elixir Implementation (Grade: C)

**Evidence:**
- Integrated into mix pipeline on all Elixir repos in the estate
- Dogfooded on burble, oblibeny, boj-server adapter layer

**Promotion path to B:** Validated on 6+ diverse external Elixir projects.

### `a2ml_gleam` — Gleam Implementation (Grade: C)

**Evidence:**
- Wired on BEAM/Gleam repos (k9_gleam, a2ml_gleam, polyglot-formalisms-gleam)
- Compiles to both BEAM and JavaScript targets

**Promotion path to B:** Validated on 6+ diverse Gleam/BEAM projects.

### `a2ml-haskell` — Haskell Implementation (Grade: C)

**Evidence:**
- Validated on Haskell repos in the estate (a2ml-haskell itself, scaffoldia)

**Promotion path to B:** Validated on 6+ diverse Haskell projects.

### `a2ml-deno` — Deno Implementation (Grade: C)

**Evidence:**
- Used on ReScript/Deno frontend repos (idaptik, nafa-app)
- Zero npm dependencies (pure Deno)

**Promotion path to B:** Validated on 6+ diverse Deno/ReScript projects.

## Recipes

```
just validate <path>     # Validate A2ML files in a repo
just test                # All implementation tests
just build               # Build all language implementations
just check-schema        # Validate schema self-consistency
just lint                # Format and lint checks
```

## Known Debt

- PLAYBOOK.a2ml schema not yet at v1.0
- No formal grammar (EBNF/PEG) for the A2ML format
- Cross-reference validation between A2ML files not implemented
- 41 repos still using SCM files instead of A2ML (migration tracked)
- IANA media type application pending
