# Proof Spec: 007-lang (PRIVATE — Claude-only access)
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/007-lang`
**Tier**: T2 — High (PRIVATE — Triple-confirm before release)
**Total Theorems**: 6
**Primary Prover(s)**: Idris2 (3), Lean4 (2), Agda (1)
**Existing Proof Coverage**: Harvard.idr (has believe_me gap), TropicalSemiring.idr, CNO.idr/.agda, Lean4 templates

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | 07-1 Close Harvard.idr believe_me | I2 | [ ] Pending | — |
| 2 | 07-2 Session type duality | I2 | [ ] Pending | — |
| 3 | 07-3 Linear resource budget monotonicity | L4 | [ ] Pending | — |
| 4 | 07-4 Actor isolation | I2 | [ ] Pending | — |
| 5 | 07-5 CNO semantics | Ag | [ ] Pending | — |
| 6 | 07-6 Elixir codegen bisimulation | L4 | [ ] Pending | — |

## Context

Multi-tier compiler/interpreter. Harvard architecture separation, tropical type algebra, session types, linear resources, formal proofs. Compiles to 7 backends (Elixir/BEAM, Zig, WASM, QBE, Cranelift, C). **PRIVATE — access control: Private, Claude-only, triple-confirm before release.**

### Key files (public observable)
- `crates/oo7-core/src/parser.rs` (1,900+ lines, Pest)
- `crates/oo7-core/src/typechecker.rs` (4,400+ lines, L1-L9 Kategoria)
- `crates/oo7-core/src/dual_ast.rs` (Harvard verification)
- `crates/oo7-core/src/codegen_elixir.rs` (2,185 lines)
- `linker-mk2/src/linker.rs`
- `proofs/idris2/{Harvard.idr, TropicalSemiring.idr, CNO.idr}`
- `proofs/lean4/` (templates)
- `proofs/agda/CNO.agda`

## Theorems to Prove

### 07-1: Close Harvard.idr believe_me

**Target**: `proofs/idris2/Harvard.idr` (EXISTING — has gap)
**Priority**: P0

**Statement**: Data and code AST nodes never merge during parsing.

**Obligations**:
- [ ] Identify believe_me usage
- [ ] Replace with constructive proof
- [ ] Zero believe_me in final file

---

### 07-2: Session type duality

**Target**: `proofs/idris2/SessionDuality.idr` (NEW)
**Priority**: P0

**Statement**: Choreographic projections produce inverse endpoints (dual session types).

**Obligations**:
- [ ] Define session types
- [ ] Prove dual(dual(t)) = t

---

### 07-3: Linear resource budget monotonicity

**Target**: `proofs/lean4/BudgetMonotonic.lean` (NEW)
**Priority**: P0

**Statement**: Linear type budgets monotonically decrease through computation. Consume → zero.

**Obligations**:
- [ ] Model budget as Nat
- [ ] Prove monotonic decrease

---

### 07-4: Actor isolation

**Target**: `proofs/idris2/ActorIsolation.idr` (NEW)
**Priority**: P1

**Statement**: Actors cannot access other actors' state directly.

**Obligations**:
- [ ] Model actor mailboxes
- [ ] Prove isolation

---

### 07-5: CNO semantics

**Target**: `proofs/agda/CNO.agda` (EXISTING — verify)
**Priority**: P1

**Obligations**:
- [ ] Verify existing CNO proofs compile
- [ ] Extend to full semantics coverage

---

### 07-6: Elixir codegen bisimulation

**Target**: `proofs/lean4/ElixirBisim.lean` (NEW)
**Priority**: P2

**Statement**: Generated Elixir is semantically bisimilar to parsed 007 source.

**Obligations**:
- [ ] Model 007 + Elixir semantics
- [ ] Prove bisimulation

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/007-lang
just proof-check-all
```

## Banned Patterns

Zero believe_me in 007-lang proofs. Current Harvard.idr has one that MUST be closed.

## Handoff Checklist

- [ ] All 6 theorems proven
- [ ] Zero believe_me
- [ ] PRIVATE: do not push to public forks
- [ ] Commit: `proof: complete 007-lang core proofs (6/6)`
