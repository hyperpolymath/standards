# Proof Spec: ephapax
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/ephapax`
**Tier**: T1 — Critical
**Total Theorems**: 5
**Primary Prover(s)**: Coq (3), Idris2 (2)
**Existing Proof Coverage**: Syntax.v (0 Admitted), Typing.v (0 Admitted), Semantics.v (2 Admitted — preservation cases + substitution lemma), 17 Idris2 files
**Dependencies**: None (self-contained language)

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | EP1 Close substitution lemma Admitted | Cq | [ ] Pending | — |
| 2 | EP2 Close 2 preservation Admitted | Cq | [ ] Pending | — |
| 3 | EP3 Type soundness (progress + preservation) | Cq | [ ] Partial | — |
| 4 | EP4 Linear resource tracking | I2 | [ ] Pending | — |
| 5 | EP5 Region-based memory safety | I2 | [ ] Pending | — |

## Context

Linear-typed language targeting WebAssembly, no runtime GC. Uses Lin/Unr qualifiers, region-based memory management (MLKit/Cyclone style), borrow semantics like Rust.

### Key files
- `formal/Syntax.v` (372 LOC, 0 Admitted)
- `formal/Typing.v` (213 LOC, 0 Admitted)
- `formal/Semantics.v` (2872 LOC, **2 Admitted** — blockers)
- `src/formal/Ephapax/Formal/Qualifier.idr` (186 LOC)
- `src/formal/Ephapax/Formal/Region.idr` (136 LOC)
- `src/formal/Ephapax/Formal/RegionLinear.idr` (169 LOC)

## Theorems to Prove

### EP1: Close substitution lemma Admitted

**Target file**: `formal/Semantics.v` (EXISTING — has 1 Admitted to close)
**Priority**: P0

**Statement**:
> The substitution lemma: `Γ, x:τ₁ ⊢ e : τ₂` and `Γ ⊢ v : τ₁` imply `Γ ⊢ e[v/x] : τ₂`.

**Hints**:
- This is the last blocker for type soundness
- See existing proof attempts in Semantics.v
- Linear substitution is trickier than standard STLC — track qualifier preservation

**Obligations**:
- [ ] Replace `Admitted` with complete `Qed` proof
- [ ] Handle Lin/Unr qualifiers correctly
- [ ] Consider T_Borrow_Val case (already added to fix substitution gap per memory)

---

### EP2: Close 2 preservation Admitted

**Target file**: `formal/Semantics.v` (EXISTING — 1 Admitted to close)
**Priority**: P0

**Statement**:
> Type preservation: if `Γ ⊢ e : τ` and `e → e'` then `Γ ⊢ e' : τ`.

**Obligations**:
- [ ] Close remaining preservation case(s)
- [ ] No new Admitted
- [ ] All `Qed`

---

### EP3: Type soundness

**Target file**: `formal/TypeSoundness.v` (NEW)
**Priority**: P0

**Statement**:
> Combined: well-typed terms don't get stuck. Progress + Preservation theorem.

**Obligations**:
- [ ] Import Semantics.v (depends on EP1, EP2)
- [ ] State and prove soundness theorem

---

### EP4: Linear resource tracking

**Target file**: `verification/proofs/idris2/LinearTracking.idr`
**Source**: `src/formal/Ephapax/Formal/Qualifier.idr`
**Priority**: P1

**Statement**:
> Every linear binding is consumed exactly once. Unrestricted bindings can be used any number of times.

**Obligations**:
- [ ] Model use counts
- [ ] Prove Lin → count = 1, Unr → count ≥ 0

---

### EP5: Region-based memory safety

**Target file**: `verification/proofs/idris2/RegionSafety.idr`
**Source**: `src/formal/Ephapax/Formal/Region.idr`
**Priority**: P1

**Statement**:
> Regions do not escape their scope. No use-after-free.

**Obligations**:
- [ ] Model region lifetime
- [ ] Prove references cannot outlive region

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/ephapax
cd formal && make && cd ..
just proof-check-idris2
```

## Handoff Checklist

- [ ] Zero Admitted in Coq (currently 2)
- [ ] EP1, EP2 close existing Admitteds
- [ ] EP3-EP5 new proofs complete
- [ ] Commit: `proof: complete ephapax type soundness (5/5 theorems)`
