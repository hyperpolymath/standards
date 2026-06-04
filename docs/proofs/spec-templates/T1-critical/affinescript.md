# Proof Spec: affinescript
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/affinescript`
**Tier**: T1 — Critical (CURRENTLY NO PROOFS — urgent gap)
**Total Theorems**: 6
**Primary Prover(s)**: Coq (4), Idris2 (2)
**Existing Proof Coverage**: 0% (compiler claims affine types but has no proofs)
**Dependencies**: None

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | AF1 Affine consumption (each var ≤ 1 use) | Cq | [ ] Pending | — |
| 2 | AF2 Type safety (progress + preservation) | Cq | [ ] Pending | — |
| 3 | AF3 Effect handler isolation | Cq | [ ] Pending | — |
| 4 | AF4 Row polymorphism coherence | Cq | [ ] Pending | — |
| 5 | AF5 Protocol state validity | I2 | [ ] Pending | — |
| 6 | AF6 Runtime memory safety (Rust) | I2 | [ ] Pending | — |

## Context

Affine type system for game devs. OCaml compiler, Rust runtime. Combines QTT (0/1/ω multiplicities), row polymorphism, algebraic effect handlers, refinement types.

**CRITICAL**: No proofs exist despite affine type claims. This is a safety gap.

## Theorems to Prove

### AF1: Affine consumption

**Target file**: `formal/coq/AffineConsumption.v` (NEW)
**Priority**: P0

**Statement**:
> Every affine-typed variable is used at most once. The type system rejects programs that violate this.

**Formal signature**:
```coq
Inductive Mult := Zero | One | Omega.  (* QTT multiplicities *)

Inductive HasType : Context -> Expr -> Ty -> Mult -> Prop := ...

Theorem affine_linearity : forall Gamma e tau,
  HasType Gamma e tau One ->
  forall x, UseCount x e <= 1.
```

**Obligations**:
- [ ] Define QTT context splitting
- [ ] Prove affine use count bound

---

### AF2: Type safety

**Target file**: `formal/coq/TypeSafety.v` (NEW)
**Priority**: P0

**Statement**:
> Progress: well-typed terms are values or can step. Preservation: stepping preserves types.

**Obligations**:
- [ ] Define operational semantics
- [ ] Prove progress
- [ ] Prove preservation

---

### AF3: Effect handler isolation

**Target file**: `formal/coq/EffectIsolation.v` (NEW)
**Priority**: P1

**Statement**:
> Algebraic effect handlers cannot leak state across handler boundaries.

**Obligations**:
- [ ] Model handlers
- [ ] Prove isolation

---

### AF4: Row polymorphism coherence

**Target file**: `formal/coq/RowPoly.v` (NEW)
**Priority**: P1

**Statement**:
> Record operations (extend, restrict, select) preserve row type structure.

**Obligations**:
- [ ] Define row types
- [ ] Prove coherence laws

---

### AF5: Protocol state validity

**Target file**: `verification/proofs/idris2/ProtocolStates.idr`
**Priority**: P2

**Statement**:
> Type-level protocol state ensures game state transitions are valid.

**Obligations**:
- [ ] Model protocol state machines at type level
- [ ] Prove only valid transitions

---

### AF6: Runtime memory safety

**Target file**: `verification/proofs/idris2/RuntimeMemSafe.idr`
**Priority**: P1

**Statement**:
> Rust runtime preserves affine guarantees. No use-after-free.

**Obligations**:
- [ ] Model runtime representation
- [ ] Prove affine → no aliasing

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/affinescript
cd formal/coq && make
just proof-check-idris2
```

## Handoff Checklist

- [ ] All 6 theorems proven (all new)
- [ ] Commit: `proof: establish affinescript baseline proofs (6/6 theorems)`
