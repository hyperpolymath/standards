# Proof Spec: tangle
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/tangle`
**Tier**: T1 — Critical
**Total Theorems**: 3
**Primary Prover(s)**: Lean4
**Existing Proof Coverage**: Tangle.lean (560 LOC, **0 sorry**)
**Dependencies**: None

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | TG1 Isotopy equivalence | L4 | [ ] Pending | — |
| 2 | TG2 Braid composition preserves types | L4 | [ ] Pending | — |
| 3 | TG3 Knot polynomial correctness | L4 | [ ] Pending | — |

## Context

Topological Turing-complete language: programs are tangles (braided strands). Computation via knot invariants (Jones, Alexander, HOMFLY). Two types: `Word[n]` (matchable braid word) vs `Tangle[A,B]` (extensional morphism).

## Theorems to Prove

### TG1: Isotopy equivalence

**Target file**: `proofs/Isotopy.lean` (NEW)
**Priority**: P0

**Statement**: Isotopic tangles produce identical results. Reidemeister moves preserve semantics.

**Obligations**:
- [ ] Define isotopy relation
- [ ] Prove semantic equivalence

---

### TG2: Braid composition preserves types

**Target file**: `proofs/BraidCompose.lean` (NEW)
**Priority**: P0

**Statement**: `compose : Tangle[A,B] × Tangle[B,C] → Tangle[A,C]` is well-typed.

**Obligations**:
- [ ] Model Tangle[A,B] as category
- [ ] Prove composition respects endpoints

---

### TG3: Knot polynomial correctness

**Target file**: `proofs/KnotPolynomials.lean` (NEW)
**Priority**: P1

**Statement**: Computed Jones, Alexander, HOMFLY polynomials match mathematical definitions.

**Obligations**:
- [ ] Define each polynomial axiomatically
- [ ] Prove computation matches axioms

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/tangle
lean proofs/*.lean
```

## Handoff Checklist

- [ ] All 3 theorems proven
- [ ] Commit: `proof: complete tangle proofs (3/3 theorems)`
