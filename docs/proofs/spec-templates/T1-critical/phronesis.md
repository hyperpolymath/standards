# Proof Spec: phronesis
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/phronesis`
**Tier**: T1 — Critical
**Total Theorems**: 4
**Primary Prover(s)**: Coq (1), Lean4 (1), Agda (1), TLA+ (1)
**Existing Proof Coverage**: Phronesis.v (672 LOC, 0), Phronesis.lean (328 LOC, **1 sorry**), Phronesis.agda (323 LOC, **1 postulate**)
**Dependencies**: None

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | PH1 Close Lean sorry + Agda postulate | L4+Ag | [ ] Pending | — |
| 2 | PH2 Belief type correctness | L4 | [ ] Pending | — |
| 3 | PH3 Ethical loop consensus | TLA | [ ] Pending | — |
| 4 | PH4 Module sandbox isolation | Cq | [ ] Pending | — |

## Context

Neuro-symbolic language for ethical reasoning. Probabilistic types (BeliefFunction, ProbabilityDistribution), first-order logic operators, BEAM/Raft consensus.

## Theorems to Prove

### PH1: Close Lean sorry + Agda postulate

**Target files**: `academic/formal-verification/lean4/Phronesis.lean`, `academic/formal-verification/agda/Phronesis.agda` (EXISTING)
**Priority**: P0

**Obligations**:
- [ ] Close 1 sorry in Phronesis.lean
- [ ] Eliminate 1 postulate in Phronesis.agda

---

### PH2: Belief type correctness

**Target file**: `academic/formal-verification/lean4/BeliefTypes.lean` (NEW)
**Priority**: P1

**Statement**: BeliefFunction and ProbabilityDistribution types correctly model epistemic uncertainty. Values always in [0,1], sums preserve invariants.

**Obligations**:
- [ ] Define types with bounds
- [ ] Prove arithmetic preserves bounds

---

### PH3: Ethical loop consensus

**Target file**: `academic/formal-verification/tla/EthicalLoop.tla` (NEW)
**Priority**: P1

**Statement**: Raft consensus in distributed ethical loop preserves decision correctness.

**Obligations**:
- [ ] Model BEAM/Raft for ethical decisions
- [ ] Model-check agreement

---

### PH4: Module sandbox isolation

**Target file**: `academic/formal-verification/coq/Sandboxing.v` (NEW)
**Priority**: P2

**Statement**: Phronesis modules cannot access state of other modules without explicit capability.

**Obligations**:
- [ ] Model module isolation
- [ ] Prove capability requirement

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/phronesis
cd academic/formal-verification && make
```

## Handoff Checklist

- [ ] PH1: sorry + postulate closed
- [ ] PH2-PH4 complete
- [ ] Commit: `proof: complete phronesis proofs (4/4 theorems)`
