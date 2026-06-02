# Proof Spec: betlang
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/betlang`
**Tier**: T1 — Critical
**Total Theorems**: 4
**Primary Prover(s)**: Lean4
**Existing Proof Coverage**: BetLang.lean (653 LOC, **3 sorry**)
**Dependencies**: None

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | BL1 Close 3 sorry in BetLang.lean | L4 | [ ] Pending | — |
| 2 | BL2 Probability distribution validity | L4 | [ ] Pending | — |
| 3 | BL3 Lazy evaluation no side-effects | L4 | [ ] Pending | — |
| 4 | BL4 14 number systems soundness | L4 | [ ] Pending | — |

## Context

Ternary probabilistic DSL in Racket. `(bet A B C)` with implicit 1/3 probability, weighted variants, MCMC + Bayesian inference, 14 uncertainty number systems.

## Theorems to Prove

### BL1: Close 3 sorry in BetLang.lean

**Target file**: `proofs/BetLang.lean` (EXISTING)
**Priority**: P0

**Obligations**:
- [ ] Close 3 sorry statements constructively

---

### BL2: Probability distribution validity

**Target file**: `proofs/ProbDistributions.lean` (NEW)
**Priority**: P0

**Statement**: For every bet expression, probability weights sum to 1.0.

**Obligations**:
- [ ] Model bet distributions
- [ ] Prove sum = 1.0

---

### BL3: Lazy evaluation no side-effects

**Target file**: `proofs/LazyEval.lean` (NEW)
**Priority**: P1

**Statement**: Only the selected branch is evaluated. Unselected branches have no observable effects.

**Obligations**:
- [ ] Model evaluation semantics
- [ ] Prove side-effect isolation

---

### BL4: 14 number systems soundness

**Target file**: `proofs/NumberSystems.lean` (NEW)
**Priority**: P2

**Statement**: Each of 14 uncertainty number systems (DistnumberNormal, AffineNumber, FuzzyTriangular, BayesianNumber, RiskNumber, p-Adic, ...) has sound arithmetic.

**Obligations**:
- [ ] For each system: define arithmetic axioms
- [ ] Prove closure under operations

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/betlang
lean proofs/*.lean
```

## Handoff Checklist

- [ ] 3 sorry closed
- [ ] BL2-BL4 complete
- [ ] Commit: `proof: complete betlang proofs (4/4 theorems)`
