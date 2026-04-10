# Proof Spec: eclexia
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/eclexia`
**Tier**: T1 — Critical
**Total Theorems**: 4
**Primary Prover(s)**: Coq (3), Agda (1)
**Existing Proof Coverage**: Typing.v (0 Admitted, complete), ShadowPrices.v (5 Axioms documented), ResourceTracking.agda (1 postulate)
**Dependencies**: None

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | EC1 Reduce 5 LP axioms to cited theorems | Cq | [ ] Pending | — |
| 2 | EC2 Eliminate postulate in ResourceTracking | Ag | [ ] Pending | — |
| 3 | EC3 Shadow-price optimality (general) | Cq | [ ] Pending | — |
| 4 | EC4 Compilation correctness (src → wasm) | Cq | [ ] Pending | — |

## Context

Economics-as-Code resource language. Energy, time, memory, carbon as first-class constraints. Rust compiler (25 crates, 507 tests), WASM/LLVM/Cranelift backends. Type safety COMPLETE; extended theorems pending.

### Key files
- `formal/coq/src/Syntax.v` (261 LOC, 0 Admitted)
- `formal/coq/src/Typing.v` (559 LOC, 0 Admitted)
- `formal/coq/src/ShadowPrices.v` (467 LOC, **5 Axioms**: weak duality, complementary slackness, LP sensitivity, strong duality, dual simplex convergence)
- `formal/agda/ResourceTracking.agda` (299 LOC, 1 postulate)

## Theorems to Prove

### EC1: Reduce LP axioms to cited theorems

**Target file**: `formal/coq/src/LPFoundations.v` (NEW)
**Priority**: P1

**Statement**:
> The 5 LP axioms in ShadowPrices.v (weak duality, complementary slackness, LP sensitivity, strong duality, dual simplex convergence) should either be proven or cited with formal references.

**Obligations**:
- [ ] For each of 5 axioms: either prove OR add explicit citation to a published formal proof
- [ ] Acceptable: Axioms with citations to Bertsimas & Tsitsiklis, or to formalized LP libraries
- [ ] Document which are reducible and which remain cited

---

### EC2: Eliminate postulate in ResourceTracking

**Target file**: `formal/agda/ResourceTracking.agda` (EXISTING)
**Priority**: P1

**Statement**:
> Close the 1 postulate in ResourceTracking.agda with a constructive proof.

**Obligations**:
- [ ] Identify the postulate
- [ ] Provide constructive proof
- [ ] Zero postulates remaining

---

### EC3: Shadow-price optimality (general conditions)

**Target file**: `formal/coq/src/OptimalityGeneral.v` (NEW)
**Priority**: P2

**Statement**:
> Extend shadow-price optimality beyond the current research preview conditions to general multi-objective optimisation.

**Obligations**:
- [ ] State general theorem
- [ ] Prove under standard LP assumptions

---

### EC4: Compilation correctness (source → WASM)

**Target file**: `formal/coq/src/CompilerCorrect.v` (NEW)
**Priority**: P2

**Statement**:
> For any eclexia program P, `compile(P)` preserves P's operational semantics in the WASM target.

**Obligations**:
- [ ] Model WASM target semantics
- [ ] Prove compilation is a simulation

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/eclexia
cd formal/coq && make
agda --safe formal/agda/ResourceTracking.agda
```

## Handoff Checklist

- [ ] EC1: 5 axioms reduced or cited
- [ ] EC2: 1 postulate eliminated
- [ ] EC3, EC4: complete
- [ ] Commit: `proof: complete eclexia extended proofs (4/4 theorems)`
