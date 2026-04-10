# Proof Spec: anvomidav
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/anvomidav`
**Tier**: T1 — Critical (pre-implementation — design phase)
**Total Theorems**: 2 (foundational, design-stage)
**Primary Prover(s)**: Idris2
**Existing Proof Coverage**: 0% — concept phase
**Dependencies**: None

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | AV1 Type system skeleton (foundational) | I2 | [ ] Pending | — |
| 2 | AV2 ISU rules validation soundness | I2 | [ ] Pending | — |

## Context

Figure skating choreography DSL. Notate, compose, validate (ISU rules), visualise skating programs. Pre-implementation: compiler target TBD (OCaml or Rust), viz in ReScript+WebGL.

## Theorems to Prove

### AV1: Type system skeleton

**Target file**: `verification/proofs/idris2/AnvomidavTypes.idr` (NEW)
**Priority**: P1

**Statement**: Define foundational type system for moves, sequences, programs. Type soundness for program composition.

**Obligations**:
- [ ] Define Move, Sequence, Program types
- [ ] Define composition type rules
- [ ] Prove composition well-typedness

---

### AV2: ISU rules validation soundness

**Target file**: `verification/proofs/idris2/ISUValidator.idr` (NEW)
**Priority**: P2

**Statement**: Programs marked "ISU-valid" actually satisfy all ISU scoring rules.

**Obligations**:
- [ ] Formalise ISU rules as predicates
- [ ] Prove validator implements each rule

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/anvomidav
just proof-check-idris2
```

## Handoff Checklist

- [ ] AV1 foundational types defined
- [ ] AV2 validator correctness proved (when implementation exists)
- [ ] Commit: `proof: establish anvomidav foundational proofs (2/2 theorems)`

## Blockers

Implementation is pre-alpha. Proofs may need to wait until type system design stabilises. Document design decisions before proving.
