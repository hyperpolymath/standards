# Proof Spec: my-lang
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/my-lang`
**Tier**: T1 — Critical
**Total Theorems**: 5
**Primary Prover(s)**: Coq
**Existing Proof Coverage**: Syntax.v (0 Admitted), Typing.v (**1 postulate**)
**Dependencies**: None

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | ML1 Close 1 postulate in Typing.v | Cq | [ ] Pending | — |
| 2 | ML2 Type preservation across 4 dialects | Cq | [ ] Pending | — |
| 3 | ML3 Progress across 4 dialects | Cq | [ ] Pending | — |
| 4 | ML4 Confluence (Duet + Ensemble) | Cq | [ ] Pending | — |
| 5 | ML5 Dialect interop type safety | Cq | [ ] Pending | — |

## Context

Educational framework: 4 dialects (Me = imperative, Solo = functional, Duet = concurrent, Ensemble = distributed). HM inference across all dialects. Progressive teaching.

### Key files
- `proofs/verification/coq/Syntax.v` (172 LOC, 0 Admitted)
- `proofs/verification/coq/Typing.v` (585 LOC, 1 postulate)

## Theorems to Prove

### ML1: Close Typing.v postulate

**Target file**: `proofs/verification/coq/Typing.v` (EXISTING)
**Priority**: P0

**Obligations**:
- [ ] Identify the 1 postulate
- [ ] Replace with constructive proof
- [ ] Zero postulates remaining

---

### ML2: Type preservation

**Target file**: `proofs/verification/coq/Preservation.v` (NEW)
**Priority**: P0

**Statement**: Preservation holds for each of 4 dialects.

**Obligations**:
- [ ] Prove for Me, Solo, Duet, Ensemble separately

---

### ML3: Progress

**Target file**: `proofs/verification/coq/Progress.v` (NEW)
**Priority**: P0

**Obligations**:
- [ ] Prove for each dialect

---

### ML4: Confluence (concurrent/distributed dialects)

**Target file**: `proofs/verification/coq/Confluence.v` (NEW)
**Priority**: P1

**Statement**: Duet and Ensemble have determinate semantics (Church-Rosser property).

**Obligations**:
- [ ] Define parallel reduction
- [ ] Prove diamond property
- [ ] Derive Church-Rosser

---

### ML5: Dialect interop type safety

**Target file**: `proofs/verification/coq/DialectInterop.v` (NEW)
**Priority**: P2

**Statement**: Terms can be moved between dialects preserving type.

**Obligations**:
- [ ] Define dialect embedding
- [ ] Prove type preservation under embedding

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/my-lang
cd proofs/verification/coq && make
```

## Handoff Checklist

- [ ] ML1 postulate eliminated
- [ ] ML2-ML5 complete
- [ ] Commit: `proof: complete my-lang proofs (5/5 theorems)`
