# Proof Spec: error-lang
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/error-lang`
**Tier**: T1 — Critical
**Total Theorems**: 3
**Primary Prover(s)**: Idris2
**Existing Proof Coverage**: 1 ABI stub (Foreign.idr), no formal proofs
**Dependencies**: None

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | EL1 Stability metric validity | I2 | [ ] Pending | — |
| 2 | EL2 Paradox detection soundness | I2 | [ ] Pending | — |
| 3 | EL3 Position metadata preservation | I2 | [ ] Pending | — |

## Context

Pedagogical language — intentionally fragile. Syntax/semantics expose complexity via computational haptics (visual feedback on code quality). ReScript compiler (7,468 LOC), Zig FFI for haptics.

## Theorems to Prove

### EL1: Stability metric validity

**Target file**: `verification/proofs/idris2/Stability.idr`
**Priority**: P1

**Statement**: Computational haptics stability score correlates with actual type-error count. Programs with fewer errors get higher stability.

**Obligations**:
- [ ] Define stability metric
- [ ] Prove monotonicity (more errors → lower stability)

---

### EL2: Paradox detection soundness

**Target file**: `verification/proofs/idris2/Paradox.idr`
**Priority**: P1

**Statement**: All semantic contradictions are detected. No false negatives.

**Obligations**:
- [ ] Enumerate paradox patterns
- [ ] Prove detection function covers all

---

### EL3: Position metadata preservation

**Target file**: `verification/proofs/idris2/PositionMeta.idr`
**Priority**: P2

**Statement**: Source position metadata preserved through parsing, type-checking, error reporting.

**Obligations**:
- [ ] Model AST with positions
- [ ] Prove every transform preserves positions

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/error-lang
just proof-check-idris2
```

## Handoff Checklist

- [ ] All 3 theorems proven
- [ ] Commit: `proof: establish error-lang foundational proofs (3/3 theorems)`
