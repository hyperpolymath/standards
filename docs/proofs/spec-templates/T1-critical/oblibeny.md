# Proof Spec: oblibeny
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/oblibeny`
**Tier**: T1 — Critical
**Total Theorems**: 4
**Primary Prover(s)**: Lean4 (3), Idris2 (1)
**Existing Proof Coverage**: Oblibeny.lean (571 LOC, **1 sorry**)
**Dependencies**: None

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | OB1 Close Oblibeny.lean sorry | L4 | [ ] Pending | — |
| 2 | OB2 Termination guarantee (Constrained Form) | L4 | [ ] Pending | — |
| 3 | OB3 Reversibility soundness | L4 | [ ] Pending | — |
| 4 | OB4 Post-quantum crypto integrity | I2 | [ ] Pending | — |

## Context

Dual-form language: Factory Form (Turing-complete, compile-time) vs Constrained Form (Turing-incomplete, runtime). Full reversibility, accountability, resource bounds. OCaml compiler, Zig FFI for liboqs post-quantum crypto.

## Theorems to Prove

### OB1: Close Oblibeny.lean sorry

**Target file**: `proofs/Oblibeny.lean` (EXISTING)
**Priority**: P0

**Obligations**:
- [ ] Identify and close 1 sorry

---

### OB2: Termination guarantee

**Target file**: `proofs/Termination.lean` (NEW)
**Priority**: P0

**Statement**: All Constrained Form programs terminate within resource bounds.

**Obligations**:
- [ ] Model Constrained Form (no general recursion)
- [ ] Prove structural recursion only
- [ ] Prove resource bounds respected

---

### OB3: Reversibility soundness

**Target file**: `proofs/Reversible.lean` (NEW)
**Priority**: P0

**Statement**: Every runtime operation has a unique inverse. Computation history is reconstructible from trace.

**Obligations**:
- [ ] Define trace type
- [ ] Prove each op has inverse
- [ ] Prove trace completeness

---

### OB4: Post-quantum crypto integrity

**Target file**: `verification/proofs/idris2/OBCrypto.idr`
**Priority**: P1

**Statement**: Zig FFI bindings to liboqs preserve cryptographic security properties.

**Obligations**:
- [ ] Model FFI boundary
- [ ] Prove no information leakage

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/oblibeny
lean proofs/*.lean
just proof-check-idris2
```

## Handoff Checklist

- [ ] OB1 sorry closed
- [ ] OB2-OB4 complete
- [ ] Commit: `proof: complete oblibeny proofs (4/4 theorems)`
