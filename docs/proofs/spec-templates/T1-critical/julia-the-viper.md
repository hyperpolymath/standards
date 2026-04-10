# Proof Spec: julia-the-viper
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/nextgen-languages/julia-the-viper`
**Tier**: T1 — Critical
**Total Theorems**: 3 (most already complete)
**Primary Prover(s)**: Lean4
**Existing Proof Coverage**: 6 Lean files, 2260 LOC, **0 sorry** (mature)
**Dependencies**: None

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | JV1 Existing 6 Lean files recheck | L4 | [x] Done | verify |
| 2 | JV2 LLVM backend correctness | L4 | [ ] Pending | — |
| 3 | JV3 Cross-prover consistency (Lean vs Coq) | L4 | [ ] Pending | — |

## Context

Security-focused language. Effects tracking (I/O, state, crypto), information flow types (public/private/secret), capability-based security. MATURE: 6 Lean files with zero sorry.

### Existing files (DO NOT REDO)
- `jtv_proofs/JtvCore.lean` (165 LOC)
- `jtv_proofs/JtvExtended.lean` (426 LOC)
- `jtv_proofs/JtvOperational.lean` (416 LOC)
- `jtv_proofs/JtvSecurity.lean` (482 LOC)
- `jtv_proofs/JtvTheorems.lean` (413 LOC)
- `jtv_proofs/JtvTypes.lean` (358 LOC)

## Theorems to Prove

### JV1: Recheck existing proofs

**Target**: Existing files
**Priority**: P0

**Obligations**:
- [ ] Run `lake build` in jtv_proofs/ — all should pass
- [ ] Document that 0 sorry confirmed

---

### JV2: LLVM backend correctness

**Target file**: `jtv_proofs/LLVMBackend.lean` (NEW)
**Priority**: P1

**Statement**: LLVM code generation preserves source semantics AND security properties (info-flow labels propagated correctly).

**Obligations**:
- [ ] Model LLVM target semantics
- [ ] Prove codegen simulation + label preservation

---

### JV3: Cross-prover consistency

**Target file**: `jtv_proofs/CrossProver.lean` (NEW)
**Priority**: P2

**Statement**: If echidna validates JTV proofs in both Lean and Coq, both judgements agree.

**Obligations**:
- [ ] Export JTV proofs to Coq
- [ ] Prove semantic equivalence

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/nextgen-languages/julia-the-viper
lake build
```

## Handoff Checklist

- [ ] JV1: existing 6 files re-verified clean
- [ ] JV2, JV3 complete
- [ ] Commit: `proof: extend julia-the-viper proofs (3/3 theorems)`
