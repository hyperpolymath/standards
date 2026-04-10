# Proof Spec: reposystem
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/reposystem`
**Tier**: T2 — High
**Total Theorems**: 5
**Primary Prover(s)**: Idris2 (2), V (2), Lean4 (1)
**Existing Proof Coverage**: 1 Idris2 ABI, 22 V validators (GitHub ops, guards, validation)

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | RS1 Slot/provider cardinality | I2 | [ ] Pending | — |
| 2 | RS2 Plan/apply transactional rollback | TLA | [ ] Pending | — |
| 3 | RS3 VeriSimDB state persistence | I2 | [ ] Pending | — |
| 4 | RS4 Aspect tag access control | L4 | [ ] Pending | — |
| 5 | RS5 ER cardinality validation | V | [ ] Pending | — |

## Context

Railway-yard TUI for multi-repo component management. Visual wiring, aspect tagging, slot/provider registry, graph planning with rollback. Rust/SPARK (not Ada/SPARK).

### Key files
- `src/graph.rs`
- `src/commands/apply.rs`
- `scaffoldia/repo-batcher/src/v/safety/guards.v`
- `src/scanner.rs`
- `src/tui.rs`

## Theorems to Prove

### RS1: Slot/provider cardinality

**Target**: `verification/proofs/idris2/SlotCard.idr`
**Priority**: P0

**Statement**: 1-to-many cardinality validated.

**Obligations**:
- [ ] Model slot/provider registry
- [ ] Prove cardinality constraint

---

### RS2: Transactional rollback

**Target**: `verification/proofs/tlaplus/ApplyTxn.tla`
**Priority**: P0

**Statement**: Plan + apply is transactional. Failure → full rollback.

**Obligations**:
- [ ] Model apply pipeline
- [ ] Model-check atomicity

---

### RS3: VeriSimDB state persistence

**Target**: `verification/proofs/idris2/StatePersist.idr`
**Priority**: P1

**Statement**: State persists across restarts via VeriSimDB (or JSON fallback).

**Obligations**:
- [ ] Model save/load
- [ ] Prove roundtrip

---

### RS4: Aspect tag access control

**Target**: `verification/proofs/lean4/AspectACL.lean`
**Priority**: P1

**Statement**: Aspect tags enforce cross-module visibility.

**Obligations**:
- [ ] Model aspects
- [ ] Prove access control soundness

---

### RS5: ER cardinality validation

**Target**: `scaffoldia/repo-batcher/src/v/safety/er_cardinality.v` (NEW)
**Priority**: P2

**Statement**: Crow's-foot notation cardinalities validated before execution.

**Obligations**:
- [ ] Formalise cardinality checks
- [ ] Prove validation

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/reposystem
just proof-check-all
```

## Handoff Checklist

- [ ] All 5 theorems proven
- [ ] Commit: `proof: complete reposystem proofs (5/5)`
