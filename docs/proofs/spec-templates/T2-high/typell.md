# Proof Spec: typell
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/typell`
**Tier**: T2 — High (foundation for L1-L10 hierarchy)
**Total Theorems**: 5
**Primary Prover(s)**: Idris2 (all)
**Existing Proof Coverage**: 6 .idr files (~300 LOC)

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | TL1 Unification termination (occurs check) | I2 | [ ] Pending | — |
| 2 | TL2 Substitution idempotence | I2 | [ ] Pending | — |
| 3 | TL3 Inference soundness (principal type) | I2 | [ ] Pending | — |
| 4 | TL4 Level progression ordering | I2 | [ ] Pending | — |
| 5 | TL5 Existential type sealing | I2 | [ ] Pending | — |

## Context

Foundation for progressive type safety. Powers L1-L10 proofs in typed-wasm, panll TypeLLEngine, gossamer capabilities. Robinson unification, HM inference.

### Key files
- `crates/typell-core/src/types.rs` (1,163 LOC)
- `crates/typell-core/src/unify.rs` (961 LOC)
- `crates/typell-core/src/infer.rs` (459 LOC)
- `src/abi/TypeLL/Proofs.idr`

## Theorems to Prove

### TL1: Unification termination

**Target**: `src/abi/TypeLL/UnifyTermination.idr` (NEW)
**Priority**: P0

**Statement**: Occurs check prevents infinite types. Unification always terminates.

**Obligations**:
- [ ] Model unification algorithm
- [ ] Prove termination via well-founded recursion

---

### TL2: Substitution idempotence

**Target**: `src/abi/TypeLL/SubstIdem.idr` (NEW)
**Priority**: P0

**Statement**: Applying substitution twice equals applying once.

**Obligations**:
- [ ] Define substitution
- [ ] Prove `apply s (apply s t) = apply s t`

---

### TL3: Inference soundness (principal type)

**Target**: `src/abi/TypeLL/InferSound.idr` (NEW)
**Priority**: P0

**Statement**: Inferred type is most general (principal). No overfitting.

**Obligations**:
- [ ] Define principal type
- [ ] Prove inferred = principal

---

### TL4: Level progression ordering

**Target**: `src/abi/TypeLL/LevelOrder.idr` (NEW)
**Priority**: P1

**Statement**: Type checking phase precedes constraint solving.

**Obligations**:
- [ ] Model phases
- [ ] Prove ordering

---

### TL5: Existential type sealing

**Target**: `src/abi/TypeLL/ExistSeal.idr` (NEW)
**Priority**: P2

**Statement**: Existential types sealed before returning to user code.

**Obligations**:
- [ ] Model existential packing
- [ ] Prove no leakage

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/typell
just proof-check-idris2
```

## Handoff Checklist

- [ ] All 5 theorems proven
- [ ] Commit: `proof: complete typell foundations (5/5)`
