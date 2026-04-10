# Proof Spec: idaptik
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/idaptik`
**Tier**: T2 — High (AGPL-3.0-or-later, co-developed)
**Total Theorems**: 5
**Primary Prover(s)**: Idris2 (4), Coq/V (1)
**Existing Proof Coverage**: 17 Idris2 ABI modules (0 believe_me), 1 Coq/V validation

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | ID1 Verify 17 ABI modules zero believe_me | I2 | [ ] Pending | — |
| 2 | ID2 Game engine determinism | I2 | [ ] Pending | — |
| 3 | ID3 Multiplayer sync consistency | I2 | [ ] Pending | — |
| 4 | ID4 Reversible VM state determinism | I2 | [ ] Pending | — |
| 5 | ID5 API surface validation | Cq | [ ] Pending | — |

## Context

Asymmetric co-op stealth puzzle-platformer. Browser-based hacking sim. ReScript+PixiJS frontend, Elixir/Phoenix sync, pure reversible VM, Idris2 ABI.

### Key files
- `idaptik-ums/src/abi/Assassin.idr`, `GameSystems.idr`, `Multiplayer.idr`, `Validation.idr`
- `src/abi/Types.idr`
- `api/v/idaptik.v`
- `src/GameEngine.res`
- `src/Network.res`

## Theorems to Prove

### ID1: Verify 17 ABI modules

**Target**: Existing files
**Priority**: P0

**Obligations**:
- [ ] Run `idris2 --check` on all 17 modules
- [ ] Confirm 0 believe_me via grep
- [ ] Document results

---

### ID2: Game engine determinism

**Target**: `verification/proofs/idris2/GameDet.idr`
**Priority**: P0

**Statement**: Game state transitions are deterministic given inputs.

**Obligations**:
- [ ] Model game state + ops
- [ ] Prove determinism

---

### ID3: Multiplayer sync consistency

**Target**: `verification/proofs/idris2/MultiplayerSync.idr`
**Priority**: P0

**Statement**: All clients converge to same game state.

**Obligations**:
- [ ] Model distributed state
- [ ] Prove convergence

---

### ID4: Reversible VM determinism

**Target**: `verification/proofs/idris2/ReversibleVM.idr`
**Priority**: P1

**Statement**: VM state deterministic across all transitions. Each op has inverse.

**Obligations**:
- [ ] Model VM
- [ ] Prove inversibility

---

### ID5: API surface validation

**Target**: `api/v/idaptik.v` (EXTEND)
**Priority**: P2

**Statement**: All API endpoints validate inputs before processing.

**Obligations**:
- [ ] Enumerate endpoints
- [ ] Prove validation completeness

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/idaptik
for f in idaptik-ums/src/abi/*.idr src/abi/*.idr; do idris2 --check "$f"; done
```

## Handoff Checklist

- [ ] All 5 theorems verified
- [ ] Commit: `proof: complete idaptik proofs (5/5)`
