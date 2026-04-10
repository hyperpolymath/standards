# Proof Spec: the-nash-equilibrium
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/the-nash-equilibrium`
**Tier**: T2 — High (AGPL-3.0-or-later)
**Total Theorems**: 4
**Primary Prover(s)**: Idris2 (3), Lean4 (1)
**Existing Proof Coverage**: 3 Idris2 ABI files (Types, Layout, Foreign)

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | NE1 6X pillar balance invariant | I2 | [ ] Pending | — |
| 2 | NE2 Equilibrium existence (Nash) | L4 | [ ] Pending | — |
| 3 | NE3 Trade symmetric information | I2 | [ ] Pending | — |
| 4 | NE4 Game state transition validity | I2 | [ ] Pending | — |

## Context

6X strategy game (explore, expand, exploit, exterminate, exchange, experiment). Game theory-based. AGPL.

### Key files
- `src/interface/abi/Types.idr`
- `src/aspects/security/`
- `src/aspects/integrity/`
- `src/contracts/`
- `src/core/` (equilibrium solver)

## Theorems to Prove

### NE1: 6X pillar balance invariant

**Target**: `verification/proofs/idris2/PillarBalance.idr`
**Priority**: P1

**Obligations**:
- [ ] Define 6 pillars as types
- [ ] Prove no pillar dominates

---

### NE2: Equilibrium existence

**Target**: `verification/proofs/lean4/NashExistence.lean`
**Priority**: P1

**Statement**: For any game state, a Nash equilibrium exists.

**Obligations**:
- [ ] Model game as payoff matrix
- [ ] Cite Brouwer/Kakutani fixed point theorem

---

### NE3: Trade symmetric information

**Target**: `verification/proofs/idris2/TradeSym.idr`
**Priority**: P2

**Obligations**:
- [ ] Model trade
- [ ] Prove symmetric info

---

### NE4: State transition validity

**Target**: `verification/proofs/idris2/StateTransitions.idr`
**Priority**: P2

**Obligations**:
- [ ] Model valid transitions
- [ ] Prove reachability

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/the-nash-equilibrium
just proof-check-all
```

## Handoff Checklist

- [ ] All 4 theorems proven
- [ ] Commit: `proof: complete nash-equilibrium proofs (4/4)`
