# Proof Spec: panll
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/panll`
**Tier**: T2 — High
**Total Theorems**: 5
**Primary Prover(s)**: Idris2 (4), Lean4 (1)
**Existing Proof Coverage**: 3 .idr stubs (~60 LOC)

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | PN1 Custom TEA runtime invariants | I2 | [ ] Pending | — |
| 2 | PN2 Binary Star model separation | I2 | [ ] Pending | — |
| 3 | PN3 AntiCrash validation completeness | I2 | [ ] Pending | — |
| 4 | PN4 OrbitalSync panel coherence | I2 | [ ] Pending | — |
| 5 | PN5 Panel state monotonicity | L4 | [ ] Pending | — |

## Context

Panel management: 108 panels, ~686 modules. Binary Star model (Panel-L symbolic + Panel-N neural + Panel-W world). Custom TEA runtime, Gossamer backend, Idris2 TypeLL integration.

### Key files
- `src/Model.res` — single state tree
- `src/Update.res` — state transitions (~7,500 lines)
- `src/tea/Tea_App.res` — custom TEA runtime (18 modules)
- `src/core/AntiCrash.res` — validates neural tokens
- `src/core/OrbitalSync.res` — multi-panel coherence

## Theorems to Prove

### PN1: Custom TEA runtime invariants

**Target**: `verification/proofs/idris2/TEARuntime.idr`
**Priority**: P0

**Statement**: State transitions deterministic. Update function pure.

**Obligations**:
- [ ] Model TEA state machine
- [ ] Prove determinism + purity

---

### PN2: Binary Star model separation

**Target**: `verification/proofs/idris2/BinaryStar.idr`
**Priority**: P0

**Statement**: Panel-L, Panel-N, Panel-W state never mixed. Strict separation preserved.

**Obligations**:
- [ ] Define three panel types
- [ ] Prove no cross-contamination

---

### PN3: AntiCrash validation

**Target**: `verification/proofs/idris2/AntiCrash.idr`
**Priority**: P0

**Statement**: ALL neural tokens validated before reaching symbolic state.

**Obligations**:
- [ ] Model neural → symbolic flow
- [ ] Prove validation mandatory

---

### PN4: OrbitalSync panel coherence

**Target**: `verification/proofs/idris2/OrbitalSync.idr`
**Priority**: P1

**Statement**: 108 panels maintain consistent shared state.

**Obligations**:
- [ ] Model panel cluster state
- [ ] Prove coherence invariant

---

### PN5: Panel state monotonicity

**Target**: `verification/proofs/lean4/PanelMonotonic.lean`
**Priority**: P2

**Statement**: Panel version numbers strictly increase.

**Obligations**:
- [ ] Model version counter
- [ ] Prove monotonicity

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/panll
just proof-check-all
```

## Handoff Checklist

- [ ] All 5 theorems proven
- [ ] Commit: `proof: complete panll proofs (5/5)`
