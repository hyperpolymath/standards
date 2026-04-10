# Proof Spec: gossamer
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/gossamer`
**Tier**: T2 — High
**Total Theorems**: 5 (3 already specified)
**Primary Prover(s)**: Idris2 (all)
**Existing Proof Coverage**: 8 proof modules (~2K LOC): Groove.idr, HandleLinearity.idr, CapabilityAuthenticity.idr, IPCIntegrity.idr, PanelIsolation.idr

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | GS1 Handle linearity (exactly-once use) | I2 | [x] Specified | — |
| 2 | GS2 Capability authenticity (cannot forge) | I2 | [x] Specified | — |
| 3 | GS3 IPC thread-safety | I2 | [ ] Pending | — |
| 4 | GS4 Panel isolation | I2 | [x] Specified | — |
| 5 | GS5 Platform-agnostic ABI stability | I2 | [ ] Pending | — |

## Context

Cross-platform desktop runtime (Windows, macOS, Linux, Android, iOS). WebKit integration, async IPC, capability-based security. Replaces Tauri 2.0.

### Existing proofs (verify these compile)
- `src/interface/abi/Groove.idr` (435 LOC)
- `src/interface/abi/HandleLinearity.idr` (282 LOC)
- `src/interface/abi/CapabilityAuthenticity.idr` (230 LOC)
- `src/interface/abi/IPCIntegrity.idr` (211 LOC)
- `src/interface/abi/PanelIsolation.idr` (225 LOC)

## Theorems to Prove

### GS1/GS2/GS4: Verify existing proofs compile

**Target**: Existing .idr files
**Priority**: P0

**Obligations**:
- [ ] `idris2 --check` each existing file
- [ ] Confirm zero believe_me
- [ ] Document any compile errors

---

### GS3: IPC thread-safety

**Target**: `src/interface/abi/IPCThreadSafety.idr` (NEW)
**Priority**: P1

**Statement**: All IPC callbacks serialized. No race conditions.

**Obligations**:
- [ ] Model concurrent IPC queue
- [ ] Prove serialisation

---

### GS5: Platform-agnostic ABI stability

**Target**: `src/interface/abi/ABIStable.idr` (NEW)
**Priority**: P2

**Statement**: Zig ABI is identical across Windows/GTK/Cocoa/WebKit.

**Obligations**:
- [ ] Model ABI types
- [ ] Prove platform invariance

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/gossamer
for f in src/interface/abi/*.idr; do idris2 --check "$f"; done
```

## Handoff Checklist

- [ ] All 5 proven
- [ ] Commit: `proof: complete gossamer proofs (5/5)`
