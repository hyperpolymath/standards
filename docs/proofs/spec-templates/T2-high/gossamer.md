# Proof Spec: gossamer
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/gossamer`
**Tier**: T2 — High
**Total Theorems**: 9 (7 done, 1 blocked, 1 pending)
**Primary Prover(s)**: Idris2 (all)
**Existing Proof Coverage**: 11 proof modules in `src/interface/abi/`

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | GS1-base Handle linearity (exactly-once use) | I2 | [x] Done | 2026-04-11 |
| 2 | GS2-base Capability authenticity (cannot forge) | I2 | [x] Done | 2026-04-11 |
| 3 | GS-IPC IPC message integrity (hash/seq/protocol) | I2 | [x] Done | 2026-04-11 |
| 4 | GS-GRV Groove handshake termination | I2 | [x] Done | 2026-04-11 |
| 5 | GS-PNL Panel isolation (state/channel/registry) | I2 | [x] Done | 2026-04-11 |
| 6 | GS-LAY Memory layout ABI stability | I2 | [x] Done | 2026-04-11 |
| 7 | GS-RES Resource cleanup on teardown (LIFO) | I2 | [x] Done | 2026-04-11 |
| 8 | GS1 Window state machine correctness (INV) | I2 | [x] Done | 2026-04-11 |
| 9 | GS2 IPC handler type safety (25 handlers TP) | I2 | [x] Done | 2026-04-11 |
| 10 | GS3 Groove protocol compliance | I2 | [ ] Pending | — |
| — | Extension loading safety | I2 | [ ] BLOCKED | Ephapax module system |

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
