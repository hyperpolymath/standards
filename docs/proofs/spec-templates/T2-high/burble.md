# Proof Spec: burble
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/burble`
**Tier**: T2 — High
**Total Theorems**: 5
**Primary Prover(s)**: Idris2 (3), TLA+ (2)
**Existing Proof Coverage**: verification/ scaffolding + e2e tests; no formal proofs yet

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | BU1 DataChannel JSON schema validation | I2 | [ ] Pending | — |
| 2 | BU2 Token auth before room join | I2 | [ ] Pending | — |
| 3 | BU3 Participant consistency (bidirectional) | TLA | [ ] Pending | — |
| 4 | BU4 Message ordering (TCP-ordered) | TLA | [ ] Pending | — |
| 5 | BU5 Audio/data channel isolation | I2 | [ ] Pending | — |

## Context

P2P encrypted voice chat (WebRTC) with bidirectional AI agent data channel. Enables two Claude Code instances to exchange JSON over same connection as voice.

### Key files
- `server/lib/burble/rooms/room_manager.ex`
- `server/lib/burble/auth/guardian.ex`
- `client/web/burble-ai-bridge.js`
- `server/test/burble/e2e/signaling_test.exs`
- `src/interface/ffi/test/integration_test.zig`

## Theorems to Prove

### BU1: DataChannel JSON schema

**Target**: `verification/proofs/idris2/DataChannelSchema.idr`
**Priority**: P0

**Statement**: All messages validated against JSON schema before routing.

**Obligations**:
- [ ] Define message schema
- [ ] Prove validator invariant

---

### BU2: Token auth before room join

**Target**: `verification/proofs/idris2/JoinAuth.idr`
**Priority**: P0

**Statement**: Room join requires verified token.

**Obligations**:
- [ ] Model room state machine
- [ ] Prove auth precondition

---

### BU3: Participant consistency

**Target**: `verification/proofs/tlaplus/Participants.tla`
**Priority**: P1

**Statement**: Only bidirectional connections in participant list. No stale entries.

**Obligations**:
- [ ] Model room + connections
- [ ] Model-check consistency

---

### BU4: Message ordering

**Target**: `verification/proofs/tlaplus/MsgOrder.tla`
**Priority**: P1

**Statement**: Messages within a peer stream preserve order.

**Obligations**:
- [ ] Model stream
- [ ] Model-check FIFO

---

### BU5: Channel isolation

**Target**: `verification/proofs/idris2/ChannelIsolation.idr`
**Priority**: P2

**Statement**: Audio packets never leak into data channel.

**Obligations**:
- [ ] Model two channels
- [ ] Prove disjoint

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/burble
just proof-check-all
```

## Handoff Checklist

- [ ] All 5 theorems proven
- [ ] Commit: `proof: complete burble proofs (5/5)`
