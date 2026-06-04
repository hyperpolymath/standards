# Proof Spec: cloudguard-server
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/cloudguard-server`
**Tier**: T2 — High
**Total Theorems**: 4
**Primary Prover(s)**: Idris2 (3), TLA+ (1)
**Existing Proof Coverage**: tests/ stubs, FFI integration tests

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | CGS1 API key enforcement on protected routes | I2 | [ ] Pending | — |
| 2 | CGS2 WebSocket ops cancellable + idempotent | I2 | [ ] Pending | — |
| 3 | CGS3 Token never in response headers | I2 | [ ] Pending | — |
| 4 | CGS4 Concurrent same-zone requests serialised | TLA | [ ] Pending | — |

## Context

REST + WebSocket API wrapping CloudGuard. Axum framework, stateless. Optional API key auth.

### Key files
- `src/main.rs` (946 LOC)
- `src/api/`
- `src/auth.rs`
- `.conflow.yaml`

## Theorems to Prove

### CGS1: API key enforcement

**Target**: `verification/proofs/idris2/APIKeyEnforce.idr`
**Priority**: P0

**Statement**: `/api/*` and `/ws/*` require key (when set). `/health` exempt.

**Obligations**:
- [ ] Model route table
- [ ] Prove auth check by route class

---

### CGS2: WebSocket cancellable + idempotent

**Target**: `verification/proofs/idris2/WSOps.idr`
**Priority**: P1

**Statement**: Bulk ops cancellable, re-runnable safely.

**Obligations**:
- [ ] Model op state machine
- [ ] Prove cancel + idempotent

---

### CGS3: Token never in headers

**Target**: `verification/proofs/idris2/TokenOut.idr`
**Priority**: P0

**Statement**: Cloudflare API token never leaks to response.

**Obligations**:
- [ ] Model response construction
- [ ] Prove redaction

---

### CGS4: Zone request serialisation

**Target**: `verification/proofs/tlaplus/ZoneLock.tla`
**Priority**: P1

**Statement**: Concurrent requests to same zone do not race.

**Obligations**:
- [ ] Model zone lock
- [ ] Model-check mutex

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/cloudguard-server
just proof-check-all
```

## Handoff Checklist

- [ ] All 4 theorems proven
- [ ] Commit: `proof: complete cloudguard-server proofs (4/4)`
