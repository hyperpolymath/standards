# Proof Spec: svalinn
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/svalinn`
**Tier**: T1 — Critical
**Total Theorems**: 5
**Primary Prover(s)**: Idris2 (3), TLA+ (2)
**Existing Proof Coverage**: 0 (currently JSON Schema validation only)
**Dependencies**: `proven`

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | SV1 Request validation no-bypass | I2 | [ ] Pending | — |
| 2 | SV2 JWT verification before delegation | I2 | [ ] Pending | — |
| 3 | SV3 Policy rule evaluation ordering | TLA | [ ] Pending | — |
| 4 | SV4 Vörðr delegation atomicity | TLA | [ ] Pending | — |
| 5 | SV5 Compose output determinism | I2 | [ ] Pending | — |

## Context

Post-cloud edge gateway. HTTP gateway (Hono/Deno) validates requests against verified-container-spec, delegates to Vörðr via MCP/JSON-RPC. OAuth2/JWT auth, Compose-compatible orchestration.

### Key files
- `src/gateway/GatewayServer.res` — route dispatch
- `src/auth/AuthMiddleware.res` — JWT validation
- `src/compose/ComposeOrchestrator.res` — docker-compose CLI
- `spec/schemas/gatekeeper-policy.v1.json` — policy schema
- `spec/schemas/verified-container-spec.json` — request schema

## Theorems to Prove

### SV1: Request validation no-bypass

**Target file**: `verification/proofs/idris2/NoBypass.idr`
**Priority**: P0

**Statement**:
> No request reaches Vörðr delegation without passing verified-container-spec validation.

**Obligations**:
- [ ] Model request pipeline as indexed type
- [ ] Prove validation is mandatory step

---

### SV2: JWT verification before delegation

**Target file**: `verification/proofs/idris2/JWTFirst.idr`
**Priority**: P0

**Statement**:
> JWT token signature + expiry verified before any delegation. Clock skew ≤ 60s.

**Obligations**:
- [ ] Model request states (Unauthenticated → Authenticated → Delegated)
- [ ] Prove transitions require verification

---

### SV3: Policy rule evaluation ordering

**Target file**: `verification/proofs/tlaplus/PolicyOrder.tla`
**Priority**: P1

**Statement**:
> Policy rules evaluated in fixed order (deny-first). No race conditions between deny/allow.

**Obligations**:
- [ ] Model policy engine
- [ ] Model-check ordering property

---

### SV4: Vörðr delegation atomicity

**Target file**: `verification/proofs/tlaplus/DelegationAtomic.tla`
**Priority**: P1

**Statement**:
> Delegation is atomic: either full request forwarded, or none.

**Obligations**:
- [ ] Model delegation steps
- [ ] Model-check atomicity

---

### SV5: Compose output determinism

**Target file**: `verification/proofs/idris2/ComposeDet.idr`
**Priority**: P2

**Statement**:
> Given the same input spec, compose output is identical (field ordering, whitespace, etc.).

**Obligations**:
- [ ] Define Compose output as deterministic function
- [ ] Prove equal inputs → equal outputs

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/svalinn
just proof-check-all
```

## Handoff Checklist

- [ ] All 5 theorems proven
- [ ] Commit: `proof: complete svalinn proofs (5/5 theorems)`
