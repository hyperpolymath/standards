# Archetype: security (Security/Crypto Tool)
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Applies to**: Security tools, crypto wrappers, auth, access control

## Common Proofs

### SEC-1: Key never exposed

**Target**: `verification/proofs/idris2/KeyPrivacy.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: Private keys never appear in logs, outputs, or error messages.

**Obligations**:
- [ ] Define KeyMaterial opaque type
- [ ] Prove no debug/print paths expose

---

### SEC-2: Access control no-bypass

**Target**: `verification/proofs/idris2/AccessControl.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: Protected operations require valid credentials.

**Obligations**:
- [ ] Model auth state
- [ ] Prove auth precondition

---

### SEC-3: Injection-free

**Target**: `verification/proofs/idris2/NoInjection.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: User input sanitised; no SQL/shell/path injection possible.

**Obligations**:
- [ ] Define Sanitised refinement type
- [ ] Prove all dangerous sinks require Sanitised

---

### SEC-4: Constant-time comparison

**Target**: `verification/proofs/idris2/ConstTime.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**: Token/password comparison is constant time.

**Obligations**:
- [ ] Model step count
- [ ] Prove count depends only on length

---

## Plus: Mandatory ABI proofs

ABI-1 through ABI-5.

## Repos using this archetype

safe-brute-force, sanctify-php, php-aegis, pimcore-fortress, reasonably-good-token-vault, explicit-trust-plane, sdp-hkdf-deployment, befunge93-vault-cracker, dicti0nary-attack, cookie-rebound, http-capability-gateway, proof-of-work
