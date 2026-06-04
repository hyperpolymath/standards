# Proof Spec: cloudguard-cli
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/cloudguard-cli`
**Tier**: T2 — High
**Total Theorems**: 4
**Primary Prover(s)**: Idris2 (all)
**Existing Proof Coverage**: tests/smoke_test.rs + FFI integration tests

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | CGC1 API token redaction completeness | I2 | [ ] Pending | — |
| 2 | CGC2 Dry-run equals apply minus side-effects | I2 | [ ] Pending | — |
| 3 | CGC3 DNS operation idempotence | I2 | [ ] Pending | — |
| 4 | CGC4 Config diff correctness | I2 | [ ] Pending | — |

## Context

Cloudflare domain security CLI. Audit + harden. DNS records, Pages projects.

### Key files
- `src/main.rs` (1,392 LOC)
- `src/api/` (Cloudflare client)
- `src/abi/` (FFI)

## Theorems to Prove

### CGC1: API token redaction

**Target**: `verification/proofs/idris2/TokenRedact.idr`
**Priority**: P0

**Statement**: API token never appears in logs or serialised output.

**Obligations**:
- [ ] Model output paths
- [ ] Prove redaction invariant

---

### CGC2: Dry-run correctness

**Target**: `verification/proofs/idris2/DryRunEq.idr`
**Priority**: P0

**Statement**: Dry-run produces same output as apply, minus state changes.

**Obligations**:
- [ ] Model both modes
- [ ] Prove output equivalence

---

### CGC3: DNS operation idempotence

**Target**: `verification/proofs/idris2/DNSIdem.idr`
**Priority**: P1

**Statement**: Re-running harden is safe.

**Obligations**:
- [ ] Model harden op
- [ ] Prove idempotence

---

### CGC4: Config diff correctness

**Target**: `verification/proofs/idris2/ConfigDiff.idr`
**Priority**: P2

**Statement**: Diff tracks all live-vs-policy changes.

**Obligations**:
- [ ] Model diff algorithm
- [ ] Prove completeness

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/cloudguard-cli
just proof-check-all
```

## Handoff Checklist

- [ ] All 4 theorems proven
- [ ] Commit: `proof: complete cloudguard-cli proofs (4/4)`
