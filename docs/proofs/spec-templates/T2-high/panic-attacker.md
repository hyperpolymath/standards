# Proof Spec: panic-attacker
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/panic-attacker`
**Tier**: T2 — High
**Total Theorems**: 4
**Primary Prover(s)**: Idris2 (3), Coq/V (1)
**Existing Proof Coverage**: 3 Idris2 spec files, 1 Coq taint-track soundness proof

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | PA1 Pattern detection completeness (47 langs, 20 categories, cross-lang) | I2 | [x] Done | 2026-04-11 |
| 2 | PA2 Classification soundness (severity total order, monotone aggregation) | I2 | [x] Done | 2026-04-11 |
| 3 | PA3 CVE phantom-dep classification | I2 | [ ] Pending | — |
| 4 | PA4 Attestation chain unforgeability | Cq | [ ] Pending | — |

**Notes:**
- PA1 proved in `src/abi/PatternCompleteness.idr` — `analyzerFor` covers all 47 Lang constructors, `detectorsFor` covers all 20 WPCategory constructors, `completeScanForAll` combines both.
- PA2 proved in `src/abi/ClassificationSoundness.idr` — Severity is a total order, `maxSeverity` is monotone, numeric encoding preserves ordering.

## Context

Static analysis + logic-based vulnerability detection across 47 languages. miniKanren relational engine, CVE lifecycle bridge. 20 weak-point categories.

### Key files
- `src/assail/analyzer.rs`
- `src/assail/patterns.rs`
- `src/kanren/core.rs`
- `src/kanren/taint.rs`
- `src/bridge/reachability.rs`

## Theorems to Prove

### PA1: Pattern detection completeness

**Target**: `verification/proofs/idris2/PatternComplete.idr`
**Priority**: P0

**Statement**: For the 20 weak-point categories, the detection function covers every defined pattern.

**Obligations**:
- [ ] Enumerate 20 categories
- [ ] Prove detector handles each

---

### PA2: miniKanren rule enumeration

**Target**: `verification/proofs/idris2/KanrenComplete.idr`
**Priority**: P0

**Statement**: Forward-chaining engine enumerates all derivable vulnerabilities (no missed rules).

**Obligations**:
- [ ] Model rule set
- [ ] Prove completeness

---

### PA3: CVE phantom-dep classification

**Target**: `verification/proofs/idris2/PhantomDep.idr`
**Priority**: P1

**Statement**: Crates in Cargo.lock but not imported are correctly classified as Informational.

**Obligations**:
- [ ] Model dep resolution
- [ ] Prove classification correctness

---

### PA4: Attestation chain unforgeability

**Target**: `verification/proofs/coq/AttestationChain.v`
**Priority**: P1

**Statement**: Intent/evidence/seal triple cryptographically bound (Ed25519). Tampering detectable.

**Obligations**:
- [ ] Model attestation structure
- [ ] Prove Ed25519 binding

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/panic-attacker
just proof-check-all
```

## Handoff Checklist

- [ ] All 4 theorems proven
- [ ] Commit: `proof: complete panic-attacker proofs (4/4)`
