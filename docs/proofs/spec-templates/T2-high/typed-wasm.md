# Proof Spec: typed-wasm
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/typed-wasm`
**Tier**: T2 — High
**Total Theorems**: 6
**Primary Prover(s)**: Idris2 (all)
**Existing Proof Coverage**: 11 .idr files, 2,438 LOC (Region.idr, Levels.idr, TypedAccess.idr, Effects.idr, Linear.idr, Lifetime.idr, Proofs.idr)

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | TW1 L1-L10 progressive type safety | I2 | [ ] Pending | — |
| 2 | TW2 Region immutability post-declaration | I2 | [ ] Pending | — |
| 3 | TW3 Multi-module schema agreement | I2 | [ ] Pending | — |
| 4 | TW4 No escape hatches in L1-L10 | I2 | [ ] Pending | — |
| 5 | TW5 Linear handle consumption (L10) | I2 | [ ] Pending | — |
| 6 | TW6 Lifetime resource cleanup (L9) | I2 | [ ] Pending | — |

## Context

Progressive type safety for WebAssembly linear memory (12 levels). Adds region schemas and type-safe access ops. Implements hyperpolymath 10-level type safety hierarchy (L1-L10 checked, L11-L12 draft).

### Key files
- `src/abi/TypedWasm/ABI/Region.idr` (338 LOC)
- `src/abi/TypedWasm/ABI/Levels.idr` (317 LOC)
- `src/abi/TypedWasm/ABI/Proofs.idr`
- `src/abi/TypedWasm/ABI/TypedAccess.idr` (236 LOC)
- `src/abi/TypedWasm/ABI/Effects.idr` (247 LOC)
- `src/abi/TypedWasm/ABI/Linear.idr` (L10)
- `src/abi/TypedWasm/ABI/Lifetime.idr` (L9)

## Theorems to Prove

### TW1: L1-L10 progressive type safety

**Target**: `src/abi/TypedWasm/ABI/Proofs.idr` (EXTEND existing)
**Priority**: P0

**Statement**: Each level Ln depends on L1..L(n-1). Level skipping not possible. L1-L10 all type-safe.

**Obligations**:
- [ ] Prove safety theorem for each level
- [ ] Prove level dependency ordering

---

### TW2: Region immutability post-declaration

**Target**: `src/abi/TypedWasm/ABI/RegionImmut.idr` (NEW)
**Priority**: P0

**Statement**: Once a region schema is declared, it cannot be mutated.

**Obligations**:
- [ ] Model region lifecycle (Declared → Locked)
- [ ] Prove no transition back to mutable

---

### TW3: Multi-module schema agreement

**Target**: `src/abi/TypedWasm/ABI/SchemaAgree.idr` (NEW)
**Priority**: P0

**Statement**: Rust and ReScript modules importing same region must see identical definition.

**Obligations**:
- [ ] Model cross-module region import
- [ ] Prove definitional equality

---

### TW4: No escape hatches in L1-L10

**Target**: `verification/proofs/idris2/NoEscape.idr` (NEW)
**Priority**: P0

**Statement**: Zero `believe_me`, `assert_total`, `assert_impossible` in L1-L10 proof modules.

**Obligations**:
- [ ] Scan L1-L10 modules
- [ ] Document zero matches

---

### TW5: Linear handle consumption (L10)

**Target**: `src/abi/TypedWasm/ABI/Linear.idr` (EXTEND)
**Priority**: P1

**Statement**: Each linear handle consumed exactly once.

**Obligations**:
- [ ] Model linear context
- [ ] Prove use count = 1

---

### TW6: Lifetime resource cleanup (L9)

**Target**: `src/abi/TypedWasm/ABI/Lifetime.idr` (EXTEND)
**Priority**: P1

**Statement**: Resources freed at end of lifetime scope. No leaks.

**Obligations**:
- [ ] Model lifetime scopes
- [ ] Prove cleanup invariant

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/typed-wasm
for f in src/abi/TypedWasm/ABI/*.idr; do idris2 --check "$f"; done
```

## Handoff Checklist

- [ ] All 6 theorems proven
- [ ] Zero escape hatches confirmed
- [ ] Commit: `proof: complete typed-wasm L1-L10 verification (6/6)`
