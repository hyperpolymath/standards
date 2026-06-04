# Proof Spec: protocol-squisher
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/protocol-squisher`
**Tier**: T1 — Critical
**Total Theorems**: 8 (3 already done)
**Primary Prover(s)**: Agda (all)
**Existing Proof Coverage**: 5 theorems proven: ConcordeSafety, CarriesInvariant (basic), ContainerPropagation, OptimizationSoundness, WheelbarrowNecessity
**Dependencies**: `proven`

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | PQ1 CarriesInvariant for all 13 analyzers | Ag | [x] Done | 2026-04-11 |
| 2 | PQ2 Transport class soundness | Ag | [x] Done | 2026-XX |
| 3 | PQ3 Optimizer preserves CarriesInvariant | Ag | [x] Done | 2026-XX |
| 4 | PQ4 Adapter synthesis correctness | Ag | [x] Done | 2026-04-11 |
| 5 | PQ5 Concorde bidirectional losslessness | Ag | [x] Done | 2026-XX |
| 6 | PQ6 Business class loss documentation | Ag | [x] Done | 2026-04-11 |
| 7 | PQ7 All 29 crates unwrap-free | I2 | [x] Done | 2026-04-11 |
| 8 | PQ8 Buffer overflow freedom | I2 | [x] Done | 2026-04-11 |

## Context

Universal protocol interoperability: analyses 13 protocol formats (Rust, Python, JSON Schema, Protobuf, Bebop, FlatBuffers, MessagePack, Avro, Cap'n Proto, Thrift, ReScript, GraphQL, TOML), synthesises adapters, classifies transport capacity (Concorde/Business/Economy/Wheelbarrow).

### Key files
- `proofs/agda/CarriesInvariant.agda` (9.1K)
- `proofs/agda/ConcordeSafety.agda`
- `crates/protocol-squisher-optimizer/src/`
- 29 Rust crates total

### Existing proofs (DO NOT REDO)
- ConcordeSafety
- CarriesInvariant (base cases)
- ContainerPropagation
- OptimizationSoundness
- WheelbarrowNecessity

## Theorems to Prove

### PQ1: CarriesInvariant for ALL 13 analyzers

**Target file**: `verification/proofs/agda/CarriesInvariantExtended.agda`
**Priority**: P0

**Statement**:
> All 13 analyzers (Rust, Python, JSONSchema, Protobuf, Bebop, FlatBuffers, MessagePack, Avro, CapnProto, Thrift, ReScript, GraphQL, TOML) satisfy CarriesInvariant: all data entering adapter exits (no silent drops).

**Obligations**:
- [ ] Extend existing CarriesInvariant to all 13 formats
- [ ] Document any necessary losses (Business class)

---

### PQ4: Adapter synthesis correctness

**Target file**: `verification/proofs/agda/AdapterSynthesis.agda`
**Priority**: P1

**Statement**:
> Synthesized adapter correctly translates source format to target format preserving all required fields.

**Obligations**:
- [ ] Formalise adapter specification
- [ ] Prove synthesized adapter matches spec

---

### PQ6: Business class loss documentation

**Target file**: `verification/proofs/agda/BusinessClassLoss.agda`
**Priority**: P2

**Statement**:
> Every Business class translation has documented, provable loss (e.g., Int64 → Int32 truncates to 32 bits).

**Obligations**:
- [ ] Enumerate all Business class translations
- [ ] Prove loss bound for each

---

### PQ7: 21 remaining crates unwrap-free

**Target file**: `verification/proofs/idris2/NoPanics.idr`
**Priority**: P1

**Statement**:
> No `.unwrap()` or `.expect()` calls in 21 remaining Rust crates (8 already done).

**Obligations**:
- [ ] Audit all 21 crates
- [ ] Replace unwraps with explicit Result handling
- [ ] Document audit results

---

### PQ8: Buffer overflow freedom

**Target file**: `verification/proofs/idris2/BufferSafety.idr`
**Priority**: P1

**Statement**:
> All byte-level operations check bounds. No buffer overflow possible.

**Obligations**:
- [ ] Identify byte manipulation points
- [ ] Prove bounds checks present

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/protocol-squisher
just proof-check-agda
just proof-check-idris2
```

## Handoff Checklist

- [x] All 8 theorems (5 existing + 3 new) verified — 2026-04-11
- [x] All 29 crates unwrap-free (commit 4231afb, 2026-02-04)
- [ ] Commit: `proof: complete protocol-squisher proofs (8/8 theorems)`
