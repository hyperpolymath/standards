# Proof Spec: verisimdb
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/verisimdb`
**Tier**: T1 — Critical
**Total Theorems**: 12
**Primary Prover(s)**: Idris2 (4), Lean4 (4), Agda (2), TLA+ (2)
**Existing Proof Coverage**: ~15% (Idris2 ABI 1748 LOC, ReScript VCL 11 proof types)
**Dependencies**: `proven` library, `rsr-template-repo` ABI proofs

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | V1 Octad coherence invariant | I2 | [ ] Pending | — |
| 2 | V2 VCL type inference soundness | L4 | [ ] Pending | — |
| 3 | V3 VCL subtyping transitivity | L4 | [ ] Pending | — |
| 4 | V4 Raft consensus safety | L4 | [ ] Pending | — |
| 5 | V5 Transaction atomicity | TLA | [ ] Pending | — |
| 6 | V6 WAL integrity + replay idempotence | L4 | [ ] Pending | — |
| 7 | V7 Provenance chain immutability | Ag | [x] Done 2026-04-11 | `verisimdb/verification/proofs/agda/ProvenanceChain.agda` |
| 8 | V8 Drift metric correctness | I2 | [ ] Pending | — |
| 9 | V9 Normalizer determinism | TLA | [ ] Pending | — |
| 10 | V10 Transaction serializability | TLA | [ ] Pending | — |
| 11 | V11 Connector type safety (no Obj.magic) | I2 | [ ] Pending | — |
| 12 | V12 FFI pointer validity + ownership | I2 | [ ] Pending | — |

## Context

### What this repo does

VeriSimDB is a cross-modal consistency engine for entities that exist simultaneously across 8 representations (the "octad"): Graph, Vector, Tensor, Semantic, Document, Temporal, Provenance, Spatial. It detects drift across modalities, automatically repairs via self-normalisation, and maintains ACID transactions across all 8 modalities atomically. Queries return `Sigma(result, proof_certificate)`.

**Critical invariant**: All 8 modalities of an octad entity MUST remain mutually coherent after every operation.

### Source file tree (relevant subset)

```
verisimdb/
├── rust-core/                          48,789 LOC
│   ├── verisim-octad/                  Unified octad entity (8 modalities)
│   │   ├── src/store.rs                Transaction manager
│   │   ├── src/transaction.rs          ACID isolation
│   │   └── tests/                      atomicity/crash_recovery/e2e/contract/aspect
│   ├── verisim-graph/                  RDF/property graphs (Oxigraph)
│   ├── verisim-vector/                 HNSW embeddings
│   ├── verisim-tensor/                 ndarray/Burn
│   ├── verisim-semantic/               CBOR proof blobs, ZKP
│   ├── verisim-document/               Tantivy + LZ4
│   ├── verisim-temporal/               Version history
│   ├── verisim-provenance/             Hash-chain lineage (SHA-256)
│   ├── verisim-spatial/                R-tree geospatial
│   ├── verisim-drift/                  Cross-modal drift detection
│   ├── verisim-normalizer/             Self-normalisation
│   ├── verisim-wal/                    Write-ahead log
│   └── verisim-storage/                redb B-tree
├── elixir-orchestration/               185,797 LOC (OTP/Raft)
├── src/
│   ├── vcl/                            7,283 LOC ReScript (VCL type system)
│   │   ├── VCLTypeChecker.res
│   │   ├── VCLBidir.res                Bidirectional type inference
│   │   ├── VCLSubtyping.res
│   │   └── VCLProofObligation.res
│   ├── registry/                       Raft cluster (ReScript)
│   │   └── KRaftCluster.res
│   └── abi/                            1,748 LOC Idris2
└── docs/
    ├── vcl-formal-semantics.adoc
    ├── vcl-grammar.ebnf
    └── vcl-type-system.adoc
```

### Languages & LOC

| Language | LOC | Purpose |
|----------|-----|---------|
| Rust | 48,789 | Core engine, 8 modality stores |
| Elixir | 185,797 | OTP orchestration, Raft |
| ReScript | 7,283 | VCL type system |
| Idris2 | 1,748 | ABI proofs |

## Existing Proofs (DO NOT REDO)

| File | LOC | Covers |
|------|-----|--------|
| `src/abi/Types.idr` | ~500 | Entity size proofs, platform sizing |
| `src/abi/Foreign.idr` | ~400 | FFI IO monad correctness |
| `src/abi/Layout.idr` | ~300 | Memory layout invariants |
| `src/vcl/VCLBidir.res` | 1500 | Bidirectional inference impl |
| `src/vcl/VCLSubtyping.res` | 247 | Subtyping rules impl |

## Theorems to Prove

### V1: Octad coherence invariant

**Target file**: `verification/proofs/idris2/OctadCoherence.idr`
**Source being verified**: `rust-core/verisim-octad/src/store.rs`
**Prover**: Idris2
**Priority**: P0

**Statement**:
> For every Octad entity, all 8 modalities remain mutually coherent after every operation. Specifically: graph edges reference existing document IDs; vector embeddings match document content (within drift threshold); provenance lineage matches temporal version history; spatial coords align with document location mentions.

**Formal signature**:
```idris
module OctadCoherence

%default total

public export
data Modality = Graph | Vector | Tensor | Semantic
              | Document | Temporal | Provenance | Spatial

public export
record Octad where
  constructor MkOctad
  graphData : GraphModality
  vectorData : VectorModality
  tensorData : TensorModality
  semanticData : SemanticModality
  documentData : DocumentModality
  temporalData : TemporalModality
  provenanceData : ProvenanceModality
  spatialData : SpatialModality

||| Pairwise coherence predicate between modalities.
public export
Consistent : Modality -> Modality -> Octad -> Type

||| The octad coherence invariant: all pairs are consistent.
public export
Coherent : Octad -> Type
Coherent o = (m1, m2 : Modality) -> Consistent m1 m2 o

||| Operations on octads.
public export
data Op = UpdateGraph | UpdateVector | UpdateDoc | ... -- all ops

||| Applying an op that respects coherence preserves coherence.
export
opPreservesCoherence : (o : Octad) -> Coherent o -> (op : Op) ->
                       Coherent (applyOp op o)
```

**Hints**:
- Define Consistent as a relation parameterised over modality pairs
- Most pairs are trivially consistent; focus on the non-trivial ones (graph↔doc, vec↔doc, prov↔temp)
- Transaction layer ensures atomicity — use this in proof

**Obligations**:
- [ ] Define 8 modalities
- [ ] Define Consistent relation (pairwise)
- [ ] Define Coherent (universal quantification over pairs)
- [ ] Prove each Op preserves Coherent

---

### V2: VCL type inference soundness

**Target file**: `verification/proofs/lean4/VCLTypeSoundness.lean`
**Source being verified**: `src/vcl/VCLBidir.res` + `src/vcl/VCLTypes.res`
**Prover**: Lean4
**Priority**: P0

**Statement**:
> Bidirectional type inference is sound: if `Γ ⊢ e ⇒ τ` (synthesis) or `Γ ⊢ e ⇐ τ` (checking) holds, then `e` evaluates to a value of type `τ`. Progress: well-typed expressions either are values or can step. Preservation: stepping preserves types.

**Formal signature**:
```lean
inductive VclTy where
  | TString | TNat | TBool | TEntity
  | TList : VclTy → VclTy
  | TArrow : VclTy → VclTy → VclTy
  | TPi : VclTy → (VclTy → VclTy) → VclTy   -- dependent function
  | TSigma : VclTy → (VclTy → VclTy) → VclTy -- dependent pair

inductive VclExpr where
  | EVar : Nat → VclExpr
  | ELit : VclVal → VclExpr
  | ELam : VclTy → VclExpr → VclExpr
  | EApp : VclExpr → VclExpr → VclExpr

-- Typing judgement
inductive HasType : Ctx → VclExpr → VclTy → Prop

-- Evaluation
inductive Step : VclExpr → VclExpr → Prop
inductive IsValue : VclExpr → Prop

-- Progress
theorem progress : ∀ e t, HasType [] e t → IsValue e ∨ ∃ e', Step e e'

-- Preservation
theorem preservation : ∀ Γ e e' t,
  HasType Γ e t → Step e e' → HasType Γ e' t

-- Soundness = Progress + Preservation
theorem type_soundness : ∀ e t v,
  HasType [] e t → Steps e v → IsValue v → HasTypeVal v t
```

**Hints**:
- Standard STLC + dependent types proof
- Use Lean's `induction` tactic on typing derivation
- Skip alpha-equivalence complications; use de Bruijn indices

**Obligations**:
- [ ] Define VclTy (including dependent types)
- [ ] Define HasType (bidirectional judgements)
- [ ] Define Step + IsValue
- [ ] Prove progress
- [ ] Prove preservation
- [ ] Combine into type_soundness

---

### V3: VCL subtyping transitivity

**Target file**: `verification/proofs/lean4/VCLSubtyping.lean`
**Source being verified**: `src/vcl/VCLSubtyping.res`
**Prover**: Lean4
**Priority**: P0

**Statement**:
> The subtyping relation `<:` is reflexive, transitive, and antisymmetric (modulo type equality). Subtyping is decidable for the VCL type system.

**Formal signature**:
```lean
inductive Subtype : VclTy → VclTy → Prop
  | refl : ∀ t, Subtype t t
  | nat_any : Subtype TNat TEntity  -- example
  | arrow : ∀ s1 s2 t1 t2, Subtype t1 s1 → Subtype s2 t2 →
            Subtype (TArrow s1 s2) (TArrow t1 t2)
  -- ... other rules

theorem subtype_refl : ∀ t, Subtype t t
theorem subtype_trans : ∀ a b c, Subtype a b → Subtype b c → Subtype a c
theorem subtype_decidable : ∀ a b, Decidable (Subtype a b)
```

**Hints**:
- Arrow types are contravariant in arg, covariant in result
- Use structural induction on the types
- Decidability from structural recursion

**Obligations**:
- [ ] Define Subtype inductive
- [ ] Prove reflexivity
- [ ] Prove transitivity
- [ ] Prove decidability

---

### V4: Raft consensus safety

**Target file**: `verification/proofs/lean4/RaftSafety.lean`
**Source being verified**: `src/registry/KRaftCluster.res` + `src/registry/MetadataLog.res`
**Prover**: Lean4
**Priority**: P0

**Statement**:
> Raft safety: once a log entry is committed, no node's log will ever diverge at that index. Leader completeness: any committed entry is present in all future leaders' logs.

**Formal signature**:
```lean
structure NodeState where
  log : List LogEntry
  commitIndex : Nat
  currentTerm : Nat

structure ClusterState where
  nodes : List NodeState
  leaderId : Option NodeId

-- Safety invariant
def LogMatching (s : ClusterState) : Prop := ...
def LeaderCompleteness (s : ClusterState) : Prop := ...

-- Transition relation
inductive Step : ClusterState → ClusterState → Prop

-- SAFETY THEOREM
theorem raft_safety : ∀ s s', Reachable s → Step s s' →
  LogMatching s' ∧ LeaderCompleteness s'
```

**Hints**:
- Reference Diego Ongaro's Raft thesis (Chapter 5 has safety proofs)
- Lean4 has existing Raft formalisations (search for "raft" in mathlib/community)
- This is the largest proof; allocate significant time

**Obligations**:
- [ ] Model node state + cluster
- [ ] Define append-entries, vote, commit steps
- [ ] Define safety invariants
- [ ] Prove each step preserves invariants

---

### V5: Transaction atomicity (all-or-nothing across 8 modalities)

**Target file**: `verification/proofs/tlaplus/OctadAtomicity.tla`
**Source being verified**: `rust-core/verisim-octad/src/transaction.rs`
**Prover**: TLA+
**Priority**: P0

**Statement**:
> For any transaction T on an octad, either all 8 modalities are updated (COMMITTED) or none (ABORTED). No PARTIAL state is ever observable.

**Formal signature**:
```tla
EXTENDS Naturals, Sequences, FiniteSets

VARIABLES
    octadState,     \* Current state of each modality
    txnStatus,      \* PENDING | COMMITTED | ABORTED
    modalityUpdates \* Set of updated modalities

Modalities == {"graph", "vector", "tensor", "semantic",
               "document", "temporal", "provenance", "spatial"}

Atomicity ==
    /\ (txnStatus = "COMMITTED" => modalityUpdates = Modalities)
    /\ (txnStatus = "ABORTED" => modalityUpdates = {})
    /\ (txnStatus = "PENDING" => TRUE)  \* unrestricted during execution

Invariant == [](txnStatus \in {"COMMITTED", "ABORTED"} => Atomicity)
```

**Hints**:
- Model-check with TLC
- Use TLC config: MaxTransactions = 3, MaxModalities = 8
- Start with single-transaction model, then extend to concurrent

**Obligations**:
- [ ] Define state variables
- [ ] Define transaction lifecycle
- [ ] Model-check Atomicity invariant
- [ ] Document any discovered counterexamples

---

### V6: WAL integrity + replay idempotence

**Target file**: `verification/proofs/lean4/WALIntegrity.lean`
**Source being verified**: `rust-core/verisim-wal/src/`
**Prover**: Lean4
**Priority**: P1

**Statement**:
> Write-Ahead Log replay is idempotent: `replay(replay(wal)) = replay(wal)`. Sequence numbers are monotonic. CRC verification rejects corrupted entries.

**Formal signature**:
```lean
structure WALEntry where
  seqNum : Nat
  payload : List Byte
  crc : Nat

structure WAL where
  entries : List WALEntry
  mono : ∀ i j, i < j → (entries.get? i).seqNum < (entries.get? j).seqNum

def replay (wal : WAL) (state : DBState) : DBState
theorem replay_idempotent : ∀ wal s, replay wal (replay wal s) = replay wal s
theorem replay_monotonic : ∀ wal s1 s2, s1 ≤ s2 → replay wal s1 ≤ replay wal s2
```

**Hints**:
- Idempotence requires tracking applied seq numbers
- Model replay as fold over entries with skip-if-already-applied

**Obligations**:
- [ ] Define WAL structure
- [ ] Define replay function
- [ ] Prove idempotence
- [ ] Prove monotonicity

---

### V7: Provenance chain immutability

**Target file**: `verification/proofs/agda/ProvenanceChain.agda`
**Source being verified**: `rust-core/verisim-provenance/src/lib.rs`
**Prover**: Agda
**Priority**: P1

**Statement**:
> Provenance chain is append-only and tamper-evident. Each entry's hash depends on its predecessor's hash (hash chain). Timestamps are strictly monotonic. Changing any entry breaks all subsequent hashes.

**Formal signature**:
```agda
module ProvenanceChain where

open import Data.Nat
open import Data.List
open import Relation.Binary.PropositionalEquality

record Entry : Set where
  field
    timestamp : ℕ
    actor : ℕ
    action : ℕ
    prevHash : ℕ
    myHash : ℕ

data ValidChain : List Entry → Set where
  Empty : ValidChain []
  Cons : (e : Entry) (es : List Entry) →
         ValidChain es →
         PrevHashCorrect e es →
         TimestampMonotonic e es →
         ValidChain (e ∷ es)

-- THEOREM: Append-only: if c is valid, (e ∷ c) valid only if e extends c correctly
append-only : ∀ {e c} → ValidChain (e ∷ c) → ValidChain c

-- THEOREM: Tamper-evident: modifying any entry invalidates the chain
tamper-evident : ∀ {c c'} → ValidChain c →
                  HasCorruptedEntry c c' → ¬ ValidChain c'
```

**Hints**:
- Use Agda's dependent types to encode hash-chain invariant
- Treat hash function as abstract but injective
- Reference existing hash-chain formalisations

**Obligations**:
- [ ] Define Entry with hash fields
- [ ] Define ValidChain inductively
- [ ] Prove append-only property
- [ ] Prove tamper-evidence

---

### V8: Drift metric correctness

**Target file**: `verification/proofs/idris2/DriftMetric.idr`
**Source being verified**: `rust-core/verisim-drift/src/calculator.rs`
**Prover**: Idris2
**Priority**: P1

**Statement**:
> The drift metric d(m1, m2) is a proper metric: d(x,x)=0, d(x,y)=d(y,x), triangle inequality. Thresholds are sound: if d(x,y) > threshold, drift detection returns True.

**Obligations**:
- [ ] Define drift metric
- [ ] Prove metric axioms (identity, symmetry, triangle inequality)
- [ ] Prove threshold soundness

---

### V9: Normalizer determinism

**Target file**: `verification/proofs/tlaplus/Normalizer.tla`
**Source being verified**: `rust-core/verisim-normalizer/src/lib.rs`
**Prover**: TLA+
**Priority**: P2

**Statement**:
> Self-normalisation produces deterministic output: given the same octad state and conflict, the resolution is the same. Multi-strategy resolution converges.

**Obligations**:
- [ ] Model normaliser as state transition
- [ ] Model-check determinism property
- [ ] Model-check convergence

---

### V10: Transaction serializability

**Target file**: `verification/proofs/tlaplus/Serializability.tla`
**Source being verified**: `rust-core/verisim-octad/src/transaction.rs`
**Prover**: TLA+
**Priority**: P2

**Statement**:
> Concurrent transactions across 8 modalities are serializable: there exists a serial ordering equivalent to the concurrent execution.

**Obligations**:
- [ ] Model concurrent transactions
- [ ] Define serializability predicate
- [ ] Model-check with 2-3 concurrent transactions

---

### V11: Connector type safety (eliminate Obj.magic)

**Target file**: `verification/proofs/idris2/ConnectorSafety.idr`
**Source being verified**: `connectors/clients/*.res` (20+ Obj.magic instances)
**Prover**: Idris2
**Priority**: P2

**Statement**:
> API schema refinement types prevent unvalidated JSON casts. Every `json -> typed` conversion must go through a validator that returns `Either Error T`.

**Obligations**:
- [ ] Define Schema type
- [ ] Define ValidatedValue (refinement)
- [ ] Prove unsafe cast is impossible by construction

---

### V12: FFI pointer validity + memory ownership

**Target file**: `verification/proofs/idris2/FFIOwnership.idr`
**Source being verified**: `src/abi/Foreign.idr` + Rust FFI exports
**Prover**: Idris2
**Priority**: P2

**Statement**:
> FFI pointers are non-null before dereference. Memory ownership is explicit: whoever allocates, frees. Double-free is impossible by type system.

**Obligations**:
- [ ] Define Owned<T> linear type
- [ ] Prove non-null invariant
- [ ] Prove no-double-free by type system

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/verisimdb
mkdir -p verification/proofs/{idris2,lean4,agda,tlaplus}

just proof-check-all

idris2 --check verification/proofs/idris2/*.idr
lean verification/proofs/lean4/*.lean
agda --safe verification/proofs/agda/*.agda
tlc verification/proofs/tlaplus/*.tla

just proof-scan-dangerous
```

## Banned Patterns (NEVER use)

- `believe_me`, `assert_total`, `postulate` (Idris2)
- `sorry` (Lean4)
- `Admitted` (Coq)
- `postulate` (Agda)
- `unsafeCoerce` (Haskell)
- `Obj.magic` (OCaml/ReScript) — 20+ instances exist in connectors; V11 aims to remove these

## Handoff Checklist

- [ ] All 12 theorems proven
- [ ] No banned patterns in new proof files
- [ ] SPDX headers on all files
- [ ] `just proof-check-all` returns PASS
- [ ] `just proof-scan-dangerous` returns PASS
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete verisimdb critical proofs (12/12 theorems)`

## Blockers

- V4 (Raft safety) is a very large proof — may take multiple sessions
- V5/V9/V10 (TLA+) require TLC model checker installed
- V11 requires touching 20+ ReScript files in connectors/clients/; scope carefully
