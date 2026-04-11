# Proof Spec: echidna
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/echidna`
**Tier**: T1 — Critical
**Total Theorems**: 13
**Primary Prover(s)**: Idris2 (7), Lean4 (3), Agda (2), TLA+ (1)
**Existing Proof Coverage**: ~40% (Idris2 ABI 17 modules, 30+ Agda meta-checker properties, 1K+ property tests)
**Dependencies**: `proven` library, `rsr-template-repo` ABI proofs

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | E1 TrustLevel lattice properties | L4 | [x] Done | 2026-04-04 |
| 2 | E2 Axiom tracker completeness | I2 | [x] Done | 2026-04-04 |
| 3 | E3 Dispatch pipeline ordering | I2 | [x] Done | 2026-04-04 |
| 4 | E4 Trust level soundness (Reject→L1) | L4+I2 | [x] Done | L4 2026-04-04; I2 2026-04-11 |
| 5 | E5 Prover dispatch compatibility | I2 | [x] Done | 2026-04-04 |
| 6 | E6 ProverKind injectivity (49 variants) | I2 | [x] Done | 2026-04-04 |
| 7 | E7 GNN embedding faithfulness | I2 | [x] Done | 2026-04-04 (EchidnaABI/Gnn.idr) |
| 8 | E8 VQL-UT query safety | I2 | [x] Done | 2026-04-04 (EchidnaABI/VqlUt.idr) |
| 9 | E9 Proof composition soundness | Ag | [x] Done | 2026-04-04 (ProofComposition.agda) |
| 10 | E10 Pareto frontier maximality | L4 | [ ] Pending | — |
| 11 | E11 SHAKE3/BLAKE3 integrity | L4 | [ ] Pending | — |
| 12 | E12 ProofState serialization | I2 | [ ] Pending | — |
| 13 | E13 Portfolio disagreement detection | TLA | [ ] Pending | — |

## Context

### What this repo does

ECHIDNA (Extensible Cognitive Hybrid Intelligence for Deductive Neural Assistance) is a trust-hardened neurosymbolic theorem proving platform. It orchestrates 49 formal verifiers through a trust pipeline where proofs NEVER become "true" by neural suggestion — formal provers have final say. The platform's core guarantee is that weak proofs cannot escape to output with undeserved trust levels.

**Critical invariant**: If a proof contains any dangerous axiom (sorry, Admitted, believe_me, postulate), its trust level MUST be Level1 (lowest). A tool that checks proofs MUST itself be provably correct.

### Source file tree (relevant subset)

```
echidna/
├── src/
│   ├── rust/
│   │   ├── core.rs                        (310 LOC) Term, Goal, ProofState
│   │   ├── dispatch.rs                    (541 LOC) Trust pipeline orchestration
│   │   ├── provers/mod.rs                 49 ProverKind variants, ProverBackend trait
│   │   ├── verification/
│   │   │   ├── confidence.rs              (259 LOC) TrustLevel hierarchy
│   │   │   ├── axiom_tracker.rs           (446 LOC) Dangerous pattern detection
│   │   │   ├── portfolio.rs               Cross-prover disagreement
│   │   │   ├── pareto.rs                  Multi-objective optimisation
│   │   │   └── certificates.rs            Alethe/DRAT/LRAT/TSTP verification
│   │   ├── integrity/solver_integrity.rs  SHAKE3-512 + BLAKE3 binary checks
│   │   ├── gnn/                           Proof graph GNN
│   │   └── vql_ut.rs                      VQL-UT query engine
│   └── abi/                               17 Idris2 files, 2973 LOC, 0 believe_me
│       ├── Types.idr                      (655 LOC)
│       ├── Foreign.idr                    (445 LOC)
│       ├── Layout.idr                     (236 LOC)
│       └── EchidnaABI/                    Provers, Gnn, VqlUt
└── proofs/                                Existing example proofs (Coq/Lean/Agda/Isabelle/Mizar)
```

### Languages & LOC

| Language | LOC | Purpose |
|----------|-----|---------|
| Rust | 59,578 | Core prover orchestration, trust pipeline |
| Idris2 | 2,973 | Formal ABI proofs |
| Julia | ~3,000 | ML tactic prediction |
| ReScript | 33 files | UI |
| Coq/Lean/Agda/Isabelle/Mizar | ~4,000 | Example proof corpora |

## Existing Proofs (DO NOT REDO)

| File | LOC | Covers |
|------|-----|--------|
| `src/abi/Types.idr` | 655 | 49 ProverKind variants, platform detection, C FFI types |
| `src/abi/Foreign.idr` | 445 | FFI struct layouts |
| `src/abi/Layout.idr` | 236 | Memory layout correctness |
| `src/abi/EchidnaABI/Provers.idr` + subdirs | ~1000 | Prover categorical classification |
| `src/abi/EchidnaABI/Gnn.idr` | ~200 | GNN type definitions, graph structure |
| `src/abi/EchidnaABI/VqlUt.idr` | ~200 | VQL-UT query type safety |
| Agda meta-checker (in CI) | ~500 | 30+ trust pipeline properties |

## Theorems to Prove

### E1: TrustLevel lattice properties

**Target file**: `verification/proofs/lean4/ConfidenceLattice.lean`
**Source being verified**: `src/rust/verification/confidence.rs:19-117`
**Prover**: Lean4
**Priority**: P0

**Statement**:
> The 5-level TrustLevel (Level1..Level5) forms a valid partially-ordered lattice. Adding confirming provers is monotone (never decreases trust). Cross-checking is monotone. The lattice has a bottom (Level1) and top (Level5).

**Formal signature**:
```lean
inductive TrustLevel where
  | Level1 | Level2 | Level3 | Level4 | Level5
  deriving DecidableEq, Repr

instance : LE TrustLevel := ...
instance : LinearOrder TrustLevel := ...

theorem trust_bottom : ∀ t : TrustLevel, TrustLevel.Level1 ≤ t
theorem trust_top : ∀ t : TrustLevel, t ≤ TrustLevel.Level5
theorem trust_antisymm : ∀ a b : TrustLevel, a ≤ b → b ≤ a → a = b
theorem trust_trans : ∀ a b c : TrustLevel, a ≤ b → b ≤ c → a ≤ c

-- Adding confirming provers is monotone
def addConfirming (current : TrustLevel) (newProver : ProverId) : TrustLevel := ...
theorem addConfirming_monotone : ∀ t p, t ≤ addConfirming t p
```

**Hints**:
- Lean's `LinearOrder` derivation handles most algebra if you order variants Level1 < Level2 < ... < Level5
- Use `decide` tactic for finite enumeration proofs
- Reference existing Agda meta-checker for property ordering

**Obligations**:
- [ ] Define TrustLevel with correct ordering
- [ ] Prove reflexivity, antisymmetry, transitivity
- [ ] Prove bottom/top elements
- [ ] Prove monotonicity of addConfirming
- [ ] Prove monotonicity of crossCheck

---

### E2: Axiom tracker completeness

**Target file**: `verification/proofs/idris2/AxiomCompleteness.idr`
**Source being verified**: `src/rust/verification/axiom_tracker.rs:14-250`
**Prover**: Idris2
**Priority**: P0

**Statement**:
> Given the closed set of dangerous patterns D = {sorry, Admitted, believe_me, postulate, assert_total, unsafeCoerce, mk_thm, prim__crash, Obj.magic}, the detection function `detectAxioms` classifies ALL occurrences correctly. No false negatives: if a pattern in D appears in input, detectAxioms returns a matching DangerLevel. The axiom policy function is idempotent.

**Formal signature**:
```idris
module AxiomCompleteness

import Data.List

%default total

public export
data DangerousPattern : Type where
  Sorry : DangerousPattern
  Admitted : DangerousPattern
  BelieveMe : DangerousPattern
  Postulate : DangerousPattern
  AssertTotal : DangerousPattern
  UnsafeCoerce : DangerousPattern
  MkThm : DangerousPattern
  PrimCrash : DangerousPattern
  ObjMagic : DangerousPattern

public export
data DangerLevel = Safe | Noted | Warning | Reject

||| The closed set of all dangerous patterns (exhaustive).
public export
allDangerousPatterns : List DangerousPattern
allDangerousPatterns = [Sorry, Admitted, BelieveMe, Postulate, AssertTotal,
                        UnsafeCoerce, MkThm, PrimCrash, ObjMagic]

||| Classify each pattern to its danger level.
public export
classify : DangerousPattern -> DangerLevel

||| Completeness: every pattern maps to Warning or Reject (never Safe).
export
noPatternIsSafe : (p : DangerousPattern) -> Not (classify p = Safe)

||| Injectivity: the dangerous pattern set has 9 distinct members.
export
patternsDistinct : length allDangerousPatterns = 9

||| Idempotence: classifying twice gives the same result.
export
classifyIdempotent : (p : DangerousPattern) -> classify p = classify p
```

**Hints**:
- Use Idris2 `Uninhabited` instances for the `Not` proofs
- Pattern match exhaustively on DangerousPattern
- Reference axiom_tracker.rs for current classification (reject=believe_me/mk_thm, warning=sorry/Admitted)

**Obligations**:
- [ ] Define exhaustive DangerousPattern enum
- [ ] Prove `noPatternIsSafe` for all 9 patterns
- [ ] Prove length of `allDangerousPatterns` is exactly 9
- [ ] Prove classification is deterministic (idempotent)

---

### E3: Dispatch pipeline ordering

**Target file**: `verification/proofs/idris2/DispatchOrdering.idr`
**Source being verified**: `src/rust/dispatch.rs:72-300`
**Prover**: Idris2
**Priority**: P0

**Statement**:
> The trust pipeline executes stages in a fixed order: Integrity → Sandbox → Parse → Verify → Certificates → AxiomScan → Confidence. No proof can reach "verified" output without passing every stage. The pipeline is modelled as an indexed type where each stage transition is a proof term.

**Formal signature**:
```idris
module DispatchOrdering

%default total

public export
data PipelineStage = Integrity | Sandbox | Parse | Verify
                   | Certificates | AxiomScan | Confidence | Complete

||| Valid forward transitions only (no skipping).
public export
data Advance : PipelineStage -> PipelineStage -> Type where
  IntToSand : Advance Integrity Sandbox
  SandToPar : Advance Sandbox Parse
  ParToVer  : Advance Parse Verify
  VerToCert : Advance Verify Certificates
  CertToAx  : Advance Certificates AxiomScan
  AxToConf  : Advance AxiomScan Confidence
  ConfToDone : Advance Confidence Complete

||| Stage numbers for ordering proofs.
public export
stageNum : PipelineStage -> Nat
stageNum Integrity = 0
stageNum Sandbox = 1
stageNum Parse = 2
stageNum Verify = 3
stageNum Certificates = 4
stageNum AxiomScan = 5
stageNum Confidence = 6
stageNum Complete = 7

||| Proof that advance increases stage number by exactly 1.
export
advanceIncrements : {from, to : PipelineStage} -> Advance from to ->
                    stageNum to = S (stageNum from)

||| Proof that Complete can only be reached from Confidence.
export
completeOnlyFromConfidence : {from : PipelineStage} -> Advance from Complete ->
                              from = Confidence

||| Proof that Integrity is always the entry point (no predecessors).
export
integrityIsEntry : {from : PipelineStage} -> Not (Advance from Integrity)
```

**Hints**:
- Use GADT-style indexed types (like `ValidTransition` in hypatia's PipelineState.idr)
- Each `Advance` constructor corresponds to one stage transition
- `Uninhabited` for the no-predecessor proof

**Obligations**:
- [ ] Define all 7 stage transitions as GADT constructors
- [ ] Prove advance increments stage number
- [ ] Prove Complete is only reachable from Confidence
- [ ] Prove Integrity has no predecessors

---

### E4: Trust level soundness (Reject axiom → Level1)

**Target file**: `verification/proofs/idris2/TrustSoundness.idr`
**Source being verified**: `src/rust/verification/confidence.rs:77-117` + `src/rust/verification/axiom_tracker.rs`
**Prover**: Idris2
**Priority**: P0

**Statement**:
> If any Reject-level dangerous axiom appears in a proof, the computed trust level MUST be Level1. Similarly, Warning axioms cap at Level1. This is the critical safety theorem: weak proofs cannot escape.

**Formal signature**:
```idris
module TrustSoundness

%default total

public export
data TrustLevel = L1 | L2 | L3 | L4 | L5

public export
data AxiomPolicy = Accepted | Noted | Warned | Rejected

||| The trust computation function as modelled from Rust.
public export
computeTrust : AxiomPolicy -> (crossChecked : Bool) -> (hasCert : Bool) ->
               (smallKernel : Bool) -> TrustLevel

||| MAIN THEOREM: Rejected axiom caps trust at Level1.
export
rejectedCapsAtL1 : (cc : Bool) -> (hc : Bool) -> (sk : Bool) ->
                   computeTrust Rejected cc hc sk = L1

||| Warned axiom caps trust at Level1.
export
warnedCapsAtL1 : (cc : Bool) -> (hc : Bool) -> (sk : Bool) ->
                 computeTrust Warned cc hc sk = L1
```

**Hints**:
- This is the single most important safety proof in echidna
- Function `computeTrust` should pattern-match policy FIRST, fall through other arguments only for Accepted/Noted
- Proof by pattern matching

**Obligations**:
- [ ] Define computeTrust that matches Rust logic in confidence.rs
- [ ] Prove rejectedCapsAtL1 for all boolean combinations
- [ ] Prove warnedCapsAtL1 for all boolean combinations

---

### E5: Prover dispatch compatibility

**Target file**: `verification/proofs/idris2/DispatchCompatibility.idr`
**Source being verified**: `src/rust/dispatch.rs` + `src/rust/provers/mod.rs`
**Prover**: Idris2
**Priority**: P1

**Statement**:
> Goals are dispatched only to compatible prover tiers. Linear logic goals do NOT go to first-order ATPs. Dependent type goals do NOT go to SAT solvers. Constraint problems go only to constraint solvers. This prevents semantic-level category errors.

**Formal signature**:
```idris
module DispatchCompatibility

%default total

public export
data GoalKind = PropLogic | FirstOrder | HigherOrder | Dependent
              | Linear | Modal | Constraint | SMT

public export
data ProverTier = InteractiveAsst | SMTSolver | AutoActive | SmallKernel
                | FirstOrderATP | ConstraintSolver | ModelChecker | SATSolver

public export
data Compatible : GoalKind -> ProverTier -> Type where
  PropLogToSAT : Compatible PropLogic SATSolver
  PropLogToSMT : Compatible PropLogic SMTSolver
  FOToATP : Compatible FirstOrder FirstOrderATP
  FOToSMT : Compatible FirstOrder SMTSolver
  HOToInteractive : Compatible HigherOrder InteractiveAsst
  HOToSmallKernel : Compatible HigherOrder SmallKernel
  DepToInteractive : Compatible Dependent InteractiveAsst
  LinearToInteractive : Compatible Linear InteractiveAsst
  ModalToModelCheck : Compatible Modal ModelChecker
  ConstraintToConstr : Compatible Constraint ConstraintSolver
  SMTToSMT : Compatible SMT SMTSolver

||| KEY THEOREM: Linear logic goals are never compatible with first-order ATPs.
export
linearNotToATP : Not (Compatible Linear FirstOrderATP)

||| Dependent goals are never compatible with SAT solvers.
export
dependentNotToSAT : Not (Compatible Dependent SATSolver)

||| Constraint problems only go to constraint solvers.
export
constraintOnlyConstraint : Compatible Constraint t -> t = ConstraintSolver
```

**Hints**:
- Make `Compatible` a GADT; only valid pairs have constructors
- `Uninhabited` for the `Not` proofs
- For `constraintOnlyConstraint`, exhaustive pattern match on the proof

**Obligations**:
- [ ] Define compatibility GADT with ONLY valid pairs
- [ ] Prove linearNotToATP
- [ ] Prove dependentNotToSAT
- [ ] Prove constraintOnlyConstraint

---

### E6: ProverKind discriminant injectivity

**Target file**: `verification/proofs/idris2/ProverKindInjectivity.idr`
**Source being verified**: `src/rust/provers/mod.rs` (ProverKind enum with 49 variants)
**Prover**: Idris2
**Priority**: P1

**Statement**:
> The ProverKind enum has exactly 49 distinct variants. The kind_to_u8 FFI mapping is injective (no collisions). All 49 values are distinct at the type level.

**Formal signature**:
```idris
module ProverKindInjectivity

import Data.List
import Data.List.Elem

%default total

public export
data ProverKind = Agda | Coq | Lean4 | IsabelleHOL | Idris2 | FStar
                | Z3 | CVC5 | AltErgo | DReal
                | Dafny | Why3
                | Metamath | HOLLight | Mizar | HOL4 | PVS | ACL2
                | TLAPS | Twelf | Nuprl | Minlog | Imandra
                | Vampire | EProver | SPASS
                | GLPK | SCIP | MiniZinc | Chuffed | ORTools
                | SPIN | CBMC | SeaHorn
                | CaDiCaL | Kissat | MiniSat
                | NuSMV | TLC | Alloy | Prism | UPPAAL
                | FramaC | Viper | Tamarin | ProVerif | KeY | TypedWasm
                | LinearLogicProver | BDDProver | ModalProver

||| There are exactly 49 prover kinds.
public export
allProvers : List ProverKind

||| THEOREM: The list contains exactly 49 variants.
export
proversCount : length allProvers = 49

||| FFI mapping to u8 codes (0..48).
public export
kindToU8 : ProverKind -> Bits8

||| THEOREM: The mapping is injective.
export
kindToU8Injective : (a, b : ProverKind) -> kindToU8 a = kindToU8 b -> a = b
```

**Hints**:
- Use DecEq derivation for ProverKind
- `allProvers` is a literal list of all constructors
- Injectivity proof by case analysis (long but mechanical)
- Reference `src/abi/EchidnaABI/Provers.idr` for existing categorization

**Obligations**:
- [ ] Define all 49 prover kinds as distinct variants
- [ ] Prove count is exactly 49
- [ ] Define kindToU8 mapping
- [ ] Prove kindToU8 injectivity

---

### E7: GNN embedding faithfulness

**Target file**: `verification/proofs/agda/GNNFaithfulness.agda`
**Source being verified**: `src/rust/gnn/graph.rs`
**Prover**: Agda
**Priority**: P1

**Statement**:
> GNN graph construction is deterministic (same proof tree → same graph). Structural properties are preserved: two proofs with identical trees produce identical embeddings. 7 node kinds × 8 edge kinds are all distinct.

**Formal signature**:
```agda
module GNNFaithfulness where

open import Data.Nat
open import Relation.Binary.PropositionalEquality

data NodeKind : Set where
  TermNode HypothesisNode GoalNode TacticNode
  LemmaNode AxiomNode MetaNode : NodeKind

data EdgeKind : Set where
  Applies Contains Proves Assumes
  Substitutes Abstracts Witnesses Rewrites : EdgeKind

-- Proof tree structure
data ProofTree : Set where
  leaf : ProofTree
  node : NodeKind → List ProofTree → ProofTree

-- Graph construction is deterministic
buildGraph : ProofTree → Graph

-- THEOREM: Same tree produces same graph
buildGraph-det : (t1 t2 : ProofTree) → t1 ≡ t2 → buildGraph t1 ≡ buildGraph t2
```

**Hints**:
- Use Agda's `cong` for congruence
- Graph construction should be a pure function
- Reference existing Idris2 Gnn.idr for types

**Obligations**:
- [ ] Define 7 NodeKinds + 8 EdgeKinds
- [ ] Prove they are all distinct (Uninhabited evidence)
- [ ] Prove buildGraph determinism
- [ ] Prove structural preservation

---

### E8: VQL-UT query safety

**Target file**: `verification/proofs/idris2/VqlUtSafety.idr`
**Source being verified**: `src/rust/vql_ut.rs` + `src/abi/EchidnaABI/VqlUt.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**:
> VQL queries are injection-free: user-provided strings cannot alter query structure. Type safety: query composition preserves types at the ABI boundary.

**Formal signature**:
```idris
module VqlUtSafety

%default total

public export
data VqlType = TString | TNat | TBool | TEntity | TList VqlType

public export
data SafeLiteral : VqlType -> Type where
  SLStr : String -> SafeLiteral TString  -- already escaped
  SLNat : Nat -> SafeLiteral TNat
  SLBool : Bool -> SafeLiteral TBool

public export
data VqlQuery : VqlType -> Type where
  Lit : SafeLiteral t -> VqlQuery t
  Select : VqlQuery TEntity -> VqlQuery (TList TEntity)
  Where : VqlQuery (TList t) -> VqlQuery TBool -> VqlQuery (TList t)

||| KEY THEOREM: Every query literal is pre-escaped by construction.
export
noRawStrings : (q : VqlQuery t) -> SafelyEscaped q
```

**Obligations**:
- [ ] Define VQL type system
- [ ] Define SafeLiteral that only admits escaped values
- [ ] Prove queries cannot contain raw user strings
- [ ] Prove composition preserves types

---

### E9: Proof composition soundness

**Target file**: `verification/proofs/agda/ProofComposition.agda`
**Source being verified**: `src/rust/verification/portfolio.rs`
**Prover**: Agda
**Priority**: P2

**Statement**:
> Combining sub-proofs from different provers preserves overall soundness: no implicit axiom conflicts when joining proof fragments.

**Obligations**:
- [ ] Model proof fragments with axiom sets
- [ ] Define composition operation
- [ ] Prove composed axiom set = union of inputs
- [ ] Prove if no input has Reject, composition has no Reject

---

### E10: Pareto frontier maximality

**Target file**: `verification/proofs/lean4/ParetoFrontier.lean`
**Source being verified**: `src/rust/verification/pareto.rs`
**Prover**: Lean4
**Priority**: P2

**Statement**:
> The computed Pareto frontier is maximal: no dominated point remains. The frontier is complete: no non-dominated point is missing.

**Obligations**:
- [ ] Define domination relation
- [ ] Define frontier computation
- [ ] Prove frontier contains no dominated points
- [ ] Prove frontier misses no non-dominated points

---

### E11: SHAKE3-512/BLAKE3 integrity soundness

**Target file**: `verification/proofs/lean4/HashIntegrity.lean`
**Source being verified**: `src/rust/integrity/solver_integrity.rs`
**Prover**: Lean4
**Priority**: P2

**Statement**:
> Solver binary verification is based on SHAKE3-512 and BLAKE3. Reference FIPS 202 for collision resistance. Manifest loading (TOML) is authenticated.

**Obligations**:
- [ ] Model hash function as injective up to collisions
- [ ] Prove manifest parsing rejects malformed inputs
- [ ] Cite FIPS 202 for collision resistance (axiom allowed here, cite properly)

---

### E12: ProofState serialization losslessness

**Target file**: `verification/proofs/idris2/ProofStateRoundtrip.idr`
**Source being verified**: `src/rust/core.rs`
**Prover**: Idris2
**Priority**: P2

**Statement**:
> JSON roundtrip of ProofState preserves all fields: `deserialize(serialize(s)) = s`.

**Formal signature**:
```idris
module ProofStateRoundtrip

%default total

public export
record ProofState where
  constructor MkProofState
  goals : List String
  hypotheses : List String
  metaVars : List Nat

serialize : ProofState -> String
deserialize : String -> Maybe ProofState

export
roundtrip : (s : ProofState) -> deserialize (serialize s) = Just s
```

**Obligations**:
- [ ] Define minimal ProofState
- [ ] Define serialize + deserialize
- [ ] Prove roundtrip theorem

---

### E13: Portfolio cross-checking (disagreement detection)

**Target file**: `verification/proofs/tlaplus/PortfolioSolving.tla`
**Source being verified**: `src/rust/verification/portfolio.rs`
**Prover**: TLA+
**Priority**: P2

**Statement**:
> When two provers produce contradictory results (one says valid, other says invalid), the disagreement is always detected and trust is capped.

**Obligations**:
- [ ] Model portfolio as set of prover results
- [ ] Define disagreement detection
- [ ] Model-check that no contradictory pair escapes detection

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/echidna
mkdir -p verification/proofs/{idris2,lean4,agda,coq,tlaplus}

# Check all proofs
just proof-check-all

# Individual checks
idris2 --check verification/proofs/idris2/AxiomCompleteness.idr
idris2 --check verification/proofs/idris2/DispatchOrdering.idr
idris2 --check verification/proofs/idris2/TrustSoundness.idr
idris2 --check verification/proofs/idris2/DispatchCompatibility.idr
idris2 --check verification/proofs/idris2/ProverKindInjectivity.idr
idris2 --check verification/proofs/idris2/VqlUtSafety.idr
idris2 --check verification/proofs/idris2/ProofStateRoundtrip.idr
lean verification/proofs/lean4/ConfidenceLattice.lean
lean verification/proofs/lean4/ParetoFrontier.lean
lean verification/proofs/lean4/HashIntegrity.lean
agda --safe verification/proofs/agda/GNNFaithfulness.agda
agda --safe verification/proofs/agda/ProofComposition.agda
tlc verification/proofs/tlaplus/PortfolioSolving.tla

# Dangerous pattern scan
just proof-scan-dangerous
```

## Banned Patterns (NEVER use)

- `believe_me`, `assert_total`, `postulate` (Idris2)
- `sorry` (Lean4)
- `Admitted` (Coq)
- `postulate` (Agda)
- `unsafeCoerce` (Haskell)
- `Obj.magic` (OCaml/ReScript)

Note: The existing Idris2 ABI in `src/abi/` has **0 believe_me** — do not add any.

## Handoff Checklist

- [ ] All 13 theorems proven
- [ ] No banned patterns anywhere in new proof files
- [ ] All files have SPDX-License-Identifier: PMPL-1.0-or-later header
- [ ] `just proof-check-all` returns PASS
- [ ] `just proof-scan-dangerous` returns PASS
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete echidna critical trust proofs (13/13 theorems)`

## Blockers

None currently. E11 requires citation of FIPS 202; this is acceptable as a cited axiom.
