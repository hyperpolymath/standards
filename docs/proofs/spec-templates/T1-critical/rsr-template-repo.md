# Proof Spec: rsr-template-repo
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/rsr-template-repo`
**Tier**: T1 — Critical (baseline for ALL repos)
**Total Theorems**: 7 (5 ABI + 2 core typing)
**Primary Prover(s)**: Idris2 (6), Lean4 (1)
**Existing Proof Coverage**: 0% — scaffolding exists, proofs must be written
**Dependencies**: None — this IS the baseline other repos depend on

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | ABI-1 Non-null pointer safety | I2 | [ ] Pending | — |
| 2 | ABI-2 Memory layout correctness | I2 | [ ] Pending | — |
| 3 | ABI-3 Platform type sizes | I2 | [ ] Pending | — |
| 4 | ABI-4 FFI return types | I2 | [ ] Pending | — |
| 5 | ABI-5 C ABI compliance | I2 | [ ] Pending | — |
| 6 | TP-1 Core data types | I2 | [ ] Pending | — |
| 7 | TP-2 Public API type safety | L4 | [ ] Pending | — |

**IMPORTANT**: The starter files already exist. You need to FILL IN the proof bodies (currently the `export` signatures are declared but bodies may be holes). Verify each compiles with `idris2 --check`.

## Context

### What this repo does

rsr-template-repo is the **canonical baseline template** that every new hyperpolymath repo is cloned from. It defines:
- ABI/FFI standard (Idris2 specs + Zig implementation)
- 17 mandatory GitHub Actions workflows
- `.machine_readable/` A2ML metadata structure
- `verification/` pillar directory structure
- CRG (Component Readiness Grade) criteria
- Contractile system (MUST/TRUST/DUST/INTENT)

**Critical role**: This repo's proofs become the ABI baseline that every derivative repo inherits. Getting these right unblocks ~270 downstream repos.

### Starter files (already exist)

```
rsr-template-repo/
├── PROOF-NEEDS.md                              (template)
├── PROOF-STATUS.md                             (template)
└── verification/proofs/
    ├── idris2/
    │   ├── ABI/
    │   │   ├── Pointers.idr      (starter written, needs verification)
    │   │   ├── Layout.idr        (starter written, needs verification)
    │   │   ├── Platform.idr      (starter written, needs verification)
    │   │   ├── Foreign.idr       (starter written, needs verification)
    │   │   └── Compliance.idr    (starter written, needs verification)
    │   └── Types.idr             (starter written)
    ├── lean4/
    │   └── ApiTypes.lean         (starter written)
    ├── agda/
    │   └── Properties.agda       (starter written)
    ├── coq/
    │   └── TypeSafety.v          (starter written)
    └── tlaplus/
        └── StateMachine.tla      (starter written)
```

### Languages & LOC

| Language | LOC | Purpose |
|----------|-----|---------|
| Idris2 | ~200 | ABI specs (starter) |
| Lean4 | ~50 | API typing (starter) |
| Agda | ~50 | Properties (starter) |
| Coq | ~80 | Type safety template |
| TLA+ | ~80 | State machine template |
| Zig | varies | FFI implementation |

## Existing Proofs (DO NOT REDO)

None yet — this spec IS the baseline creation.

## Theorems to Prove

### ABI-1: Non-null pointer safety

**Target file**: `verification/proofs/idris2/ABI/Pointers.idr` (EXISTS)
**Source being verified**: N/A (this IS the spec for other repos)
**Prover**: Idris2
**Priority**: P0

**Statement**:
> `SafePtr` type carries a compile-time witness that `ptr /= 0`. Any pointer obtained via `checkPtr` is guaranteed non-null. `checkPtr 0` always returns `Nothing`.

**What already exists** (verify these type-check):
```idris
record SafePtr where
  constructor MkSafePtr
  ptr : Bits64
  {auto 0 nonNull : So (ptr /= 0)}

safePtrNeverNull : (sp : SafePtr) -> So (sp.ptr /= 0)
checkPtr : (raw : Bits64) -> Maybe SafePtr
checkPtrZeroIsNothing : checkPtr 0 = Nothing
handlePtrEq : (h1, h2 : Handle tag) -> h1.safePtr.ptr = h2.safePtr.ptr -> h1 = h2
```

**Obligations**:
- [ ] Verify file compiles: `idris2 --check verification/proofs/idris2/ABI/Pointers.idr`
- [ ] If compilation fails, fix proof bodies (NO believe_me)
- [ ] Add any missing lemmas

---

### ABI-2: Memory layout correctness

**Target file**: `verification/proofs/idris2/ABI/Layout.idr` (EXISTS)
**Prover**: Idris2
**Priority**: P0

**Statement**:
> Struct layouts have provable sizes and alignments. Padding calculation is correct: `paddingFor offset alignment` gives the bytes needed to reach the next aligned offset.

**What already exists** (verify these compile):
```idris
interface HasSize (ty : Type) where sizeOf : Nat
interface HasAlignment (ty : Type) where alignOf : Nat
paddingFor : (offset : Nat) -> (alignment : Nat) -> {auto 0 ok : NonZero alignment} -> Nat
alignedNeedsPadding : ...
record StructField, StructLayout ...
FieldAligned, FieldInBounds ...
```

**Obligations**:
- [ ] Verify file compiles
- [ ] Add theorem: `paddingLessThanAlignment` — padding is always < alignment
- [ ] Add theorem: `alignedOffsetPlusPaddingAligned` — (offset + paddingFor offset a) mod a = 0

---

### ABI-3: Platform type sizes

**Target file**: `verification/proofs/idris2/ABI/Platform.idr` (EXISTS)
**Prover**: Idris2
**Priority**: P0

**Statement**:
> For each supported platform (Linux64, MacOS64, Windows64, WASM32, etc.), pointer sizes and C type sizes are correct. size_t always equals pointer size. All pointer sizes are 4 or 8 bytes.

**What already exists**:
```idris
data Platform = Linux64 | LinuxARM64 | MacOS64 | MacOSARM64
              | Windows64 | FreeBSD64 | WASM32
ptrSize : Platform -> Nat
cSizeT : Platform -> Nat
sizeTEqPtrSize : (p : Platform) -> cSizeT p = ptrSize p
ptrSizeValid : (p : Platform) -> Either (ptrSize p = 4) (ptrSize p = 8)
cIntAlways4 : (p : Platform) -> cIntSize p = 4
ptrSizeAtLeast4 : (p : Platform) -> LTE 4 (ptrSize p)
```

**Obligations**:
- [ ] Verify file compiles
- [ ] Add: `cLongSize` proofs per platform (Windows64 is 4, Linux64 is 8)

---

### ABI-4: FFI return types

**Target file**: `verification/proofs/idris2/ABI/Foreign.idr` (EXISTS)
**Prover**: Idris2
**Priority**: P0

**Statement**:
> All FFI functions return through `FFIResult a`. The type is a functor (map preserves structure, identity law holds).

**What already exists**:
```idris
data FFIResult : Type -> Type where
  FFISuccess : (value : a) -> FFIResult a
  FFIError   : (code : Int) -> (msg : String) -> FFIResult a
mapFFIResult : (a -> b) -> FFIResult a -> FFIResult b
mapIdPreserves : (r : FFIResult a) -> mapFFIResult Prelude.id r = r
```

**Obligations**:
- [ ] Verify file compiles
- [ ] Add: composition law `mapFFIResult (g . f) = mapFFIResult g . mapFFIResult f`
- [ ] Add: `FFIReturns` proof utility

---

### ABI-5: C ABI compliance

**Target file**: `verification/proofs/idris2/ABI/Compliance.idr` (EXISTS)
**Prover**: Idris2
**Priority**: P0

**Statement**:
> A struct layout is C ABI compliant when all fields are aligned, all fields are in bounds, and struct size is a multiple of struct alignment.

**What already exists**:
```idris
data AllFieldsAligned : List StructField -> Type
data AllFieldsInBounds : (size : Nat) -> List StructField -> Type
record CABICompliant (layout : StructLayout)
emptyStructCompliant : CABICompliant (MkLayout "empty" [] 1 1)
```

**Obligations**:
- [ ] Verify file compiles
- [ ] Add: example compliance proof for a 2-field struct
- [ ] Add: `CABICompliant` composition (nested structs)

---

### TP-1: Core data type well-formedness

**Target file**: `verification/proofs/idris2/Types.idr` (EXISTS)
**Prover**: Idris2
**Priority**: P1

**Statement**:
> Bounded natural numbers stay within bounds. Non-empty lists satisfy the NonEmpty predicate.

**What already exists**:
```idris
record Bounded (max : Nat) where
  constructor MkBounded
  value : Nat
  {auto 0 inBounds : LTE value max}
boundedLeMax : (b : Bounded max) -> LTE b.value max
zeroIsBounded : {max : Nat} -> Bounded (S max)
data NonEmpty : List a -> Type where
  IsNonEmpty : NonEmpty (x :: xs)
consIsNonEmpty : (x : a) -> (xs : List a) -> NonEmpty (x :: xs)
```

**Obligations**:
- [ ] Verify file compiles

---

### TP-2: Public API type safety

**Target file**: `verification/proofs/lean4/ApiTypes.lean` (EXISTS)
**Prover**: Lean4
**Priority**: P1

**Statement**:
> `ApiResult α` is a functor. BoundedNat preserves bounds.

**What already exists**:
```lean
inductive ApiResult (α : Type)
def map : (α → β) → ApiResult α → ApiResult β
theorem map_id : ∀ r, map id r = r
theorem map_comp : ∀ f g r, map (g ∘ f) r = map g (map f r)
structure BoundedNat (max : Nat)
```

**Obligations**:
- [ ] Verify file compiles with `lean`
- [ ] Add: `BoundedNat.succ` proof (successor preserves bound if possible)

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/rsr-template-repo

# Install provers if missing
# (Idris2, Lean4 required for this spec)

# Check all
just proof-check-all
just proof-scan-dangerous

# Individual
idris2 --check verification/proofs/idris2/ABI/Pointers.idr
idris2 --check verification/proofs/idris2/ABI/Layout.idr
idris2 --check verification/proofs/idris2/ABI/Platform.idr
idris2 --check verification/proofs/idris2/ABI/Foreign.idr
idris2 --check verification/proofs/idris2/ABI/Compliance.idr
idris2 --check verification/proofs/idris2/Types.idr
lean verification/proofs/lean4/ApiTypes.lean
```

## Banned Patterns

As elsewhere: no believe_me, assert_total, postulate, sorry, Admitted.

## Handoff Checklist

- [ ] All 7 starter files compile cleanly with their provers
- [ ] No banned patterns
- [ ] `just proof-check-all` returns PASS
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: baseline rsr-template-repo ABI proofs (7/7 theorems)`

## Blockers

If `idris2 --check` fails on starter files, the proof bodies may need adjustment. Fix them WITHOUT using believe_me. If truly stuck, document in Blockers section and move on.
