# Proof Spec: ProvenCrypto.jl
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/ProvenCrypto.jl`
**Tier**: T1 — Critical
**Total Theorems**: 7
**Primary Prover(s)**: Lean4 (5), Idris2 (2)
**Existing Proof Coverage**: 0% (Julia interfaces defined; proofs are planned targets)
**Dependencies**: `proven`

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | PC1 Kyber KEM correctness | L4 | [ ] Pending | — |
| 2 | PC2 Dilithium signature soundness | L4 | [ ] Pending | — |
| 3 | PC3 SPHINCS+ hash-based security | L4 | [ ] Pending | — |
| 4 | PC4 Constant-time primitives | I2 | [ ] Pending | — |
| 5 | PC5 ZK-STARK transparency | L4 | [ ] Pending | — |
| 6 | PC6 Proof export semantics preservation | I2 | [ ] Pending | — |
| 7 | PC7 Hardware RNG entropy bounds | L4 | [ ] Pending | — |

## Context

Julia post-quantum crypto library. Primitives: Kyber, Dilithium, SPHINCS+. Protocols: Noise, Signal, TLS 1.3. ZK: Groth16, PLONK, Halo2, STARKs. Hardware accel: GPU (CUDA, ROCm, Metal), NPU/TPU, SGX/SEV/TrustZone.

### Key files
- `src/postquantum/kyber.jl` — NIST PQC KEM
- `src/postquantum/dilithium.jl` — NIST PQC signatures
- `src/zkproofs/zkstark.jl` (391 LOC) — transparent STARK
- `src/verification/proof_export.jl` (320 LOC) — export to Lean/Coq/Isabelle

### Critical invariants
1. Constant-time key operations (no side channels)
2. ZK-STARK is transparent (no trusted setup)
3. Proof export preserves semantics
4. GPU accel doesn't introduce precision errors
5. Hardware RNG meets NIST SP 800-90

## Theorems to Prove

### PC1: Kyber KEM correctness

**Target file**: `verification/proofs/lean4/KyberCorrectness.lean`
**Source**: `src/postquantum/kyber.jl`
**Priority**: P0

**Statement**:
> Kyber KEM: decaps(privkey, encaps(pubkey, msg)) = msg with overwhelming probability. IND-CCA2 security under MLWE assumption.

**Obligations**:
- [ ] Model Kyber parameters (n=256, q=3329)
- [ ] Prove correctness (decaps undoes encaps)
- [ ] Cite MLWE assumption as axiom with NIST reference

---

### PC2: Dilithium signature soundness

**Target file**: `verification/proofs/lean4/DilithiumSound.lean`
**Priority**: P0

**Statement**:
> Dilithium: verify(pubkey, msg, sign(privkey, msg)) = True. EUF-CMA security.

**Obligations**:
- [ ] Model Dilithium scheme
- [ ] Prove sign/verify correctness
- [ ] Cite security assumption

---

### PC3: SPHINCS+ hash-based security

**Target file**: `verification/proofs/lean4/SPHINCSSecurity.lean`
**Priority**: P0

**Statement**:
> SPHINCS+ security reduces to hash function preimage resistance.

**Obligations**:
- [ ] Model SPHINCS+ structure
- [ ] Prove security reduction

---

### PC4: Constant-time primitives

**Target file**: `verification/proofs/idris2/ConstantTime.idr`
**Priority**: P0

**Statement**:
> All security-sensitive comparisons take constant time regardless of inputs.

**Obligations**:
- [ ] Model runtime as step count
- [ ] Prove comparison step count independent of values

---

### PC5: ZK-STARK transparency

**Target file**: `verification/proofs/lean4/STARKTransparent.lean`
**Source**: `src/zkproofs/zkstark.jl`
**Priority**: P1

**Statement**:
> ZK-STARK proof system requires no trusted setup. All randomness is publicly verifiable.

**Obligations**:
- [ ] Model setup phase
- [ ] Prove no secret randomness used
- [ ] Prove all challenges derived from public transcript (Fiat-Shamir)

---

### PC6: Proof export semantics preservation

**Target file**: `verification/proofs/idris2/ProofExport.idr`
**Source**: `src/verification/proof_export.jl`
**Priority**: P1

**Statement**:
> Exporting a Julia proof to Lean/Coq/Isabelle preserves its semantics. Type equivalence across exports.

**Obligations**:
- [ ] Define proof abstract syntax
- [ ] Prove export is injective on well-formed proofs

---

### PC7: Hardware RNG entropy bounds

**Target file**: `verification/proofs/lean4/RNGEntropy.lean`
**Priority**: P2

**Statement**:
> Hardware RNG output has min-entropy ≥ H_min (per NIST SP 800-90B).

**Obligations**:
- [ ] Model RNG output as distribution
- [ ] Cite entropy estimation procedure

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/ProvenCrypto.jl
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] All 7 theorems discharged
- [ ] Commit: `proof: complete ProvenCrypto.jl proofs (7/7 theorems)`
