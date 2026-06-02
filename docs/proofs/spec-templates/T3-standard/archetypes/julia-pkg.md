# Archetype: julia-pkg (Julia Package)
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

**Applies to**: `*.jl` packages

## Common Proofs

### JL-1: Type stability

**Target**: `verification/proofs/idris2/TypeStable.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**: Exported functions return a consistent type regardless of input (no type flip-flop).

**Obligations**:
- [ ] Enumerate exports
- [ ] Document output types
- [ ] Prove no dynamic type switching

---

### JL-2: API contracts

**Target**: `verification/proofs/idris2/APIContract.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**: Function signatures match documented contracts.

**Obligations**:
- [ ] List documented contracts
- [ ] Prove each function respects contract

---

### JL-3: Numerical bounds (where applicable)

**Target**: `verification/proofs/idris2/NumBounds.idr` or `verification/proofs/lean4/NumBounds.lean`
**Prover**: Idris2 or Lean4
**Priority**: P2

**Statement**: Numerical outputs stay in documented ranges.

**Obligations**:
- [ ] Identify numerical functions
- [ ] Prove output bounds

---

### JL-4: NaN/Inf handling

**Target**: `verification/proofs/idris2/NaNHandling.idr`
**Prover**: Idris2
**Priority**: P2

**Statement**: NaN/Inf inputs produce documented behaviour (error or propagation).

**Obligations**:
- [ ] Document NaN/Inf policy
- [ ] Prove compliance

---

## Plus: Mandatory ABI proofs

ABI-1 through ABI-5.

## Repos using this archetype

AcceleratorGate.jl, Axiology.jl, Axiom.jl, BowtieRisk.jl, Causals.jl, Cladistics.jl, Cliodynamics.jl, Cliometrics.jl, Exnovation.jl, FirmwareAudit.jl, HackenbushGames.jl, HardwareResilience.jl, Hyperpolymath.jl, InvestigativeJournalist.jl, JuliaKids.jl, JuliaPackage-Reuse-Audit.jl, KnotTheory.jl, LowLevel.jl, MacroPower.jl, MinixSDK.jl, PolyglotFormalisms.jl, PostDisciplinary.jl, PRComms.jl, QuantumCircuit.jl, ShellIntegration.jl, SiliconCore.jl, Skein.jl, SMTLib.jl, SoftwareSovereign.jl, TradeUnionist.jl, ViableSystems.jl, ZeroProb.jl
