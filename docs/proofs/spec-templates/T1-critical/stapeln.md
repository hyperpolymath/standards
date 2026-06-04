# Proof Spec: stapeln
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/stapeln`
**Tier**: T1 — Critical
**Total Theorems**: 5
**Primary Prover(s)**: Idris2 (4), TLA+ (1)
**Existing Proof Coverage**: 26 .idr files in src/abi/, CryptoProofs + ImporterProofs in container-stack/cerro-torre/verification/
**Dependencies**: `proven`, `ephapax` IR integration

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | ST1 Validation engine determinism | I2 | [ ] Pending | — |
| 2 | ST2 Canvas state machine consistency | I2 | [ ] Pending | — |
| 3 | ST3 Code generator well-formedness | I2 | [ ] Pending | — |
| 4 | ST4 Component connection type safety | I2 | [ ] Pending | — |
| 5 | ST5 Security boundary enforcement | TLA | [ ] Pending | — |

## Context

Visual drag-and-drop container stack designer with formal verification. ReScript/Deno TEA frontend, Elixir/Phoenix backend, Idris2 validator, Rust code generator. Validates compose specs, generates docker-compose output.

### Key files
- `src/abi/Proofs.idr` — ABI invariants
- `backend/lib/stapeln/validation_engine.ex` — compose validation
- `backend/lib/stapeln/security/panic_attacker.ex` — security boundary
- `frontend/src/Canvas.res` — drag-and-drop canvas state machine

### Critical invariants
1. Validation is deterministic (same input → same verdict)
2. Canvas state always consistent (no orphaned nodes)
3. Code generator output always valid YAML
4. Connections respect protocol compatibility
5. Security boundaries cryptographically enforced

## Theorems to Prove

### ST1: Validation engine determinism

**Target file**: `verification/proofs/idris2/ValidationDeterminism.idr`
**Priority**: P0

**Statement**:
> For any compose spec s, `validate s` always returns the same verdict. No hidden state, no race conditions.

**Formal signature**:
```idris
data Verdict = Valid | Invalid (List Error)
validate : ComposeSpec -> Verdict
validateDeterministic : (s : ComposeSpec) -> validate s = validate s
```

**Obligations**:
- [ ] Model ComposeSpec
- [ ] Prove validate is pure function

---

### ST2: Canvas state machine consistency

**Target file**: `verification/proofs/idris2/CanvasStateMachine.idr`
**Source**: `frontend/src/Canvas.res`
**Priority**: P0

**Statement**:
> Canvas invariants: every edge connects two nodes that exist in the graph. No orphaned edges after any operation.

**Obligations**:
- [ ] Model Canvas as (nodes, edges) with proof
- [ ] Prove all operations preserve invariant

---

### ST3: Code generator well-formedness

**Target file**: `verification/proofs/idris2/CodeGenWF.idr`
**Priority**: P1

**Statement**:
> For any valid canvas c, `generate c` produces syntactically valid docker-compose YAML.

**Obligations**:
- [ ] Model YAML grammar
- [ ] Prove generator output matches grammar

---

### ST4: Component connection type safety

**Target file**: `verification/proofs/idris2/ConnectionTypes.idr`
**Priority**: P1

**Statement**:
> Only compatible components can be connected. Network protocol compatibility checked at type level.

**Obligations**:
- [ ] Define Component with typed ports
- [ ] Prove connect requires compatible types

---

### ST5: Security boundary enforcement

**Target file**: `verification/proofs/tlaplus/SecurityBoundary.tla`
**Source**: `backend/lib/stapeln/security/panic_attacker.ex`
**Priority**: P2

**Statement**:
> Requests from frontend cannot bypass verifier. All generated code is scanned by panic_attacker before output.

**Obligations**:
- [ ] Model request flow
- [ ] Model-check no-bypass property

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/stapeln
just proof-check-all
```

## Handoff Checklist

- [ ] All 5 theorems proven
- [ ] Commit: `proof: complete stapeln proofs (5/5 theorems)`
