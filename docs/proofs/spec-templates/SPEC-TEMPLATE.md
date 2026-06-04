# Proof Spec: {{REPO_NAME}}
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/{{REPO_NAME}}`
**Tier**: T{{N}} — {{Critical|High|Standard|Light}}
**Total Theorems**: {{N}}
**Primary Prover(s)**: {{Idris2|Lean4|Agda|Coq|TLA+}}
**Existing Proof Coverage**: {{X}}% ({{existing}}/{{total}})
**Dependencies**: {{list of upstream specs required}}

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| 1 | {{ID}} {{short name}} | I2 | [ ] Pending | — |
| 2 | {{ID}} {{short name}} | L4 | [ ] Pending | — |

## Context

### What this repo does
{{1-2 paragraph summary}}

### Source file tree (relevant subset)
```
{{repo_name}}/
├── src/...
└── ...
```

### Languages & LOC
| Language | LOC | Purpose |
|----------|-----|---------|
| | | |

## Existing Proofs (DO NOT REDO)

| File | LOC | Covers |
|------|-----|--------|
| | | |

## Theorems to Prove

### {{ID}}: {{Short name}}

**Target file**: `verification/proofs/{{prover}}/{{Filename}}.{{ext}}`
**Source being verified**: `{{source file}}:{{line range}}`
**Prover**: {{Idris2|Lean4|Agda|Coq|TLA+}}
**Priority**: P0|P1|P2

**Statement**:
> {{Plain-English statement of what must be proven}}

**Formal signature**:
```{{lang}}
{{Expected theorem signature / type}}
```

**Hints**:
- {{Relevant proof technique}}
- {{Any existing lemmas that can be reused}}
- {{Common pitfalls}}

**Obligations**:
- [ ] {{Sub-goal 1}}
- [ ] {{Sub-goal 2}}

---

### {{ID-2}}: {{Short name}}

{{...repeat for each theorem...}}

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/{{REPO_NAME}}

# Check all Idris2 proofs
idris2 --check verification/proofs/idris2/*.idr

# Check all Lean4 proofs
lean verification/proofs/lean4/*.lean

# Check all Agda proofs (safe mode)
agda --safe verification/proofs/agda/*.agda

# Check all Coq proofs
coqc verification/proofs/coq/*.v

# Run full suite via Justfile
just proof-check-all

# Scan for dangerous patterns
just proof-scan-dangerous
```

## Banned Patterns (NEVER use)

- `believe_me`, `assert_total`, `postulate` (Idris2)
- `sorry` (Lean4)
- `Admitted` (Coq)
- `postulate` (Agda)
- `unsafeCoerce` (Haskell)
- `Obj.magic` (OCaml/ReScript)

CI enforces this via `panic-attack assail --proofs-only`.

## Handoff Checklist

Before marking this spec complete:

- [ ] All {{N}} theorems proven (no Admitted/sorry)
- [ ] All proof files have SPDX header
- [ ] `just proof-check-all` returns PASS
- [ ] `just proof-scan-dangerous` returns PASS
- [ ] Repo's `PROOF-STATUS.md` updated with completion dates
- [ ] Changes committed with message: `proof: complete {{REPO_NAME}} proofs (N/N theorems)`

## Blockers

{{Document anything that prevented completion — leave empty if none}}
