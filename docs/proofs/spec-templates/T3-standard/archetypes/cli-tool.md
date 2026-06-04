# Archetype: cli-tool (Command-Line Tool)
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Applies to**: CLI tools (Rust, Deno, Julia)

## Common Proofs

### CLI-1: Exit code discipline

**Target**: `verification/proofs/idris2/ExitCodes.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: 0 on success, non-zero on failure, documented exit codes.

**Obligations**:
- [ ] Enumerate exit codes
- [ ] Prove each path leads to documented code

---

### CLI-2: Idempotence (where applicable)

**Target**: `verification/proofs/idris2/Idempotent.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**: Re-running a completed action is safe.

**Obligations**:
- [ ] Model state transitions
- [ ] Prove `apply(apply(s)) = apply(s)`

---

### CLI-3: Input validation

**Target**: `verification/proofs/idris2/InputValid.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: All inputs validated before use. No path traversal, no injection.

**Obligations**:
- [ ] Enumerate input types
- [ ] Prove validator covers each

---

### CLI-4: No secrets in output

**Target**: `verification/proofs/idris2/NoSecrets.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**: API keys, tokens, passwords never appear in stdout/stderr.

**Obligations**:
- [ ] List secret types
- [ ] Prove redaction on output

---

## Plus: Mandatory ABI proofs

ABI-1 through ABI-5.

## Repos using this archetype

bofig, chimichanga, defiant, dictask, empty-linter, fireflag, git-reticulator, git-scripts, grim-repo, infrastructure-automation, live-files, modshells, polysafe-gitfixer, raze-tui, session-sentinel, valence-shell, vexometer, vex-tools, bunsenite, supernorma
