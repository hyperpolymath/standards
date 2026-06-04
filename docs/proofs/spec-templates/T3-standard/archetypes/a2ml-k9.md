# Archetype: a2ml-k9 (Parser/Validator)
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Applies to**: A2ML and K9 parsers, validators, and format tools

## Common Proofs

### AK9-1: Parser termination

**Target**: `verification/proofs/idris2/ParserTerminates.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: Parser terminates on any input (no infinite loops).

**Obligations**:
- [ ] Model parser as structural recursion over input
- [ ] Prove termination via well-founded order

---

### AK9-2: Grammar completeness

**Target**: `verification/proofs/idris2/GrammarComplete.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: Parser accepts every string conforming to the grammar.

**Obligations**:
- [ ] Define grammar
- [ ] Prove parser soundness + completeness

---

### AK9-3: Serialisation roundtrip

**Target**: `verification/proofs/idris2/SerRoundtrip.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: `parse(serialise(ast)) = Just ast` for all well-formed ASTs.

**Obligations**:
- [ ] Define AST + parser + serialiser
- [ ] Prove roundtrip

---

### AK9-4: Validator soundness

**Target**: `verification/proofs/idris2/ValidSound.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**: If validator accepts, AST conforms to all constraints.

**Obligations**:
- [ ] Enumerate constraints
- [ ] Prove validator matches each

---

## Plus: Mandatory ABI proofs

ABI-1 through ABI-5.

## Repos using this archetype

a2ml-rs, a2ml-deno, a2ml_ex, a2ml_gleam, a2ml-haskell, a2ml-validate-action, a2ml-pre-commit, a2ml-showcase, k9-rs, k9-deno, k9_ex, k9_gleam, k9-haskell, k9-validate-action, k9-pre-commit, k9-showcase, tree-sitter-a2ml, tree-sitter-k9, pandoc-a2ml, pandoc-k9, metadata-grammar, vscode-a2ml, vscode-k9
