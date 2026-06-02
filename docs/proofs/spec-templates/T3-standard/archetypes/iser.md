# Archetype: iser (Code Generator)
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

**Applies to**: All `*-iser` repos (29 total) — code generators from source language to target representation

## Common Proofs

Every `-iser` repo needs these theorems:

### ISER-1: Template substitution safety

**Target**: `verification/proofs/idris2/TemplateSubst.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: Placeholder substitution (`{{VAR}}` → value) never introduces syntax errors in output.

**Formal signature**:
```idris
data Template = Lit String | Hole String | Seq Template Template
substitute : (Template, Map String String) -> String
export
substSyntaxSafe : (t : Template) -> (m : Map String String) ->
                   ValidSyntax (substitute t m)
```

**Obligations**:
- [ ] Define Template AST
- [ ] Prove substitution preserves syntactic well-formedness of output

---

### ISER-2: Output validity

**Target**: `verification/proofs/idris2/OutputValid.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: Generated code parses in the target language.

**Obligations**:
- [ ] Model target language grammar
- [ ] Prove generator output matches grammar

---

### ISER-3: Idempotence

**Target**: `verification/proofs/idris2/GenIdem.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**: Re-running the generator on unchanged input produces identical output.

**Obligations**:
- [ ] Prove `generate(generate(input)) = generate(input)` where sensible

---

### ISER-4: No injection

**Target**: `verification/proofs/idris2/NoInjection.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**: User-provided identifiers are escaped; cannot inject code.

**Obligations**:
- [ ] Define Identifier as escaped string type
- [ ] Prove escape is applied before concatenation

---

## Plus: Mandatory ABI proofs

ABI-1 through ABI-5 from rsr-template-repo.

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/<REPO>
just proof-check-all
```

## Repos using this archetype

a2mliser, affinescriptiser, alloyiser, anvomidaviser, atsiser, betlangiser, bqniser, chapeliser, dafniser, eclexiaiser, ephapaxiser, futharkiser, halideiser, idrisiser, iseriser, julianiser, k9iser, lustreiser, mylangiser, nimiser, oblibeniser, otpiser, phronesiser, ponyiser, rescript-evangeliser, tlaiser, typedqliser, verisimiser, wokelangiser
