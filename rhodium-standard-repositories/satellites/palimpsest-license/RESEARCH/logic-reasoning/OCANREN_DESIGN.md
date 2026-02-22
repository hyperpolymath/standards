# OCanren Logic Layer - Design Document

**Status:** FIRST PASS
**Purpose:** Use relational logic programming for license reasoning

---

## Why miniKanren/OCanren?

Traditional programming asks: "Given these inputs, what's the output?"
Relational programming asks: "Given any of these, derive the others."

For licensing, this means:

```
Traditional:
  can_use(work, purpose) → boolean

Relational:
  use_relation(work, purpose, license, consent_state)

  Can run ANY direction:
  - Given work + purpose → what licenses allow this?
  - Given license → what uses are permitted?
  - Given desired use → what consent is needed?
  - Given consent state → what can be done?
```

---

## Use Cases

### 1. License Compatibility Checking

```ocaml
(* Is Palimpsest compatible with CC-BY-SA? *)
let compatible = run q (fun q ->
  compatible_licenses "palimpsest-0.4" "cc-by-sa-4.0" q)
```

### 2. Consent Requirement Derivation

```ocaml
(* What consent do I need for AI training? *)
let requirements = run q (fun q ->
  consent_required "ai-training" q)
```

### 3. Valid Combination Generation

```ocaml
(* Generate all valid license stacks for my use case *)
let valid_stacks = run* q (fun q ->
  valid_license_stack ["commercial" "derivative" "ai-training"] q)
```

### 4. Conflict Detection

```ocaml
(* Are there contradictions in this license combination? *)
let conflicts = run q (fun q ->
  license_conflict ["gpl-3.0" "palimpsest-0.4" "proprietary"] q)
```

---

## Core Relations

```ocaml
(* License properties *)
val permits : license -> usage -> goal
val requires : license -> obligation -> goal
val prohibits : license -> usage -> goal

(* Compatibility *)
val compatible : license -> license -> goal
val incompatible : license -> license -> reason -> goal

(* Consent *)
val consent_needed : usage -> consent_type -> goal
val consent_granted : work -> consent_type -> status -> goal

(* Lineage *)
val derived_from : work -> work -> goal
val emotional_lineage : work -> lineage_marker -> goal
val cultural_context : work -> context -> goal

(* Composition *)
val valid_stack : license list -> goal
val minimal_obligations : license list -> obligation list -> goal
```

---

## Integration with Palimpsest Ontology

The logic layer needs to connect to:

1. **LKIF concepts** (legal knowledge)
2. **SPDX identifiers** (standard licenses)
3. **Palimpsest-specific** (emotional lineage, consent states)
4. **Dublin Core** (metadata)

```
┌─────────────────────────────────────┐
│         OCanren Relations           │
├─────────────────────────────────────┤
│  permits/3  requires/3  prohibits/3 │
│  compatible/2  consent_needed/2     │
└──────────────┬──────────────────────┘
               │ Grounded by
               ▼
┌─────────────────────────────────────┐
│       Palimpsest Ontology           │
├─────────────────────────────────────┤
│  License terms, consent states,     │
│  emotional lineage markers,         │
│  cultural context tags              │
└─────────────────────────────────────┘
```

---

## Questions Emerged

1. **How do we handle uncertainty?** OCanren gives definite answers. But consent can be ambiguous. Do we need probabilistic extension?

2. **How do we handle temporal aspects?** Consent can be revoked. Licenses can be updated. How does the logic layer handle time?

3. **How do we handle jurisdiction?** A license might be valid in NL but not in US. Relational or parametric?

4. **How deep does compatibility go?** Surface compatibility (terms don't conflict) vs. deep compatibility (philosophical alignment)?

5. **Can relational programming capture emotional lineage?** Or is that necessarily outside formal logic?

6. **What happens when the logic says "no solution"?** Is that "definitely prohibited" or "cannot determine"?

---

## Implementation Plan

### Phase 1: Core Relations
- [ ] Basic license properties (permits, requires, prohibits)
- [ ] SPDX license database as facts
- [ ] Simple compatibility checking

### Phase 2: Consent Layer
- [ ] Consent state representation
- [ ] Consent requirement derivation
- [ ] Temporal validity

### Phase 3: Palimpsest-Specific
- [ ] Emotional lineage relations
- [ ] Cultural context integration
- [ ] Trauma marker handling

### Phase 4: CLI Integration
- [ ] Query interface
- [ ] Combinatoric argument generation
- [ ] Integration with Nickel config

---

## Dependencies

- **OCanren**: OCaml miniKanren implementation
- **Existing OCaml parser**: For metadata input
- **SPDX license data**: As fact database
- **LKIF ontology**: For legal concepts

---

## Open Research Questions

- Can formal logic capture "emotional significance"?
- What is the computational complexity of license compatibility?
- How do we validate the logic layer against legal reality?
- Should we use constraint logic programming (CLP) for numeric constraints?
