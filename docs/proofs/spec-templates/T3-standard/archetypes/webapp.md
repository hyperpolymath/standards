# Archetype: webapp (Web Application)
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Applies to**: ReScript web applications with TEA architecture

## Common Proofs

### WEB-1: State machine well-formedness

**Target**: `verification/proofs/idris2/StateMachine.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: All state transitions documented. No undefined transitions.

**Obligations**:
- [ ] Enumerate states
- [ ] Define ValidTransition GADT
- [ ] Prove update function covers all (state, msg) pairs

---

### WEB-2: XSS freedom

**Target**: `verification/proofs/idris2/XSSFree.idr`
**Prover**: Idris2
**Priority**: P0

**Statement**: User-provided content is escaped before DOM insertion.

**Obligations**:
- [ ] Define EscapedString type
- [ ] Prove DOM insertion requires EscapedString

---

### WEB-3: API contract compliance

**Target**: `verification/proofs/idris2/APIContract.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**: Client requests match server OpenAPI/schema.

**Obligations**:
- [ ] Model schema
- [ ] Prove request construction validates

---

### WEB-4: No CSRF bypasses

**Target**: `verification/proofs/idris2/CSRFProtect.idr`
**Prover**: Idris2
**Priority**: P1

**Statement**: State-changing requests include CSRF token.

**Obligations**:
- [ ] Model request types
- [ ] Prove token presence for mutating requests

---

## Plus: Mandatory ABI proofs

ABI-1 through ABI-5.

## Repos using this archetype

aerie, rescript-tea, rescript-vite, rescript-dom-mounter, nafa-app, project-wharf, lcb-website, civic-connect, accessibility-everywhere, academic-workflow-suite, double-track-browser, grim-repo, hyperpolymath.github.io, feedback-o-tron, branch-newspaper, blocky-writer
