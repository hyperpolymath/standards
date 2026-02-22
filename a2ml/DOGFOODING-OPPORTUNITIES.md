# A2ML Dogfooding Opportunities

**Date:** 2026-01-30
**Status:** Strategy Document
**A2ML Version:** 0.6.0 (Prototype)

---

## Philosophy

A2ML is designed to provide **attested markup with formal structural guarantees**. The best way to prove its value is to use it extensively within the Hyperpolymath ecosystem that created it.

**Core Principle:** If A2ML can't document itself and the ecosystem it was built for, it's not ready for general use.

---

## Current Status

- **Version:** 0.6.0
- **Phase:** Prototype (60% complete)
- **Tech Stack:** Idris2 (typed core), ReScript (web prototype + WASM)
- **What Works:** Surface grammar, parser, validator, vector tests, ddraig-ssg integration

---

## Strategic Dogfooding Targets

### 1. **A2ML Self-Documentation** (Meta-Dogfooding)

**Priority:** CRITICAL
**Files to Convert:**
- `README.adoc` → `README.a2ml`
- `SPEC.adoc` → `SPEC.a2ml`
- `docs/*.adoc` → `docs/*.a2ml`

**Why This Matters:**
- If A2ML can't formally verify its own documentation structure, how can it verify others?
- Demonstrates progressive strictness (start lax, add attestation)
- Proves the format works for complex technical documentation

**Benefits:**
- Abstract sections required (can't skip overview)
- References automatically validated (no broken links to spec sections)
- Section structure enforced (grammar, conformance, examples)
- Version attestation (docs match implementation version)

**Example:**
```a2ml
# A2ML Specification v0.6.0

@abstract:
A2ML is a lightweight, Djot-like markup that compiles into a typed, attested core.
It provides formal structural guarantees while keeping authoring simple.
@end

@version-attestation:
spec-version = "0.6.0"
implementation-version = "0.6.0"
status = "draft"
@end

## Core Concepts

### Progressive Strictness

@definition:
A2ML supports three strictness levels:
1. **Lax**: Minimal validation, maximum flexibility
2. **Checked**: Structure validated, references resolved
3. **Attested**: Cryptographically signed with formal proofs
@end

@refs:
[1] docs/MODULES.adoc (Syntax Modules)
[2] docs/CONFORMANCE.adoc (Conformance Testing)
@end
```

**Action Items:**
- [ ] Convert README.adoc to README.a2ml (use lax mode initially)
- [ ] Convert SPEC.adoc to SPEC.a2ml (use checked mode)
- [ ] Add attestation proofs for release versions
- [ ] Ensure all internal references validate

---

### 2. **Hyperpolymath RSR Documentation Standard**

**Priority:** HIGH
**Target:** All 500+ Hyperpolymath repositories

**What to Convert:**
- `README.adoc` → `README.a2ml` (all repos)
- `STATE.scm` → `STATE.a2ml` (checkpoint files)
- `ECOSYSTEM.scm` → `ECOSYSTEM.a2ml` (relationships)
- `META.scm` → `META.a2ml` (ADRs and decisions)

**Why This Matters:**
- RSR requires specific documentation structure
- A2ML can **enforce** this structure formally
- Prevents incomplete or malformed documentation
- Automatic validation in CI/CD

**Benefits:**
- Required sections enforced (project-context, current-position, route-to-mvp)
- Metadata validated (version, dates, repo name)
- Cross-references between STATE/ECOSYSTEM/META validated
- Breaking changes to checkpoint format caught at parse time

**Example STATE.a2ml:**
```a2ml
# Project State - A2ML

@metadata:
version = "0.6.0"
schema-version = "1.0"
created = "2026-01-26"
updated = "2026-01-30"
project = "a2ml"
repo = "hyperpolymath/a2ml"
@end

@project-context:
name = "A2ML"
tagline = "Attested Markup Language"
tech-stack = ["spec", "idris2", "rescript"]
@end

@current-position:
phase = "prototype"
overall-completion = 60
working-features = [
  "Surface grammar (draft)",
  "Typed core outline",
  "ReScript web prototype",
  "Vector suite passing"
]
@end

@route-to-mvp:
## Milestone: Core Implementation
- [ ] Complete Idris2 typed core
- [ ] Full reference resolver
- [ ] Attestation signing system

## Milestone: Production Ready
- [ ] 1.0.0 release
- [ ] Full test coverage
- [ ] Documentation complete
@end
```

**Challenge:**
- STATE/ECOSYSTEM/META are currently Scheme (executable)
- A2ML is markup (data)
- **Solution:** Hybrid approach or A2ML with embedded Scheme blocks

**Action Items:**
- [ ] Design STATE.a2ml schema with required sections
- [ ] Prototype STATE.a2ml in a2ml repo first
- [ ] Create validation rules for checkpoint files
- [ ] Add A2ML validation to RSR CI workflows

---

### 3. **ABI/FFI Documentation**

**Priority:** HIGH
**Target:** All ABI/FFI repos with Idris2 ABIs

**What to Document:**
- Type definitions with formal proofs
- Memory layout verification
- Platform-specific ABIs
- FFI interface contracts

**Why This Matters:**
- ABI documentation must match ABI implementation
- A2ML can enforce documentation structure
- Formal proofs in docs mirror formal proofs in code
- Documentation becomes part of the verification chain

**Benefits:**
- Required sections: Types, Layout, Foreign, Proofs
- References between docs and code validated
- Proof blocks structured and checked
- Breaking ABI changes require doc updates (enforced)

**Example ABI Documentation:**
```a2ml
# Container ABI Documentation

@abstract:
This ABI defines the interface for container runtime operations with formal
correctness proofs. All types are verified at compile-time.
@end

@abi-version:
version = "1.0.0"
idris-version = "0.7.0"
platform = "linux-x86_64"
@end

## Types

### Handle

Non-null pointer guaranteed at type level.

@proof:
type = "Handle"
invariant = "ptr /= 0"
proof-term = "nonNull : So (ptr /= 0)"
source = "src/abi/Types.idr:15-20"
@end

@code:
```idris
data Handle : Type where
  MkHandle : (ptr : Bits64) -> {auto 0 nonNull : So (ptr /= 0)} -> Handle
```
@end

### Platform-Specific CInt

@proof:
type = "CInt"
platforms = {
  "linux" = "Bits32",
  "windows" = "Bits32",
  "macos" = "Bits32"
}
verified = true
source = "src/abi/Types.idr:45-48"
@end

## Memory Layout

@verification:
struct = "ContainerState"
total-size = 64
alignment = 8
fields = [
  { name = "pid", offset = 0, size = 4 },
  { name = "status", offset = 4, size = 4 },
  { name = "timestamp", offset = 8, size = 8 }
]
proof = "layoutCorrect : HasSize ContainerState 64"
source = "src/abi/Layout.idr:30-42"
@end

@refs:
[1] src/abi/Types.idr - Type definitions
[2] src/abi/Layout.idr - Memory layout proofs
[3] src/abi/Foreign.idr - FFI declarations
[4] Idris2 Dependent Types Manual
@end
```

**Action Items:**
- [ ] Create ABI-FFI-README.a2ml template
- [ ] Convert zig-*-ffi repo docs to A2ML
- [ ] Add A2ML validation to ABI CI
- [ ] Ensure doc updates when ABI changes

---

### 4. **Migration Guides and Technical Documentation**

**Priority:** MEDIUM
**Target:** Technical guides, tutorials, RFCs

**What to Convert:**
- `ffi-migration-guide.md` → `.a2ml`
- `abi-migration-guide.md` → `.a2ml`
- All tutorial and guide documents
- RFC and specification documents

**Why This Matters:**
- Migration guides must be complete (can't skip critical steps)
- Tutorial structure enforced (prerequisites, steps, verification)
- RFCs require specific sections (abstract, motivation, specification)

**Benefits:**
- Required sections enforced (overview, prerequisites, steps, troubleshooting)
- Checklists validated (all items present)
- Code examples required and structured
- Version-specific content clearly marked

**Example Migration Guide:**
```a2ml
# FFI Migration Guide: Rust to Zig

@abstract:
This guide covers migrating Foreign Function Interface implementations from
Rust to Zig across 5 Hyperpolymath repositories. It includes patterns,
gotchas, and a complete checklist.
@end

@metadata:
version = "1.0.0"
applies-to = ["wokelang", "valence-shell", "volumod", "vordr", "echidna"]
created = "2026-01-30"
@end

## Prerequisites

@checklist:
- [ ] Zig 0.11.0+ installed
- [ ] Idris2 0.7.0+ installed
- [ ] Existing Rust FFI understood
- [ ] ABI documentation available
@end

## Migration Steps

### Step 1: Analyze Existing FFI

@required-actions:
1. Read current Rust implementation
2. Document all exported functions
3. Identify unsafe blocks
4. List dependencies
@end

@code-example:
```rust
// Before: Rust FFI
#[no_mangle]
pub extern "C" fn init() -> *mut Handle {
    Box::into_raw(Box::new(Handle::new()))
}
```
@end

### Step 2: Port to Zig

@code-example:
```zig
// After: Zig FFI
export fn init() ?*Handle {
    const allocator = std.heap.c_allocator;
    const handle = allocator.create(Handle) catch return null;
    handle.* = Handle.init();
    return handle;
}
```
@end

@gotchas:
- Zig uses `?T` for nullable pointers (Rust uses `*mut T`)
- Zig allocators explicit (Rust uses global allocator)
- Zig error unions different from Rust Result
@end

## Verification

@verification-checklist:
- [ ] All functions exported
- [ ] Types match ABI
- [ ] Tests pass
- [ ] Memory leaks checked
- [ ] Cross-platform build tested
@end

@refs:
[1] Zig Language Reference - https://ziglang.org/documentation/master/
[2] src/abi/*.idr - ABI definitions
[3] Original Rust FFI implementation
@end
```

**Action Items:**
- [ ] Convert ffi-migration-guide.md to A2ML
- [ ] Convert abi-migration-guide.md to A2ML
- [ ] Create tutorial template in A2ML
- [ ] Add A2ML validation for all guides

---

### 5. **ddraig-ssg Integration**

**Priority:** HIGH
**Target:** ddraig-ssg static site generator

**What to Build:**
- Native A2ML input support (already has prototype!)
- A2ML → HTML/PDF rendering
- A2ML → academic paper format
- Structured blog posts

**Why This Matters:**
- ddraig-ssg is written in Idris2 (same as A2ML core)
- Static sites with formally verified structure
- Academic papers with guaranteed format
- Blog posts that enforce content structure

**Benefits:**
- Blog posts require abstract/content/conclusion
- Academic papers follow required format
- References automatically validated
- Navigation generated from structure

**Example Blog Post:**
```a2ml
# Universal Idris2 ABI + Zig FFI Standard

@metadata:
date = "2026-01-30"
author = "Jonathan D.A. Jewell"
tags = ["idris2", "zig", "ffi", "formal-verification"]
category = "technical"
@end

@abstract:
We've established a universal standard for ABIs and FFIs across the Hyperpolymath
ecosystem. Idris2 provides proven-correct ABIs, Zig provides memory-safe FFI,
and generated C headers bridge them.
@end

## Introduction

[Content...]

## Architecture

@diagram:
type = "ascii-art"
shows = "ABI → Headers → FFI flow"
@end

## Benefits

@list:
- Mathematically proven ABIs
- Memory-safe FFI implementation
- Cross-platform support
- Universal C compatibility
@end

## Conclusion

[Content...]

@refs:
[1] rsr-template-repo/ABI-FFI-README.md
[2] Idris2 Documentation
[3] Zig Documentation
@end
```

**Action Items:**
- [ ] Expand ddraig-ssg A2ML support beyond prototype
- [ ] Create A2ML blog post template
- [ ] Create A2ML academic paper template
- [ ] Generate this repo's documentation site with ddraig-ssg

---

### 6. **Architecture Decision Records (ADRs)**

**Priority:** MEDIUM
**Target:** META.scm files in all repos

**What to Structure:**
- ADR format (context, decision, consequences, status)
- Status values (proposed/accepted/deprecated/superseded)
- Cross-references between related ADRs

**Why This Matters:**
- ADRs require specific structure
- Status transitions must be valid
- Superseded ADRs must reference replacements
- Date ordering enforced

**Benefits:**
- ADR format enforced automatically
- Status validation (can't use invalid status)
- Required fields present (date, context, decision)
- Cross-references validated

**Example ADR in A2ML:**
```a2ml
# ADR-001: Universal Idris2 ABI Standard

@metadata:
adr-number = 1
status = "accepted"
date = "2026-01-30"
supersedes = []
superseded-by = []
@end

@context:
We have 22 ABI/FFI repositories with inconsistent interface definitions.
Some use Zig ABI, some use Idris2, some use C headers directly. This
creates maintenance burden and prevents formal verification.
@end

@decision:
Establish Idris2 as the universal ABI definition language for all
Hyperpolymath projects. Key points:

1. All ABIs written in Idris2 with dependent types
2. Formal proofs of correctness required
3. C headers auto-generated from Idris2
4. FFI implementations in Zig consume generated headers
@end

@consequences:

## Positive
- Mathematically proven interface correctness
- Memory layout verification at compile-time
- Platform compatibility proven, not tested
- Single source of truth for ABIs

## Negative
- Learning curve for Idris2
- Build process more complex (generate headers)
- Idris2 compiler required in toolchain

## Neutral
- Migration required for 6 repos
- Templates needed for new repos
@end

@refs:
[1] abi-migration-guide.md
[2] rsr-template-repo/src/abi/
[3] Discussion: https://github.com/hyperpolymath/standards/issues/42
@end
```

**Action Items:**
- [ ] Create ADR.a2ml template
- [ ] Convert existing META.scm ADRs to A2ML
- [ ] Add ADR validation to CI
- [ ] Document ADR process with A2ML

---

## Progressive Adoption Strategy

### Phase 1: Self-Dogfooding (A2ML v0.6.0 - v1.0.0)

**Focus:** Use A2ML to document itself

1. Convert A2ML's own documentation to A2ML
2. Use lax mode initially (minimal validation)
3. Progressively add strictness as format stabilizes
4. Prove the format works for complex technical docs

**Success Metrics:**
- A2ML README, SPEC, and guides in A2ML format
- All internal references validate
- Documentation CI passes
- Contributors find it easier than AsciiDoc

### Phase 2: Ecosystem Standards (A2ML v1.0.0+)

**Focus:** Standardize Hyperpolymath documentation

1. Create templates for STATE/ECOSYSTEM/META
2. Convert rsr-template-repo docs
3. Pilot in 5-10 repos
4. Gather feedback and iterate

**Success Metrics:**
- RSR documentation CI validates A2ML
- Incomplete docs caught automatically
- 50+ repos using A2ML
- Reduced documentation errors

### Phase 3: ABI/FFI Documentation (A2ML v1.1.0+)

**Focus:** Formal verification chain

1. A2ML ABI documentation templates
2. Convert all zig-*-ffi docs
3. Link docs to code with proofs
4. Validate docs in ABI CI

**Success Metrics:**
- ABI changes require doc updates (enforced)
- Proof blocks validated
- 100% ABI documentation coverage
- Docs match code (verified)

### Phase 4: Full Adoption (A2ML v2.0.0+)

**Focus:** A2ML as default format

1. All new repos start with A2ML
2. Gradual migration of existing repos
3. ddraig-ssg native A2ML support
4. External adoption begins

**Success Metrics:**
- 500+ repos using A2ML
- A2ML in rsr-template-repo as default
- Blog/paper templates widely used
- External projects adopt A2ML

---

## Technical Requirements

### For A2ML to Be Dogfoodable

**Must Have:**
- [ ] Stable surface syntax (no breaking changes)
- [ ] Reference resolution working
- [ ] Validation errors clear and actionable
- [ ] Tooling: a2ml validate, a2ml convert
- [ ] CI integration (GitHub Actions)

**Should Have:**
- [ ] IDE support (VS Code extension)
- [ ] Syntax highlighting
- [ ] Auto-completion
- [ ] Convert from AsciiDoc/Markdown

**Nice to Have:**
- [ ] Live preview in browser
- [ ] Documentation generator
- [ ] Attestation signing built-in

### Tooling Gaps to Fill

1. **Converter:** `a2ml convert README.adoc README.a2ml`
2. **Validator:** `a2ml validate --strict README.a2ml`
3. **CI Action:** `actions/a2ml-validate@v1`
4. **Preview:** `a2ml preview README.a2ml` (local server)
5. **Generator:** `a2ml generate --template=STATE.a2ml`

---

## Success Criteria

A2ML dogfooding is successful when:

1. **Self-Documentation:** A2ML's own docs are in A2ML format
2. **RSR Standard:** A2ML is the default documentation format in rsr-template-repo
3. **ABI Coverage:** All ABI/FFI repos document interfaces in A2ML
4. **Reduced Errors:** Incomplete/malformed docs caught automatically
5. **External Interest:** Other projects ask to adopt A2ML

---

## Risks and Mitigations

### Risk: Format Not Yet Stable

**Impact:** Breaking changes require re-writing docs
**Mitigation:** Use lax mode, version documents clearly, automated migration tools

### Risk: Tooling Gaps

**Impact:** Hard to use without good tools
**Mitigation:** Build essential tools first (validator, converter), improve incrementally

### Risk: Learning Curve

**Impact:** Contributors struggle with new format
**Mitigation:** Clear templates, good error messages, fallback to Markdown/AsciiDoc initially

### Risk: Maintenance Burden

**Impact:** Maintaining docs in two formats
**Mitigation:** Automated conversion, progressive migration, keep old format until confident

---

## Conclusion

A2ML dogfooding is **essential** for proving the format's viability. By using it extensively within the Hyperpolymath ecosystem, we demonstrate:

1. **It works for complex documentation** (specs, guides, ADRs)
2. **It catches real errors** (incomplete docs, broken references)
3. **It scales** (500+ repos)
4. **It integrates** (CI, generators, validators)

**Recommended Start:** Convert A2ML's own README and SPEC to A2ML format using lax mode. This proves the format can handle its own complexity.

**Next Steps:**
1. Create A2ML versions of key documents (README, SPEC)
2. Build essential tooling (validator, converter)
3. Pilot in 5 repos to gather feedback
4. Iterate based on real usage

---

**Document Status:** Ready for Implementation
**Next Review:** After A2ML v1.0.0 release
**Maintainer:** Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
