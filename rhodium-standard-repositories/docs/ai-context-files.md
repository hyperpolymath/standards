# AI Context Files Specification

**Version**: 1.0.0
**Last Updated**: 2025-12-15
**Purpose**: Standardize AI assistant context files for RSR-compliant repositories

---

## Overview

RSR repositories include three machine-readable Scheme files (`.scm`) that provide structured context for AI assistants, enabling continuity across sessions and consistent project understanding. These files work alongside traditional AI instruction files like `.claude/CLAUDE.md`.

### The Three SCM Files

| File | Purpose | Spec Repository |
|------|---------|-----------------|
| `META.scm` | Architecture decisions, development practices, design rationale | [hyperpolymath/META.scm](https://github.com/hyperpolymath/META.scm) |
| `ECOSYSTEM.scm` | Project relationships, ecosystem position, boundaries | [hyperpolymath/ECOSYSTEM.scm](https://github.com/hyperpolymath/ECOSYSTEM.scm) |
| `STATE.scm` | Current project state, progress, blockers, session history | [hyperpolymath/state.scm](https://github.com/hyperpolymath/state.scm) |

---

## Why Scheme S-Expressions?

1. **Human-readable**: Clear structure without JSON noise
2. **Machine-parseable**: Well-defined grammar for tooling
3. **VCS-friendly**: Text-based, diff-friendly format
4. **Homoiconic**: Code-as-data enables meta-programming
5. **Lisp tradition**: Aligns with RSR's reflexivity principles

---

## File Specifications

### META.scm — Architecture Decisions

**Purpose**: Captures Architecture Decision Records (ADRs), development practices, and design rationale in structured form.

**Core Sections**:
- `architecture-decisions` — ADRs with status, date, context, decision, consequences
- `development-practices` — Code style, security, testing, versioning, documentation
- `design-rationale` — Explanatory "why" narratives

**Example**:
```scheme
;; SPDX-License-Identifier: MPL-2.0-or-later
(define-module (project-name meta)
  #:export (architecture-decisions development-practices design-rationale))

(define architecture-decisions
  '((adr-001
     (title . "Use Rust for Core Services")
     (status . "accepted")
     (date . "2025-01-15")
     (context . "Need memory-safe systems programming")
     (decision . "Implement core in Rust with Ada/SPARK for critical paths")
     (consequences . ("Memory safety without GC"
                      "Excellent performance"
                      "RSR Gold compliance")))))

(define development-practices
  '((code-style
     (formatter . "rustfmt")
     (linter . "clippy")
     (line-length . 100))
    (security
     (sast . "CodeQL + Semgrep")
     (credentials . "Environment variables only"))))

(define design-rationale
  '((why-rust
     "Rust provides memory safety without garbage collection overhead...")))
```

**ADR Status Values**: `proposed`, `accepted`, `deprecated`, `superseded`, `rejected`

---

### ECOSYSTEM.scm — Project Relationships

**Purpose**: Declarative format describing project ecosystem position, relationships, and explicit boundaries.

**Core Sections**:
- `version`, `name`, `type`, `purpose` — Identity
- `position-in-ecosystem` — Where this project fits
- `related-projects` — Typed relationships to other projects
- `what-this-is` / `what-this-is-not` — Explicit boundaries

**Relationship Types**:
- `sibling-standard` — Related specification
- `sibling` — Related project in same ecosystem
- `potential-consumer` — Might use this project
- `inspiration` — Influenced design
- `standard` — Follows this standard

**Example**:
```scheme
;; SPDX-License-Identifier: MPL-2.0-or-later
(ecosystem
  (version "1.0.0")
  (name "my-project")
  (type "library")
  (purpose "High-performance data processing")

  (position-in-ecosystem
    "Part of the hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project
      (name "rhodium-standard-repositories")
      (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
      (relationship "standard")
      (description "RSR compliance guidelines")))

  (what-this-is
    "A library for processing structured data efficiently.")

  (what-this-is-not
    "- NOT a database
     - NOT a web framework
     - NOT a standalone application"))
```

---

### STATE.scm — Project Checkpoint

**Purpose**: Stateful Context Tracking Engine for AI conversation continuity. Preserves project state across sessions.

**Core Sections**:
- `metadata` — Version, timestamps, project identity
- `project-context` — Name, tagline, tech stack
- `current-position` — Phase, completion %, component status
- `route-to-mvp` — Milestones with items
- `blockers-and-issues` — Prioritized blockers (critical/high/medium/low)
- `critical-next-actions` — Immediate, this-week, this-month
- `session-history` — Snapshots with accomplishments

**Example**:
```scheme
;;; STATE.scm - Project Checkpoint
(define metadata
  '((version . "0.2.0")
    (schema-version . "1.0")
    (created . "2025-01-01")
    (updated . "2025-12-15")
    (project . "my-project")))

(define current-position
  '((phase . "v0.2 - Core Implementation")
    (overall-completion . 45)
    (components
     ((core-engine ((status . "functional") (completion . 70)))
      (api-layer ((status . "in-progress") (completion . 30)))))))

(define blockers-and-issues
  '((critical ())
    (high-priority
     ((missing-tests
       ((description . "Limited test coverage")
        (impact . "Risk of regressions")
        (needed . "Test suite implementation")))))))

(define critical-next-actions
  '((immediate
     (("Complete API implementation" . high)
      ("Add unit tests" . high)))
    (this-week
     (("Documentation update" . medium)))))
```

---

## Integration with AI Assistant Files

### Relationship Hierarchy

```
┌─────────────────────────────────────────────────────────────┐
│                    AI Context Stack                         │
├─────────────────────────────────────────────────────────────┤
│  .claude/CLAUDE.md      │ AI-specific instructions          │
│  (or similar)           │ Commands, preferences, workflows  │
├─────────────────────────────────────────────────────────────┤
│  META.scm               │ Architecture & Practices          │
│                         │ ADRs, coding standards, rationale │
├─────────────────────────────────────────────────────────────┤
│  ECOSYSTEM.scm          │ Project Relationships             │
│                         │ Dependencies, boundaries, context │
├─────────────────────────────────────────────────────────────┤
│  STATE.scm              │ Current State (Dynamic)           │
│                         │ Progress, blockers, next actions  │
└─────────────────────────────────────────────────────────────┘
```

### Session Protocol

**Session Start**:
1. Read `STATE.scm` for current position and blockers
2. Read `META.scm` for architecture context and practices
3. Read `ECOSYSTEM.scm` for relationships and boundaries
4. Read `.claude/CLAUDE.md` for AI-specific instructions

**During Session**:
- Update `STATE.scm` as tasks complete
- Reference `META.scm` for architectural decisions
- Consult `ECOSYSTEM.scm` for integration context

**Session End**:
- Update `STATE.scm` with accomplishments
- Add session snapshot to `session-history`
- Update completion percentages

### Example: `.claude/CLAUDE.md` Integration

```markdown
## Checkpoint File Protocol

At session start, read these files if they exist:

1. **STATE.scm** — Current project state, progress, blockers
2. **ECOSYSTEM.scm** — Project relationships and boundaries  
3. **META.scm** — Architecture decisions and practices

Update STATE.scm throughout the session as work completes.
```

---

## RSR Compliance Requirements

### Required Files (RSR Gold)

- [ ] `META.scm` — Architecture decisions documented
- [ ] `ECOSYSTEM.scm` — Project relationships defined
- [ ] `STATE.scm` — Current state tracked
- [ ] SPDX headers on all three files

### Validation

```bash
# Check for presence
test -f META.scm && test -f ECOSYSTEM.scm && test -f STATE.scm

# Validate SPDX headers
rg "SPDX-License-Identifier" META.scm ECOSYSTEM.scm STATE.scm

# Parse structure (requires Guile)
guile -c '(load "META.scm") (display architecture-decisions)'
```

---

## Tools & Automation

### Batch Generation

Use `add-scm-files.jl` (Julia) to generate all three files for repositories:

```bash
julia add-scm-files.jl --dry-run           # Preview
julia add-scm-files.jl                      # Apply to all repos
julia add-scm-files.jl --repo my-project    # Single repo
```

### Robot-Cleaner Integration

SCM files can be auto-generated during git-hud daily scans using the templates from specification repositories.

---

## References

- [META.scm Specification](https://github.com/hyperpolymath/META.scm) — Full format spec
- [ECOSYSTEM.scm Specification](https://github.com/hyperpolymath/ECOSYSTEM.scm) — Ecosystem format
- [STATE.scm Specification](https://github.com/hyperpolymath/state.scm) — State tracking
- [git-hud STATE.scm](https://github.com/hyperpolymath/git-hud/blob/main/STATE.scm) — Reference implementation

---

*"Context preservation enables continuity. Continuity enables progress."*

— The Rhodium Standard
