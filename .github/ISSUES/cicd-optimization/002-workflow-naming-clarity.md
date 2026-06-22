// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

# Issue #002: Workflow Naming + Clarity Standardization

**Track:** CICD Optimization — Ultra-Zotta-Plan  
**Priority:** HIGH  
**Status:** TODO  
**Owner:** @hyperpolymath  
**Date:** 2026-06-05  

---

## Description

Current workflow naming across the estate is inconsistent and unclear, making it difficult to:
- Understand what each workflow does
- Identify redundancies
- Maintain consistency across repos
- Onboard new contributors

---

## Current State: Chaos

### Inconsistent Patterns
```
rust-ci.yml                    # Good: language-purpose
elixir-ci.yml                  # Good: language-purpose
build-validation.yml           # Bad: purpose only, unclear scope
secret-scanner.yml            # OK: purpose only
security-scan.yml             # Overlaps with secret-scanner?
scorecard.yml                 # OK: tool name
scorecard-enforcer.yml        # What's the difference?
governance.yml                # Too broad
hypatia-scan.yml              # OK: tool name
workflow-linter.yml           # OK: purpose
mirror.yml                     # Too vague
cflite_batch.yml              # Project-specific, unclear
cflite_pr.yml                 # Project-specific, unclear
```

### Duplicate/Overlapping Names
- `secret-scanner.yml` vs `security-scan.yml`
- `scorecard.yml` vs `scorecard-enforcer.yml`
- `build-validation.yml` vs `rust-ci.yml` vs `elixir-ci.yml`

---

## Proposed Standard

### Naming Convention

**Format:** `<language>-<purpose>.yml` OR `<purpose>-<language>.yml`

**Language prefixes:**
- `rust-` — Rust language
- `elixir-` — Elixir language
- `go-` — Go language
- `javascript-` / `typescript-` — JS/TS
- `python-` — Python
- `java-` — Java
- `c-` / `cpp-` — C/C++

**Purpose suffixes:**
- `-build` — Compilation only
- `-test` — Testing only
- `-build-test` — Compilation + testing
- `-lint` — Linting/formatting
- `-audit` — Security auditing
- `-fuzz` — Fuzzing
- `-bench` — Benchmarks
- `-docs` — Documentation

**Category prefixes (for non-language-specific):**
- `security-` — Security-related
- `governance-` — Governance/policy
- `lint-` — Linting/formatting
- `test-` — Testing
- `build-` — Build-related
- `deploy-` — Deployment
- `sync-` — Synchronization
- `scan-` — Scanning (security/secrets)

### Examples of Good Names

```
# Language-specific
rust-build-test.yml        # Rust: build + test
rust-lint.yml              # Rust: clippy + rustfmt
elixir-build-test.yml      # Elixir: mix compile + test
go-build-test.yml          # Go: build + test

# Security
security-secret-scan.yml   # Secret detection (gitleaks)
security-codeql.yml        # CodeQL analysis
security-scorecard.yml     # OSSF Scorecard
security-trivy.yml         # Container scanning

# Governance
governance-license.yml    # License compliance
governance-workflow.yml   # Workflow validation
governance-spdx.yml       # SPDX header checks

# Build/Deploy
build-validation.yml      # Quick build check (keep if cross-language)
deploy-github-pages.yml   # GitHub Pages deployment
deploy-container.yml      # Container image build + push
```

### Required Metadata in Every Workflow

Add to the top of every workflow file:

```yaml
# SPDX-License-Identifier: MPL-2.0
name: Rust — Build + Test + Lint
# ==== METADATA ====
# Purpose: Validates Rust code compiles, passes tests, and clippy lint
# Language: Rust
# Owner: @hyperpolymath
# Schedule: On push/PR (path-filtered to Rust files)
# Timeout: 30m total
# Est. Cost: 5-8 minutes per run
# Paths: **/*.rs, Cargo.toml, Cargo.lock
# ==== /METADATA ====
```

---

## Tasks

### Task 2.1: Rename workflows in standards repo
- **Action:** Rename workflows in `standards/.github/workflows/` to follow convention
- **Examples:**
  - `secret-scanner.yml` → `security-secret-scan.yml`
  - `build-validation.yml` → `build-validation.yml` (keep, it's OK)
  - `scorecard-enforcer.yml` → `security-scorecard-enforcer.yml`
- **Status:** TODO
- **Priority:** MEDIUM

### Task 2.2: Create naming lint rule
- **Action:** Add workflow-linter rule to enforce naming convention
- **Check:** Workflow filename matches regex: `^[a-z]+(-[a-z]+)+\.yml$`
- **Status:** TODO
- **Priority:** MEDIUM

### Task 2.3: Estate-wide rename propagation
- **Action:** Script to rename workflows across all repos
- **Approach:** Use `gh api` to list repos, then batch update
- **Status:** TODO
- **Priority:** LOW (after standards repo is done)

---

## Success Criteria

- [ ] All new workflows follow naming convention
- [ ] All existing workflows have metadata comments
- [ ] No duplicate/overlapping workflow names in any repo
- [ ] Naming lint rule prevents regressions

---

## Tags

`cicd-optimization`, `naming`, `clarity`, `standards`, `week-1`, `high-priority`, `track-2`

---

## Related

- Roadmap: `2026-06-05-cicd-optimization-roadmap.md`
- Issue #001: Immediate Redundancy Elimination
- Issue #003: Path Filter Optimization
