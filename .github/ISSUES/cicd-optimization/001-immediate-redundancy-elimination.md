// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

# Issue #001: Immediate Redundancy Elimination

**Track:** CICD Optimization — Ultra-Zotta-Plan  
**Priority:** CRITICAL  
**Status:** IN PROGRESS  
**Owner:** @hyperpolymath  
**Date:** 2026-06-05  

---

## Description

Estate-wide CICD has significant redundancy causing unnecessary CI minute consumption and queuing delays. This issue tracks immediate wins that can be deployed within Week 1.

---

## Tasks

### ✅ Task 1.1: Remove trufflehog from secret-scanner (DONE)
- **File:** `standards/.github/workflows/secret-scanner-reusable.yml`
- **Change:** Deleted `trufflehog:` job (13s saved per run)
- **Status:** ✅ COMPLETED
- **Impact:** ~5,000+ minutes/month saved across 1,191 repos

### Task 1.2: Consolidate verisimdb build workflows
- **Repo:** `databases/verisimdb`
- **Problem:** Three workflows with overlapping jobs:
  - `build-validation.yml` — quick Rust + Elixir build checks
  - `rust-ci.yml` — full Rust suite (including `bench-compile`, `audit`)
  - `elixir-ci.yml` — full Elixir suite (including `bench-compile`, `hex audit`)
- **Duplicate jobs:**
  - `bench-compile` appears in both rust-ci and elixir-ci
  - `audit` appears as `cargo audit` (rust-ci) and `hex audit` (elixir-ci)
- **Action:** 
  - Option A: Delete `build-validation.yml` (rust-ci + elixir-ci already cover it)
  - Option B: Merge build-validation into language workflows with path filters
- **Estimated savings:** 2-3 minutes per PR
- **Status:** TODO
- **Priority:** HIGH

### Task 1.3: Eliminate rust-secrets waste
- **Problem:** 300+ repos run `rust-secrets` job but have NO `Cargo.toml`
- **Examples:** aspasia, bgp-backbone-lab, branch-newspaper, and ~297 others
- **Current behavior:** Job self-skips with "No Cargo.toml found" message (safe but wasteful)
- **Action:** Add conditional to rust-secrets job in `secret-scanner-reusable.yml`:
  ```yaml
  rust-secrets:
    if: >
      github.event_name != 'schedule' &&
      (contains(github.event.pull_request.changed_files, 'Cargo.toml') ||
       contains(github.event.pull_request.changed_files, '**.rs'))
    # OR simpler:
    if: hashFiles('**/Cargo.toml') != ''
  ```
- **Estimated savings:** ~300 repos × 3s = 900s per estate-wide push
- **Status:** TODO
- **Priority:** HIGH

### Task 1.4: Schedule-heavy workflows
- **Problem:** `codeql.yml`, `scorecard.yml`, `hypatia-scan.yml` run on every push
- **Action:** Change to weekly schedule, keep push trigger for main
- **Estimated savings:** ~5-10 minutes per push
- **Status:** TODO
- **Priority:** MEDIUM

---

## Success Criteria

- [ ] verisimdb has no duplicate job names across workflows
- [ ] rust-secrets runs only on repos with Rust code
- [ ] CI minutes reduced by >20% from Week 0 baseline

---

## Dependencies

- None (independent tasks)

---

## Tags

`cicd-optimization`, `redundancy`, `cost-reduction`, `week-1`, `high-priority`, `track-1`

---

## Related

- Roadmap: `2026-06-05-cicd-optimization-roadmap.md`
- Issue #002: Workflow Naming + Clarity
- Issue #003: Path Filter Optimization
