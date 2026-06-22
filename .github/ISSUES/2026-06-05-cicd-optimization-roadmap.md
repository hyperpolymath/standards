// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

# CICD Optimization Roadmap — Ultra-Zotta-Plan

**Date:** 2026-06-05  
**Status:** DRAFT  
**Owner:** estate-wide  
**Priority:** CRITICAL (cost + velocity blocker)

---

## Executive Summary

The estate currently runs **~281 secret-scanner deployments** + **redundant build workflows** across 1,191+ repos. Analysis of `verisimdb#114` reveals **duplicate job names** (`bench-compile`, `audit`) across `rust-ci.yml`, `elixir-ci.yml`, and `build-validation.yml`, causing CI queuing delays and unnecessary minute consumption.  

**Estimated waste:** ~30-40% of CI minutes from redundancy + path-filter misses.

---

## Track 1: Immediate Redundancy Elimination (Week 1)

### 1.1 Remove trufflehog from secret-scanner ✅ **DONE**
- **File:** `standards/.github/workflows/secret-scanner-reusable.yml`
- **Change:** Deleted `trufflehog:` job (13s saved per run × 1,191 repos)
- **Impact:** ~5,000+ minutes/month saved

### 1.2 Consolidate build workflows in verisimdb
- **Problem:** `build-validation.yml` + `rust-ci.yml` + `elixir-ci.yml` = duplicate `bench-compile` + `audit` jobs
- **Action:** 
  - Merge `build-validation.yml` into `rust-ci.yml` and `elixir-ci.yml` with path filters
  - OR: Delete `build-validation.yml`, rely on full CI workflows
  - **Estimated savings:** ~2-3 minutes per PR

### 1.3 Rust-secrets waste elimination
- **Problem:** 300+ repos run `rust-secrets` but have NO `Cargo.toml` (aspasia, bgp-backbone-lab, branch-newspaper, etc.)
- **Action:** Add path filter to rust-secrets job:
  ```yaml
  rust-secrets:
    if: contains(github.event.pull_request.changed_files, 'Cargo.toml') || contains(github.event.pull_request.changed_files, '**.rs')
    # or: if: hashFiles('Cargo.toml') != ''
  ```
- **Estimated savings:** ~300 repos × 3s = 900s per estate-wide push

### 1.4 Estate-wide workflow audit
| Workflow | Purpose | Redundant? | Action |
|----------|---------|------------|--------|
| `build-validation.yml` | Quick build check | YES (rust-ci + elixir-ci cover it) | DELETE or merge |
| `rust-ci.yml` | Full Rust CI | NO | Keep, add path filters |
| `elixir-ci.yml` | Full Elixir CI | NO | Keep, add path filters |
| `secret-scanner.yml` | Secret detection | NO (but had trufflehog+gitleaks overlap) | ✅ Fixed |
| `codeql.yml` | Security analysis | NO | Keep, schedule weekly |
| `scorecard.yml` | Supply chain | NO | Keep, schedule weekly |
| `hypatia-scan.yml` | Neurosymbolic | NO | Keep, schedule weekly |

---

## Track 2: Workflow Naming + Clarity (Week 1-2)

### 2.1 Standardize naming convention
**Current chaos:**
- `rust-ci.yml` vs `elixir-ci.yml` vs `build-validation.yml` (inconsistent)
- `secret-scanner.yml` vs `security-scan.yml` (overlap)
- `scorecard.yml` vs `scorecard-enforcer.yml` (unclear difference)

**Proposed standard:**
```
<language>-<purpose>.yml
  OR
<purpose>-<language>.yml

Examples:
- rust-build-test.yml
- elixir-build-test.yml  
- security-secret-scan.yml
- security-codeql.yml
- security-scorecard.yml
- governance-license.yml
- governance-workflow-linter.yml
```

### 2.2 Add descriptive metadata
Every workflow should have:
```yaml
# SPDX-License-Identifier: MPL-2.0
name: Rust — Build + Test + Lint
# Purpose: Validates Rust code compiles, passes tests, clippy lint
# Owner: @hyperpolymath
# Schedule: On push/PR (path-filtered to Rust files)
# Timeout: 30m total
# Est. Cost: 5-8 minutes per run
```

---

## Track 3: Path Filter Optimization (Week 2)

### 3.1 Language-specific filtering
```yaml
# rust-ci.yml
on:
  push:
    paths:
      - '**.rs'
      - 'Cargo.toml'
      - 'Cargo.lock'
      - 'rust-toolchain'
      - '.github/workflows/rust-ci.yml'
  pull_request:
    paths:
      - '**.rs'
      - 'Cargo.toml'
      - 'Cargo.lock'
```

### 3.2 Docs-only PR fast path
```yaml
# For workflows that don't need docs changes:
if: >
  contains(github.event.pull_request.changed_files, '.md') == false &&
  contains(github.event.pull_request.changed_files, '.adoc') == false
```

---

## Track 4: Test + Bench Standards (Week 2-3)

### 4.1 Audit existing test standards
- **Location:** `standards/.github/workflows/` + `standards/templates/`
- **Check:** 
  - Are test workflows applied estate-wide?
  - Are there contradictions between repos?
  - Are panic-attack tests integrated?
  - Are benchmarks proven/safe?

### 4.2 Panic-Attack integration
- **Current:** `panic-attack` workflow exists but may not be in all repos
- **Action:** Add to all repos that have Rust code
- **Patterns to detect:**
  - Unsafe blocks
  - Unwraps/expects
  - Integer overflows
  - Race conditions

### 4.3 Proven Tests Repo (Idris2)
**New repo:** `proven-tests-and-benches`

**Purpose:** Formal guarantees for test correctness using Idris2

**Coverage targets:**
- Echo-type safety tests
- Identity/projection/invariance traversal
- Set concepts
- Interdimensional transfer
- Higher-order constructs

**Properties to prove:**
```idris
-- Tests are valid
TestValid : (t : Test) -> Type
TestValid t = ...

-- Tests are sound (catch what they claim)
TestSound : (t : Test) -> Prop
TestSound t = ...

-- Tests are tamper-proof
TestTamperProof : (t : Test) -> Prop
TestTamperProof t = ...

-- Tests are unpanickable
TestUnpanickable : (t : Test) -> Prop  
TestUnpanickable t = ...
```

---

## Track 5: Acceleration Opportunities (Week 3-4)

### 5.1 Self-hosted runners for heavy work
- **Targets:** fuzzing, E2E, Hypatia scans
- **Estimated savings:** 80-90% for targeted workflows
- **Security:** Rootless Podman containers

### 5.2 Caching optimization
- **Current:** Some repos use `Swatinem/rust-cache`, others don't
- **Action:** Standardize caching across all language workflows
- **Targets:**
  - Rust: `cargo` cache + `target/` directory
  - Elixir: `deps/` + `_build/`
  - Node: `node_modules/`
  - Go: `go.mod` hash-based

### 5.3 Matrix strategy optimization
- **Problem:** Many workflows test every combination (Rust nightly/stable/beta × OS)
- **Action:** Reduce matrix for PRs, full matrix for nightly/main

---

## Track 6: Estate-Wide Workflow Inventory (Week 1)

### 6.1 Generate current inventory
```bash
# List all unique workflow files
find . -path "*/.github/workflows/*.yml" -type f | sort | uniq

# Count per workflow name
find . -path "*/.github/workflows/*.yml" -type f | xargs -I {} basename {} | sort | uniq -c | sort -rn

# Identify duplicates (same name, different content)
find . -path "*/.github/workflows/*.yml" -type f -exec sha256sum {} \; | awk '{print $1, $2}' | sort | uniq -d -w 64
```

### 6.2 Categorize workflows
| Category | Workflow | Count | Action |
|----------|----------|-------|--------|
| Security | secret-scanner.yml | 1,191 | ✅ Optimized |
| Security | codeql.yml | ~500 | Schedule weekly |
| Security | scorecard.yml | ~500 | Schedule weekly |
| Security | hypatia-scan.yml | ~500 | Schedule weekly |
| Build | rust-ci.yml | ~200 | Path filter |
| Build | elixir-ci.yml | ~50 | Path filter |
| Build | build-validation.yml | ~50 | DELETE |
| Governance | governance.yml | ~1000 | N/A |
| Lint | workflow-linter.yml | ~500 | N/A |
| Mirror | mirror.yml | ~300 | N/A |

---

## Immediate Action Items (Next 48 Hours)

1. ✅ **DONE:** Remove trufflehog from secret-scanner-reusable.yml
2. **TODO:** Merge `build-validation.yml` into language-specific CI workflows in verisimdb
3. **TODO:** Add path filters to rust-secrets job in secret-scanner-reusable.yml
4. **TODO:** Create `proven-tests-and-benches` repo skeleton with Idris2 proofs
5. **TODO:** Open per-repo issues for workflow consolidation (verisimdb, then propagate)

---

## Success Metrics

| Metric | Current | Target (4 weeks) | Target (12 weeks) |
|--------|---------|------------------|-------------------|
| CI minutes/month | ~50,000 | <25,000 (-50%) | <15,000 (-70%) |
| Workflow count | ~1,200 | <800 | <600 |
| PR merge time | ~30min | <15min | <10min |
| Test coverage | ~80% | >90% | >95% |
| Proven tests | 0 | >10 modules | >50 modules |

---

## Tags

`cicd-optimization`, `cost-reduction`, `performance`, `security`, `ultra-zotta-plan`, `estate-wide`, `high-priority`

---

## Related Issues

- [ ] Track 1: Immediate Redundancy Elimination
- [ ] Track 2: Workflow Naming + Clarity  
- [ ] Track 3: Path Filter Optimization
- [ ] Track 4: Test + Bench Standards
- [ ] Track 5: Acceleration Opportunities
- [ ] Track 6: Estate-Wide Workflow Inventory
