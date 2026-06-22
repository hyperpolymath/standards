// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

# Issue #006: verisimdb#114 — HexAudit + Benchee Duplicate Jobs

**Track:** CICD Optimization — Ultra-Zotta-Plan  
**Priority:** HIGH  
**Status:** IN PROGRESS  
**Owner:** @hyperpolymath  
**Date:** 2026-06-05  
**Related PR:** hyperpolymath/verisimdb#114

---

## Problem Statement

PR #114 in `databases/verisimdb` shows **duplicate job names** causing CI confusion:

| Job Name | Source Workflow | Purpose | Duration |
|----------|----------------|---------|----------|
| `bench-compile` | `rust-ci.yml` | Rust benchmarks compile | 3m50s |
| `benchee scripts compile` | `elixir-ci.yml` | Elixir benchee compile | (not shown in checks) |
| `audit` | `rust-ci.yml` | Cargo audit | 3m10s |
| `hex audit` | `elixir-ci.yml` | Hex package audit | (not shown in checks) |
| `Elixir build validation` | `build-validation.yml` | Elixir compile check | 1m33s |
| `Rust build validation` | `build-validation.yml` | Rust compile check | (merged into Rust build validation?) |

**Issue:** The checks output shows `benchmarks compile` and `cargo audit` passing, but the `benchee scripts compile` and `hex audit` jobs from `elixir-ci.yml` may be:
1. Not running (path filters prevent them)
2. Running but not shown in GH PR checks UI
3. Queued behind other jobs

---

## Root Cause Analysis

### Current Workflow Structure in verisimdb

```
databases/verisimdb/.github/workflows/
├── build-validation.yml    # Quick build checks (Rust + Elixir)
├── rust-ci.yml            # Full Rust CI (test, clippy, doc, audit, bench-compile)
├── elixir-ci.yml          # Full Elixir CI (test, coverage, bench-compile, hex audit)
└── ... (other workflows)
```

### Overlap Analysis

| Job | build-validation.yml | rust-ci.yml | elixir-ci.yml |
|-----|----------------------|-------------|---------------|
| Rust compile | ✅ `validate-rust` | ✅ `test` (implicit) | ❌ |
| Elixir compile | ✅ `validate-elixir` | ❌ | ✅ `build-test` |
| Rust benchmarks | ❌ | ✅ `bench-compile` | ❌ |
| Elixir benchee | ❌ | ❌ | ✅ `bench-compile` |
| Cargo audit | ❌ | ✅ `audit` | ❌ |
| Hex audit | ❌ | ❌ | ✅ `audit` |

**Duplicate coverage:** Both `build-validation.yml` and language-specific CI workflows check compilation.

---

## Solution Options

### Option A: Delete build-validation.yml (RECOMMENDED)

**Rationale:** `rust-ci.yml` and `elixir-ci.yml` already cover build validation. The `build-validation.yml` is redundant.

**Changes:**
```bash
cd databases/verisimdb
rm .github/workflows/build-validation.yml
```

**Impact:**
- ✅ Eliminates duplicate `Rust build validation` + `Elixir build validation` jobs
- ✅ Saves ~2-3 minutes per PR
- ⚠️ Need to verify rust-ci + elixir-ci have equivalent coverage

**Verification:**
- [ ] `rust-ci.yml` has `cargo check` or equivalent
- [ ] `elixir-ci.yml` has `mix compile` or equivalent
- [ ] Both run on all relevant branches

### Option B: Merge build-validation into language workflows

**Rationale:** Keep quick validation but merge into existing workflows.

**Changes:**
- Add `build-validation` job to `rust-ci.yml` with path filters
- Add `build-validation` job to `elixir-ci.yml` with path filters
- Delete `build-validation.yml`

**Impact:** Same savings, but keeps the validation concept.

### Option C: Keep all, but add concurrency groups

**Rationale:** If there's a reason to keep all three workflows separate.

**Changes:**
- Add `concurrency:` groups to all workflows to prevent queuing
- Already present in rust-ci.yml and elixir-ci.yml
- Add to build-validation.yml:
  ```yaml
  concurrency:
    group: build-validation-${{ github.ref }}
    cancel-in-progress: true
  ```

**Impact:** Minimal savings (still runs all jobs).

---

## Recommended Action: Option A

**Step 1:** Verify rust-ci.yml and elixir-ci.yml have build validation
```bash
# Check rust-ci.yml has cargo check
grep -n "cargo check\|cargo test\|cargo build" databases/verisimdb/.github/workflows/rust-ci.yml

# Check elixir-ci.yml has mix compile
grep -n "mix compile\|mix test" databases/verisimdb/.github/workflows/elixir-ci.yml
```

**Step 2:** Delete build-validation.yml
```bash
rm databases/verisimdb/.github/workflows/build-validation.yml
```

**Step 3:** Commit and monitor PR #114
```bash
cd databases/verisimdb
git add .github/workflows/build-validation.yml
git commit -m "ci: remove redundant build-validation.yml (covered by rust-ci + elixir-ci)"
git push
```

**Expected result:** PR #114 checks will show fewer jobs, complete faster.

---

## Benchee + HexAudit Specifics

### Current State in elixir-ci.yml

```yaml
bench-compile:
  name: benchee scripts compile
  runs-on: ubuntu-latest
  timeout-minutes: 10
  defaults:
    run:
      working-directory: elixir-orchestration
  steps:
    - uses: actions/checkout@v6
    - uses: erlef/setup-beam@v1
    - run: mix deps.get
    - name: Syntax check all bench scripts
      run: |
        for f in bench/*.exs; do
          elixir -e "Code.string_to_quoted!(File.read!(\"$f\"))"
          echo "  ✓ $f parses"
        done

audit:
  name: hex audit
  runs-on: ubuntu-latest
  timeout-minutes: 10
  defaults:
    run:
      working-directory: elixir-orchestration
  steps:
    - uses: actions/checkout@v6
    - uses: erlef/setup-beam@v1
    - run: mix deps.get
    - run: mix hex.audit
    - run: mix deps.unlock --check-unused
```

### Issue: Path Filters Missing

The `elixir-ci.yml` has path filters:
```yaml
on:
  push:
    paths:
      - "elixir-orchestration/**"
      - ".github/workflows/elixir-ci.yml"
  pull_request:
    paths:
      - "elixir-orchestration/**"
      - ".github/workflows/elixir-ci.yml"
```

**Problem:** If PR #114 doesn't modify files under `elixir-orchestration/`, the entire workflow (including benchee + hex audit) is **skipped**.

**Check PR #114 files:**
```bash
# Get PR #114 changed files
gh pr view 114 --repo hyperpolymath/verisimdb --json files | jq -r '.files[].path'
```

If the PR only touches machine_readable/ files and not elixir-orchestration/, then:
- ✅ elixir-ci.yml is correctly skipped (path filters working)
- ✅ benchee + hex audit are NOT hanging — they're being skipped
- ❌ The "hanging" perception is actually correct behavior

**Resolution:** The jobs aren't hanging — they're being skipped due to path filters. This is GOOD, not BAD.

---

## Verification for PR #114

**Check what files changed:**
```bash
cd databases/verisimdb
gh pr diff 114 --name-only
```

**If no elixir-orchestration/ files changed:**
- elixir-ci.yml is correctly skipped
- benchee + hex audit jobs don't run
- This is **expected behavior**, not a bug

**If elixir-orchestration/ files DID change:**
- Check if elixir-ci.yml jobs appear in PR checks
- If not, there's a different issue (permissions, syntax error, etc.)

---

## Final Recommendation

1. **For PR #114 specifically:**
   - Verify if it modifies `elixir-orchestration/` files
   - If NOT: elixir-ci.yml is correctly skipped (path filters working)
   - If YES: Check workflow syntax and permissions

2. **For estate-wide:**
   - ✅ Delete `build-validation.yml` (redundant)
   - ✅ Keep path filters in elixir-ci.yml (they're working correctly)
   - ✅ Add similar path filters to rust-ci.yml

3. **For verisimdb:**
   - Delete `build-validation.yml`
   - Add path filters to rust-ci.yml
   - Result: Faster PRs, no duplicate jobs

---

## Tasks

### Task 6.1: Verify PR #114 file changes
- **Action:** Check if PR modifies elixir-orchestration/ or only machine_readable/
- **Status:** TODO
- **Priority:** HIGH

### Task 6.2: Delete build-validation.yml from verisimdb
- **Action:** `rm .github/workflows/build-validation.yml`
- **Status:** TODO
- **Priority:** HIGH

### Task 6.3: Add path filters to rust-ci.yml in verisimdb
- **Action:** Add `paths:` filter matching elixir-ci.yml pattern
- **Status:** TODO
- **Priority:** MEDIUM

### Task 6.4: Propagate fix estate-wide
- **Action:** Find all repos with both `build-validation.yml` and language-specific CI
- **Status:** TODO
- **Priority:** MEDIUM

---

## Success Criteria

- [ ] PR #114 shows clear, non-duplicate job names
- [ ] PR #114 completes in <10 minutes (currently ~15+ minutes)
- [ ] verisimdb has no build-validation.yml
- [ ] rust-ci.yml in verisimdb has path filters

---

## Tags

`cicd-optimization`, `verisimdb`, `redundancy`, `duplicate-jobs`, `path-filters`, `high-priority`, `track-1`, `pr-114`

---

## Related

- Roadmap: `2026-06-05-cicd-optimization-roadmap.md`
- Issue #001: Immediate Redundancy Elimination
- PR: hyperpolymath/verisimdb#114
