// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

# Issue #005: Acceleration Opportunities

**Track:** CICD Optimization — Ultra-Zotta-Plan  
**Priority:** MEDIUM  
**Status:** TODO  
**Owner:** @hyperpolymath  
**Date:** 2026-06-05  

---

## Description

Explore opportunities to **dramatically reduce CI costs** through caching, self-hosted runners, and matrix optimization. These are longer-term investments with high ROI.

---

## Opportunity 1: Self-Hosted Runners (80-90% savings for heavy work)

### 1.1 Target Workflows

| Workflow | Avg Duration | Monthly Cost Estimate | Savings Potential |
|----------|---------------|----------------------|-------------------|
| `oracle-fuzz.yml` | 10-60m | ~$5,000 | 90% |
| `hypatia-scan.yml` | 5-15m | ~$3,000 | 80% |
| `codeql.yml` | 5-10m | ~$2,000 | 80% |
| E2E tests | 15-45m | ~$4,000 | 90% |
| Fuzz targets | 10-30m | ~$3,000 | 90% |
| **Total** | | **~$17,000** | **~$13,600 (80%)** |

### 1.2 Implementation Plan

#### Phase 1: Pilot (Week 3)
- **Target:** 1-2 highest-cost repos (verisimdb, 007)
- **Runner:** Self-hosted on Eclipse infrastructure
- **Security:** Rootless Podman containers
- **Setup:**
  ```bash
  # Install Podman
  sudo apt install podman
  
  # Create runner container
  podman run -d --name github-runner \
    -e REPO_URL=https://github.com/hyperpolymath/verisimdb \
    -e RUNNER_TOKEN=<token> \
    -e RUNNER_NAME=verisimdb-runner \
    -e RUNNER_GROUP=verisimdb \
    --restart always \
    ghcr.io/actions/actions-runner:latest
  ```

#### Phase 2: Scale (Week 4-5)
- **Target:** All repos with fuzzing/E2E/Hypatia workflows
- **Runner pool:** 3-5 runners (shared across repos)
- **Labels:** Use runner labels to route heavy jobs
  ```yaml
  jobs:
    fuzz:
      runs-on: [self-hosted, fuzz-runner]
  ```

#### Phase 3: Estate-wide (Week 6+)
- **Target:** All repos
- **Runner pool:** 10+ runners
- **Auto-scaling:** Use Kubernetes for dynamic scaling

### 1.3 Security Requirements

- [ ] Rootless containers (no root access)
- [ ] Read-only root filesystem
- [ ] No new privileges
- [ ] Network isolation
- [ ] Regular runner image updates
- [ ] Audit logging

---

## Opportunity 2: Caching Optimization (30-50% savings)

### 2.1 Current State

**Inconsistent caching:**
- Some repos use `Swatinem/rust-cache`
- Others use manual cache actions
- Some have no caching
- Cache keys vary (some good, some not)

### 2.2 Standard Caching Strategy

#### Rust
```yaml
- uses: Swatinem/rust-cache@v2
  with:
    key: rust-cache-${{ hashFiles('**/Cargo.lock') }}
    cache-directories: |
      ~/.cargo/registry/
      ~/.cargo/git/
      target/
```

#### Elixir
```yaml
- uses: actions/cache@v5
  with:
    path: |
      elixir-orchestration/deps
      elixir-orchestration/_build
    key: elixir-cache-${{ hashFiles('elixir-orchestration/mix.lock') }}
```

#### Node.js
```yaml
- uses: actions/cache@v5
  with:
    path: node_modules/
    key: node-cache-${{ hashFiles('package-lock.json') }}
```

#### Go
```yaml
- uses: actions/cache@v5
  with:
    path: ~/go/pkg/mod/
    key: go-cache-${{ hashFiles('go.sum') }}
```

### 2.3 Cache Key Optimization

**Problem:** Cache keys that are too broad cause cache misses.

**Solution:** Use precise, hierarchical cache keys:
```yaml
key: ${{ runner.os }}-${{ hashFiles('Cargo.lock') }}-${{ hashFiles('rust-toolchain') }}
restore-keys: |
  ${{ runner.os }}-${{ hashFiles('Cargo.lock') }}-
  ${{ runner.os }}-
```

### 2.4 Tasks

#### Task 2.4.1: Create caching standards
- **Output:** `standards/templates/github/workflows/caching.yml`
- **Status:** TODO
- **Priority:** HIGH

#### Task 2.4.2: Apply caching to top 10 repos
- **Target:** verisimdb, 007, hypatia, gossamer, etc.
- **Status:** TODO
- **Priority:** HIGH

#### Task 2.4.3: Estate-wide caching propagation
- **Approach:** Use reusable workflow with caching
- **Status:** TODO
- **Priority:** MEDIUM

---

## Opportunity 3: Matrix Strategy Optimization (20-40% savings)

### 3.1 Current State

**Typical matrix (wasteful):**
```yaml
strategy:
  matrix:
    rust:
      - stable
      - beta
      - nightly
    os:
      - ubuntu-latest
      - macos-latest
      - windows-latest
    # 3 × 3 = 9 combinations, most redundant for PRs
```

### 3.2 Optimized Matrix

**For PRs:** Only test primary combination
**For main/schedule:** Test full matrix

```yaml
jobs:
  test:
    strategy:
      matrix:
        include:
          # PR trigger: only stable on ubuntu
          - rust: stable
            os: ubuntu-latest
            if: github.event_name == 'pull_request'
          # Main/schedule: full matrix
          - rust: stable
            os: ubuntu-latest
            if: github.event_name != 'pull_request'
          - rust: beta
            os: ubuntu-latest
            if: github.event_name != 'pull_request'
          - rust: nightly
            os: ubuntu-latest
            if: github.event_name != 'pull_request'
          - rust: stable
            os: macos-latest
            if: github.event_name != 'pull_request'
          - rust: stable
            os: windows-latest
            if: github.event_name != 'pull_request'
```

### 3.3 Even Better: Separate Workflows

```yaml
# rust-ci-pr.yml (runs on PRs)
name: Rust CI (PR)
on:
  pull_request:
    paths: [**.rs, Cargo.toml]
jobs:
  test:
    strategy:
      matrix:
        rust: [stable]
        os: [ubuntu-latest]

# rust-ci-full.yml (runs on schedule/main)
name: Rust CI (Full)
on:
  push:
    branches: [main]
  schedule:
    - cron: '0 3 * * 0'
jobs:
  test:
    strategy:
      matrix:
        rust: [stable, beta, nightly]
        os: [ubuntu-latest, macos-latest, windows-latest]
```

### 3.4 Tasks

#### Task 3.4.1: Optimize matrix for top 5 repos
- **Targets:** verisimdb, 007, hypatia, gossamer, aspasia
- **Status:** TODO
- **Priority:** HIGH

#### Task 3.4.2: Create matrix templates
- **Output:** `standards/templates/github/workflows/matrix-strategies.yml`
- **Status:** TODO
- **Priority:** MEDIUM

---

## Opportunity 4: Alternative Tools (10-30% better)

### 4.1 Secret Scanning

| Tool | Strengths | Weaknesses | Recommendation |
|------|-----------|------------|----------------|
| **Gitleaks** | Fast, regex-based, good CVE coverage | Can miss entropy-based secrets | ✅ KEEP (primary) |
| **TruffleHog** | Entropy + verified detection | Slower, ~90% overlap with gitleaks | ❌ REMOVED |
| **GitGuardian** | Commercial, good UI | Paid | ⚠️ EVALUATE |
| **Semgrep** | Multi-purpose, fast | Needs custom rules for secrets | ⚠️ EVALUATE |

**Decision:** Gitleaks is sufficient. TruffleHog removed. Evaluate Semgrep for multi-purpose scanning.

### 4.2 Code Analysis

| Tool | Strengths | Weaknesses | Recommendation |
|------|-----------|------------|----------------|
| **CodeQL** | Deep semantic analysis | Slow, heavy | ✅ KEEP (scheduled) |
| **Clippy** | Rust-specific lint | Rust only | ✅ KEEP |
| **SonarQube** | Multi-language | Heavy, commercial | ❌ REPLACE with CodeQL |
| **PVS-Studio** | Commercial, deep | Paid | ❌ DROP |

### 4.3 Fuzzing

| Tool | Strengths | Weaknesses | Recommendation |
|------|-----------|------------|----------------|
| **cargo-fuzz** | Rust-native | Good |
| **AFL** | Generic | Older | ⚠️ EVALUATE |
| **libFuzzer** | LLVM-based | Integration complexity | ⚠️ EVALUATE |
| **Honggfuzz** | Multi-language | Less maintained | ❌ DROP |

---

## Tasks

### Task 5.1: Pilot self-hosted runners
- **Repos:** verisimdb, 007
- **Workflows:** fuzz, hypatia-scan, codeql
- **Status:** TODO
- **Priority:** HIGH

### Task 5.2: Apply standard caching
- **Repos:** Top 10 by CI minute consumption
- **Status:** TODO
- **Priority:** HIGH

### Task 5.3: Optimize matrix strategies
- **Repos:** All repos with matrix builds
- **Status:** TODO
- **Priority:** MEDIUM

### Task 5.4: Evaluate alternative tools
- **Tools:** Semgrep, GitGuardian
- **Status:** TODO
- **Priority:** LOW

---

## Success Criteria

- [ ] Self-hosted runners handling 50% of heavy workflows
- [ ] Caching applied to 80% of repos
- [ ] Matrix optimization applied to 50% of repos
- [ ] CI cost reduced by >50% from Week 0 baseline

---

## Tags

`cicd-optimization`, `acceleration`, `self-hosted`, `caching`, `matrix`, `cost-reduction`, `week-3`, `medium-priority`, `track-5`

---

## Related

- Roadmap: `2026-06-05-cicd-optimization-roadmap.md`
- Issue #001: Immediate Redundancy Elimination
- Issue #002: Workflow Naming + Clarity
- Issue #003: Path Filter Optimization
- Issue #004: Test + Bench Standards
