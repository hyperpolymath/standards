// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

# Issue #003: Path Filter Optimization

**Track:** CICD Optimization — Ultra-Zotta-Plan  
**Priority:** HIGH  
**Status:** TODO  
**Owner:** @hyperpolymath  
**Date:** 2026-06-05  

---

## Description

Currently, many workflows run on **every push/PR** regardless of what files changed. This causes unnecessary CI minute consumption when only docs or config files are modified.

**Estimated waste:** 40-60% of CI minutes on repos with active documentation.

---

## Current State

### Workflows WITHOUT path filters (run on every change):
```yaml
# These run even when only README.md is edited:
- rust-ci.yml          # Should only run on Rust file changes
- elixir-ci.yml        # Should only run on Elixir file changes
- codeql.yml           # Could be scheduled only
- scorecard.yml        # Could be scheduled only
- hypatia-scan.yml     # Could be scheduled only
- build-validation.yml # Should only run on code changes
```

### Workflows WITH path filters (good):
```yaml
# elixir-ci.yml in verisimdb (partial):
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

---

## Proposed Path Filters

### 1. Language-Specific Workflows

#### Rust (`rust-ci.yml`)
```yaml
on:
  push:
    branches: [main, master]
    paths:
      - '**.rs'
      - 'Cargo.toml'
      - 'Cargo.lock'
      - 'rust-toolchain'
      - 'rust-toolchain.toml'
      - '.github/workflows/rust-ci.yml'
  pull_request:
    branches: [main, master]
    paths:
      - '**.rs'
      - 'Cargo.toml'
      - 'Cargo.lock'
      - 'rust-toolchain'
      - 'rust-toolchain.toml'
      - '.github/workflows/rust-ci.yml'
```

#### Elixir (`elixir-ci.yml`)
```yaml
on:
  push:
    branches: [main, master]
    paths:
      - '**/*.ex'
      - '**/*.exs'
      - 'mix.exs'
      - 'mix.lock'
      - '.github/workflows/elixir-ci.yml'
  pull_request:
    branches: [main, master]
    paths:
      - '**/*.ex'
      - '**/*.exs'
      - 'mix.exs'
      - 'mix.lock'
      - '.github/workflows/elixir-ci.yml'
```

#### Go
```yaml
on:
  push:
    branches: [main, master]
    paths:
      - '**.go'
      - 'go.mod'
      - 'go.sum'
      - '.github/workflows/go-ci.yml'
  pull_request:
    branches: [main, master]
    paths:
      - '**.go'
      - 'go.mod'
      - 'go.sum'
      - '.github/workflows/go-ci.yml'
```

### 2. Security Workflows

#### Secret Scanner (`secret-scanner.yml`)
**Should run on ALL changes** (secrets can be added anywhere)
- Keep current: `on: [pull_request, push]`
- NO path filters

#### CodeQL (`codeql.yml`)
**Can be scheduled** (deep analysis, not needed on every commit)
```yaml
on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]
  schedule:
    - cron: '0 4 * * 1'  # Weekly Monday at 4am
```

#### Scorecard (`scorecard.yml`)
**Can be scheduled** (repo metadata doesn't change often)
```yaml
on:
  push:
    branches: [main, master]
  schedule:
    - cron: '0 3 * * 0'  # Weekly Sunday at 3am
```

#### Hypatia Scan (`hypatia-scan.yml`)
**Can be scheduled** (neurosymbolic analysis is slow)
```yaml
on:
  push:
    branches: [main, master]
  schedule:
    - cron: '0 2 * * 1'  # Weekly Monday at 2am
```

### 3. Governance Workflows

#### SPDX Check
**Should run on code changes only** (not on docs)
```yaml
on:
  push:
    branches: [main, master]
    paths-ignore:
      - '**.md'
      - '**.adoc'
      - '**.txt'
      - 'LICENSE'
      - '.gitignore'
  pull_request:
    branches: [main, master]
    paths-ignore:
      - '**.md'
      - '**.adoc'
      - '**.txt'
      - 'LICENSE'
      - '.gitignore'
```

#### Workflow Linter
**Should run on workflow changes only**
```yaml
on:
  push:
    branches: [main, master]
    paths:
      - '.github/workflows/**'
      - '.github/**'
  pull_request:
    branches: [main, master]
    paths:
      - '.github/workflows/**'
      - '.github/**'
```

---

## Tasks

### Task 3.1: Add path filters to language workflows
- **Repos:** Start with high-impact repos (verisimdb, 007, hypatia, etc.)
- **Action:** Add language-specific path filters to rust-ci.yml, elixir-ci.yml, etc.
- **Status:** TODO
- **Priority:** HIGH

### Task 3.2: Schedule heavy security workflows
- **Action:** Change codeql.yml, scorecard.yml, hypatia-scan.yml to weekly schedule
- **Status:** TODO
- **Priority:** HIGH

### Task 3.3: Add path-ignore for docs-only changes
- **Action:** Add `paths-ignore` for markdown/adoc files to governance workflows
- **Status:** TODO
- **Priority:** MEDIUM

### Task 3.4: Create path-filter templates
- **Action:** Add reusable path-filter configs in standards repo
- **Location:** `standards/templates/github/workflows/path-filters/`
- **Status:** TODO
- **Priority:** MEDIUM

---

## Success Criteria

- [ ] All language workflows have path filters
- [ ] Heavy workflows (codeql, scorecard, hypatia) run on schedule
- [ ] Docs-only PRs complete in <5 minutes
- [ ] CI minutes reduced by >30% from Week 0 baseline

---

## Tags

`cicd-optimization`, `path-filters`, `performance`, `cost-reduction`, `week-2`, `high-priority`, `track-3`

---

## Related

- Roadmap: `2026-06-05-cicd-optimization-roadmap.md`
- Issue #001: Immediate Redundancy Elimination
- Issue #002: Workflow Naming + Clarity
