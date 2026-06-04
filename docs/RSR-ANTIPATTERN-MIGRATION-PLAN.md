# RSR Anti-Pattern Migration Plan - Issue #150

## Problem Statement

There are **347 duplicate copies** of `rsr-antipattern.yml` workflow files scattered across the repository estate. Each repository maintains its own independent copy of the same anti-pattern checking logic, leading to:

- **Massive duplication**: 347 identical workflow files
- **Maintenance burden**: Updates must be applied to all 347 files
- **Drift risk**: Files can diverge over time
- **Inconsistency**: Different versions may have different checks

## Current State Analysis

### Distribution by Repository Group

| Repository Group | Count | Percentage |
|-----------------|-------|------------|
| repos-monorepo | 195 | 56.2% |
| hyperpolymath-archive | 51 | 14.7% |
| developer-ecosystem | 41 | 11.8% |
| ambientops | 25 | 7.2% |
| julia | 4 | 1.2% |
| proof-burrower, languages, isers, idaptik | 2 each | 2.3% |
| Single instances (22 repos) | 22 | 6.3% |
| **Total** | **347** | **100%** |

### Breakdown by Top-Level Directory

```
repos-monorepo: 195 instances
├── verification-ecosystem: 36 instances
│   ├── k9-ecosystem: 10 instances
│   └── a2ml-ecosystem: 12 instances
├── developer-ecosystem: 41 instances
├── social-media-ecosystem: 6 instances
├── document-management-toolset: 18 instances
├── fleet-ecosystem: 6 instances
├── boj-cartridges: 29 instances
├── poly-observability-mcp: 1 instance
├── invariant-path: 1 instance
├── repos-monorepo (self): 1 instance
└── ... (74 more in various subdirectories)

hyperpolymath-archive: 51 instances
├── asdf-plugin-collection: 7 instances
├── wordpress-tools: 7 instances
├── zotero-tools: 7 instances
└── ... (30 more in various subdirectories)

dveloper-ecosystem: 41 instances
ambientops: 25 instances
```

## Solution

Create a **reusable workflow** and migrate all 347 repositories to use it.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    standards repo                              │
│  .github/workflows/rsr-antipattern-reusable.yml  ← REUSABLE │
└─────────────────────────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                 All 347 downstream repos                       │
│                                                             │
│  .github/workflows/rsr-antipattern.yml                    │
│  ┌────────────────────────────────────────────────────────┐│
│  │jobs:                                                  ││
│  │  antipattern-check:                                   ││
│  │    uses: hyperpolymath/standards/.github/workflows/  ││
│  │          rsr-antipattern-reusable.yml@main            ││
│  └────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
```

### Reusable Workflow Created

**Location**: `/standards/.github/workflows/rsr-antipattern-reusable.yml`

**Features**:
- TypeScript checking with built-in allowlist and per-repo exemptions
- Go file detection
- Python file detection (non-SaltStack)
- npm/yarn lockfile detection
- tsconfig.json detection
- Deno configuration verification
- Summary output

**Usage**:
```yaml
jobs:
  antipattern-check:
    uses: hyperpolymath/standards/.github/workflows/rsr-antipattern-reusable.yml@main
```

## Migration Strategy

Given the massive scale (347 repos), migration will be executed in **phases** with **batches** within each phase.

### Phase 0: Infrastructure (COMPLETED ✅)

- [x] Investigate issue #150
- [x] Identify all 347 instances
- [x] Create reusable workflow in standards repo
- [x] Document migration plan

### Phase 1: Proof of Concept (Batch Size: 10 repos)

**Goal**: Validate the reusable workflow works correctly

**Repos to migrate**:
1. `a2ml`
2. `rsr-template-repo`
3. `rescript`
4. `rescript-ecosystem`
5. `servers`
6. `the-nash-equilibrium`
7. `v3-templater`
8. `verisimdb-data`
9. `zotero-tools/rescript-templater`
10. `proof-burrower`

**Steps for each repo**:
1. Delete existing `.github/workflows/rsr-antipattern.yml`
2. Create new `.github/workflows/rsr-antipattern.yml` with reusable workflow call
3. Test the workflow runs successfully
4. Commit and push

**Success criteria**: All 10 repos pass the new workflow

### Phase 2: Small Groups (Batch Size: ~50 repos)

**Group A: hyperpolymath-archive (51 repos)**
- Migrate all 51 repos in hyperpolymath-archive
- Use parallel processing where possible
- Expected time: 2-3 hours

**Group B: ambientops (25 repos)**
- Migrate all 25 repos in ambientops
- Expected time: 1-2 hours

### Phase 3: Medium Groups (Batch Size: ~100 repos)

**Group C: developer-ecosystem (41 repos)**
- Migrate all 41 repos
- Expected time: 2-3 hours

**Group D: Remaining single instances (~22 repos)**
- languages, isers, idaptik, proof-burrower, etc.
- Expected time: 1-2 hours

### Phase 4: Large Group (Batch Size: ~200 repos)

**Group E: repos-monorepo (195 repos)**
- This is the largest batch
- Break into sub-batches of 20-30 repos each
- Expected time: 6-8 hours total
- Sub-batches:
  - E1: verification-ecosystem (36 repos)
  - E2: developer-ecosystem (41 repos) - already in Phase 3
  - E3: social-media-ecosystem (6 repos)
  - E4: document-management-toolset (18 repos)
  - E5: fleet-ecosystem (6 repos)
  - E6: boj-cartridges (29 repos)
  - E7: Remaining repos-monorepo (59 repos)

## Migration Script

See `scripts/migrate-rsr-antipattern.sh` for automated migration.

### Script Usage

```bash
# Migrate a single repo
./scripts/migrate-rsr-antipattern.sh /path/to/repo

# Migrate multiple repos
find /path/to/repos -name "rsr-antipattern.yml" | xargs -I {} dirname {} | xargs -I {} dirname {} | xargs -I {} ./scripts/migrate-rsr-antipattern.sh {}

# Dry run (show what would be changed)
./scripts/migrate-rsr-antipattern.sh --dry-run /path/to/repo
```

## File Changes Required per Repository

### BEFORE (current state)

Each repo has its own copy:
```
.repo/
└── .github/
    └── workflows/
        └── rsr-antipattern.yml  (194 lines, duplicated everywhere)
```

### AFTER (desired state)

Each repo uses the reusable workflow:
```
.repo/
└── .github/
    └── workflows/
        └── rsr-antipattern.yml  (5 lines, calls reusable)
```

**New file content** (rsr-antipattern.yml):
```yaml
# SPDX-License-Identifier: MPL-2.0
# RSR Anti-Pattern Check - Uses reusable workflow from standards

name: RSR Anti-Pattern Check

on:
  push:
    branches: [main, master, develop]
  pull_request:
    branches: [main, master, develop]

jobs:
  antipattern-check:
    uses: hyperpolymath/standards/.github/workflows/rsr-antipattern-reusable.yml@main
```

## Risk Assessment

### Low Risk
- The reusable workflow is identical in functionality to the standalone version
- All exemption logic (TypeScript allowlist, per-repo CLAUDE.md exemptions) is preserved
- The workflow uses the same checks (TypeScript, Go, Python, npm, tsconfig)

### Medium Risk
- Some repos may have customized their rsr-antipattern.yml
- Need to verify no repos have divergent logic

### Mitigation
1. **Backup**: Create backup of all rsr-antipattern.yml files before migration
2. **Verification**: Run workflow in each migrated repo to ensure it passes
3. **Rollback**: Script to revert if issues arise
4. **Validation**: Check for customizations before overwriting

## Timeline Estimate

| Phase | Repos | Time Estimate | Start Date | Target Complete |
|-------|-------|---------------|------------|-----------------|
| 0 | Infrastructure | 2 hours | Already done | Done |
| 1 | Proof of Concept | 10 repos | 1 hour | TBD | TBD |
| 2A | hyperpolymath-archive | 51 repos | 2-3 hours | After Phase 1 | TBD |
| 2B | ambientops | 25 repos | 1-2 hours | After Phase 2A | TBD |
| 3A | developer-ecosystem | 41 repos | 2-3 hours | After Phase 2 | TBD |
| 3B | Remaining singles | 22 repos | 1-2 hours | After Phase 3A | TBD |
| 4A | repos-monorepo E1 | 36 repos | 2 hours | After Phase 3 | TBD |
| 4B | repos-monorepo E3-E7 | 159 repos | 5-6 hours | After Phase 4A | TBD |
| **Total** | **347 repos** | **14-21 hours** | - | - |

**Note**: This is manual time. With scripting and automation, actual time may be 30-50% less.

## Verification Plan

After each phase:
1. Verify workflow runs successfully in all migrated repos
2. Check that no TypeScript/Go/Python/npm files slip through
3. Validate that per-repo exemptions still work (CLAUDE.md parsing)
4. Confirm workflow outputs are as expected

## Rollback Plan

If migration causes issues:
1. Revert individual repos by restoring from backup
2. For systemic issues, revert all repos in the current batch
3. Fix the reusable workflow
4. Re-attempt migration

## Success Criteria

Issue #150 is considered **RESOLVED** when:

1. ✅ All 347 standalone rsr-antipattern.yml files are removed
2. ✅ All 347 repos have new rsr-antipattern.yml calling the reusable workflow
3. ✅ The reusable workflow in standards repo is working correctly
4. ✅ All repos pass the new workflow
5. ✅ No regressions in anti-pattern detection
6. ✅ Documentation updated

## Next Steps

1. **Immediate**: Execute Phase 1 (Proof of Concept with 10 repos)
2. **Short-term**: Execute Phase 2 (hyperpolymath-archive and ambientops)
3. **Medium-term**: Execute Phase 3 and 4 (remaining repos)
4. **Long-term**: Monitor for drift and maintain reusable workflow

## Files Modified

- **Created**: `/standards/.github/workflows/rsr-antipattern-reusable.yml`
- **Created**: `/standards/docs/RSR-ANTIPATTERN-MIGRATION-PLAN.md` (this file)
- **To Create**: `/standards/scripts/migrate-rsr-antipattern.sh`
- **To Modify**: 347 x `.github/workflows/rsr-antipattern.yml` files across estate

## References

- Issue #150: Anti-pattern / standards (proven repo)
- Commit 8ea1412 in proven: "Tracking: #150 (anti-pattern / standards)"
- REORGANIZATION-PLAN.md in standards: Identifies rsr-antipattern.yml duplication
- rsr-self-compliance.a2ml: RSR R-053 requires rsr-antipattern.yml
