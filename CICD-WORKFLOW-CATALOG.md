# CI/CD Workflow Catalog

**Purpose:** Central registry of all available CI/CD workflows for the hyperpolymath and metadatastician estates.

**Maintained by:** Estate-wide standards
**Source of Truth:** This file + `rsr-template-repo/.github/workflows/` + `standards/.github/workflows/`

---

## How to Use This Catalog

### For New Repos
When creating a new repo, select workflows from this catalog based on your needs:
- **All repos** should have: governance.yml, codeql.yml, scorecard.yml, hypatia-scan.yml
- **Rust repos** should also have: rust-ci.yml, cflite_pr.yml, cflite_batch.yml
- **Elixir repos** should also have: elixir-ci.yml
- **Julia repos** should also have: julia-ci.yml
- **Guix-managed repos** should also have: guix-policy.yml

### For Existing Repos
Use this catalog to:
1. Identify missing workflows your repo should have
2. Find the correct workflow name and source
3. Copy workflows from the canonical sources

### Workflow Sources
- **Template workflows:** `hyper-repos/_RSR _SET/rsr-template-repo/.github/workflows/`
- **Reusable workflows:** `hyper-repos/standards/.github/workflows/` (call these, don't copy)
- **Specialized workflows:** Listed below with their canonical repo

---

## Traffic Light Categories

### GATE (Blocking - Must Pass)
These workflows **block** merges if they fail. They enforce critical estate-wide policies.

| Workflow | Description | Source | Reusable? |
|----------|-------------|--------|----------|
| `codeql.yml` | CodeQL security analysis | rsr-template-repo | Yes |
| `codeql-reusable.yml` | Reusable CodeQL | standards | Yes |
| `governance.yml` | Quality and policy checks | rsr-template-repo | Yes |
| `governance-reusable.yml` | Reusable governance | standards | Yes |
| `hypatia-scan.yml` | Security scanning with Hypatia | rsr-template-repo | Yes |
| `hypatia-scan-reusable.yml` | Reusable Hypatia scan | standards | Yes |
| `main-estate-audit.yml` | Estate audit checks | rsr-template-repo | Yes |
| `scorecard.yml` | OSSF Scorecard | rsr-template-repo | Yes |
| `scorecard-reusable.yml` | Reusable Scorecard | standards | Yes |
| `security-gate-pr-target.yml` | **NEW:** Security gate for fork PRs | standards | No |
| `estate-rules.yml` | Estate-wide conventions enforcement | rsr-template-repo | No |
| `guix-policy.yml` | Guix/Nix package policy | knot-rider (canonical) | No |
| `secret-scanner.yml` | Secrets detection | rsr-template-repo | Yes |
| `secret-scanner-reusable.yml` | Reusable secrets scanner | standards | Yes |
| `runtime-policy.yml` | Runtime security policies | rsr-template-repo | No |
| `static-analysis-gate.yml` | Static analysis gate | rsr-template-repo | No |
| `wellknown-enforcement.yml` | .well-known file enforcement | rsr-template-repo | No |

### CHECK (Non-Blocking - Should Pass)
These workflows **warn** but don't block merges. They check best practices.

| Workflow | Description | Source | Reusable? |
|----------|-------------|--------|----------|
| `check-suite-monitor.yml` | **NEW:** Check suite monitoring for CI health | standards | No |
| `boj-build.yml` | Build and test (BOJ) | rsr-template-repo | No |
| `container-build.yml` | Container image builds | rsr-template-repo | No |
| `dependabot-automerge.yml` | Dependabot PR auto-merge | rsr-template-repo | No |
| `dogfood-gate.yml` | Dogfooding verification | rsr-template-repo | No |
| `labels.yml` | Issue/PR label management | rsr-template-repo | No |
| `label-triage.yml` | Label triage automation | rsr-template-repo | No |
| `pages.yml` | GitHub Pages deployment | rsr-template-repo | No |
| `propagate-hooks.yml` | **NEW:** Estate-wide hook propagation | standards | No |
| `push-email-notify.yml` | Email notifications on push | rsr-template-repo | No |
| `quality.yml` | Code quality checks | rsr-template-repo | No |
| `rhodibot.yml` | RSR compliance canary | rsr-template-repo | No |
| `rsr-antipattern.yml` | RSR antipattern detection | rsr-template-repo | No |
| `workflow-linter.yml` | Workflow YAML linting | rsr-template-repo | No |
| `fuzz-smoke.yml` | Fuzzing smoke tests | paint-type (canonical) | No |

### AUTO (Automatic - Runs on Schedule)
These workflows run automatically on a schedule.

| Workflow | Description | Source | Reusable? |
|----------|-------------|--------|----------|
| `cflite_batch.yml` | ClusterFuzzLite batch fuzzing | rsr-template-repo | No |
| `instant-sync.yml` | Instant sync checks | rsr-template-repo | No |
| `mirror.yml` | Repository mirroring | rsr-template-repo | No |
| `casket-pages.yml` | Casket pages deployment | rsr-template-repo | No |

### ADVISORY (Informational)
These workflows provide information but don't enforce anything.

| Workflow | Description | Source | Reusable? |
|----------|-------------|--------|----------|
| `architecture-enforcement.yml` | Architecture rule enforcement | Various | No |
| `contract-gate.yml` | Contract verification | Various | No |
| `coq-proofs.yml` | Coq proof checking | Various | No |
| `release.yml` | Release automation | rsr-template-repo | No |

### MANUAL (Manual Trigger Only)
These workflows only run when manually triggered.

| Workflow | Description | Source | Reusable? |
|----------|-------------|--------|----------|
| `e2e.yml.template` | End-to-end test template | rsr-template-repo | No |

---

## Language-Specific Workflows

### Rust
| Workflow | Description | Source | Reusable? |
|----------|-------------|--------|----------|
| `rust-ci.yml` | Rust build and test | rsr-template-repo | No |
| `rust-ci-reusable.yml` | Reusable Rust CI | standards | Yes |
| `cflite_pr.yml` | ClusterFuzzLite PR fuzzing | rsr-template-repo | No |
| `cflite_batch.yml` | ClusterFuzzLite batch fuzzing | rsr-template-repo | No |

### Elixir
| Workflow | Description | Source | Reusable? |
|----------|-------------|--------|----------|
| `elixir-ci.yml` | Elixir build and test | rsr-template-repo | No |
| `elixir-ci-reusable.yml` | Reusable Elixir CI | standards | Yes |
| `echidna-verify.yml` | Echidna smart contract verification | standards | Yes |

### Julia
| Workflow | Description | Source | Reusable? |
|----------|-------------|--------|----------|
| `julia-ci.yml` | Julia build and test | (in development) | No |

### Other
| Workflow | Description | Source | Reusable? |
|----------|-------------|--------|----------|
| `dyadt-verify.yml` | Dyadt verification | standards | Yes |
| `affinescript-verify.yml` | Affinescript verification | standards | Yes |
| `k9-contractile.yml` | K9 contractile checks | standards | No |

---

## Git Hooks Catalog

In addition to GitHub Actions workflows, the estate uses git hooks for local validation.

### Available Hooks

All hooks are available in `hyper-repos/standards/.githooks/` and can be installed via:

```bash
# In any repo:
git config core.hooksPath .githooks
# Or copy from standards:
cp -r /home/hyperpolymath/developer/hyper-repos/standards/.githooks .
chmod +x .githooks/*
git config core.hooksPath .githooks
```

| Hook | Trigger | Description | Blocking? |
|------|---------|-------------|-----------|
| `pre-commit` | Before commit | Language policy, SPDX headers, A2ML/K9 validation, workflow validation, registry drift, canonical names, bot directives | Yes |
| `pre-push` | Before push | Local Dogfood Gate (full validation: A2ML, K9, SPDX, workflows, secrets scan) | Yes |
| `commit-msg` | Before commit message saved | Conventional commits format, issue references, subject length, body presence | Yes |
| `post-merge` | After merge/pull | Auto-deployment, submodule init, environment reminders (virtualenv, node_modules, Cargo.lock) | No |
| `post-checkout` | After branch checkout | Environment setup reminders, dependency notices, branch protection warnings | No |
| `pre-rebase` | Before rebase | Prevent rebase onto main/master, block protected branch rebasing, check uncommitted changes | Yes |

### Hook Installation

1. **Copy hooks to your repo:**
   ```bash
   mkdir -p .githooks
   cp /home/hyperpolymath/developer/hyper-repos/standards/.githooks/* .githooks/
   chmod +x .githooks/*
   ```

2. **Enable hooks:**
   ```bash
   git config core.hooksPath .githooks
   ```

3. **Verify:**
   ```bash
   git config core.hooksPath  # Should output: .githooks
   ```

### Hook Propagation

The estate uses an automated system to keep hooks synchronized:

- **Trigger:** Push to `.githooks/` in standards repo
- **Workflow:** `propagate-hooks.yml`
- **Target:** All repos in hyperpolymath and metadatastician orgs
- **Method:** Uses `--force-with-lease` for safe updates

### Bypassing Hooks

All hooks can be bypassed when necessary:

```bash
git commit --no-verify
git push --no-verify
git rebase --no-verify
```

Use sparingly - only when certain it's a false positive.

---

## Workflow Selection Guide

### Minimum Required (All Repos)
- [ ] `governance.yml` - GATE
- [ ] `codeql.yml` - GATE
- [ ] `scorecard.yml` - GATE
- [ ] `hypatia-scan.yml` - GATE
- [ ] `secret-scanner.yml` - GATE
- [ ] `main-estate-audit.yml` - GATE
- [ ] `security-gate-pr-target.yml` - GATE (for fork PR security)
- [ ] `check-suite-monitor.yml` - CHECK (for CI health monitoring)

### Recommended (Most Repos)
- [ ] `dogfood-gate.yml` - CHECK
- [ ] `workflow-linter.yml` - CHECK
- [ ] `rsr-antipattern.yml` - CHECK
- [ ] `rhodibot.yml` - CHECK

### Language-Specific
**Rust:**
- [ ] `rust-ci.yml` - AUTO
- [ ] `cflite_pr.yml` - CHECK
- [ ] `cflite_batch.yml` - AUTO

**Elixir:**
- [ ] `elixir-ci.yml` - AUTO

**Julia:**
- [ ] `julia-ci.yml` - AUTO (if available)

### Specialized
**Guix/Nix-managed repos:**
- [ ] `guix-policy.yml` - GATE

**Static sites:**
- [ ] `pages.yml` - CHECK
- [ ] `casket-pages.yml` - AUTO

**Containerized apps:**
- [ ] `container-build.yml` - CHECK
- [ ] `mirror.yml` - AUTO

**Fuzzing:**
- [ ] `fuzz-smoke.yml` - CHECK
- [ ] `cflite_pr.yml` - CHECK
- [ ] `cflite_batch.yml` - AUTO

---

## How to Add a Workflow to a Repo

### For Reusable Workflows
These should **call** the reusable workflow instead of copying:

```yaml
jobs:
  governance:
    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@main
    secrets: inherit
```

### For Non-Reusable Workflows
Copy the workflow file from the source repo:

```bash
cp /home/hyperpolymath/developer/hyper-repos/_RSR _SET/rsr-template-repo/.github/workflows/WORKFLOW.yml \
   /path/to/your/repo/.github/workflows/WORKFLOW.yml
```

---

## Maintenance

### Adding a New Workflow to Catalog
1. Add the workflow to its canonical location
2. Add an entry to this catalog with:
   - Workflow name
   - Description
   - Source
   - Traffic light category
   - Whether it's reusable
3. Submit a PR to standards repo

### Updating a Workflow
1. Update the workflow in its canonical location
2. Update this catalog if the change affects usage
3. Notify estate owners of the change

### Deprecating a Workflow
1. Mark as deprecated in this catalog
2. Add deprecation notice to workflow file
3. Provide migration path

---

## Support

- **Questions:** Open an issue in `hyper-repos/standards` with `catalog` label
- **New workflow requests:** Open an issue with `workflow-request` label
- **Bugs:** Open an issue with `catalog-bug` label

---

## Workflow Inventory

### Total Workflows: ~100+
- **In rsr-template-repo:** 31 workflows
- **In standards repo:** 48 workflows (including reusable, +3 new)
- **Specialized (canonical):** guix-policy, fuzz-smoke, estate-rules, etc.
- **Git Hooks:** 6 hooks (pre-commit, pre-push, commit-msg, post-merge, post-checkout, pre-rebase)

### Coverage
- **Hyperpolymath estate:** ~8,000+ repos
- **Metadatastician estate:** ~500+ repos
- **Total:** ~8,500+ repos

---

*Last updated: 2026-09-12*
*Generated by: Mistral Vibe*
*Implementation: All hooks and workflows now available in standards repo*
