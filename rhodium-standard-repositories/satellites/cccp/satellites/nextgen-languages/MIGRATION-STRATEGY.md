# GitLab → GitHub Migration Strategy

## Goal
**GitHub is the single source of truth.** All development happens on GitHub, then mirrors out to:
- GitLab (gitlab.com/maa-framework)
- Codeberg
- Bitbucket

## Current Problem
- Original work (Aug-Oct 2025) lives on GitLab
- GitHub repos created Dec 2025, some with parallel Claude implementations
- Result: diverged repos, missing content, confusion

## The Fix (4 Phases)

### Phase 1: Language Repos (This Week)
Priority repos - the 8 languages in this ecosystem:

| Repo | GitLab Status | GitHub Status | Action |
|------|---------------|---------------|--------|
| eclexia | 13 commits, 70-page white paper, Rust compiler | 1 commit (template) | **SYNC FROM GITLAB** |
| phronesis | 11+ commits, original design | Claude Elixir impl | Evaluate both, merge best |
| wokelang | 20+ commits, original design | Claude Rust impl | Evaluate both, merge best |
| oblibeny | 40+ commits | 30 commits, diverged | Manual merge |
| anvomidav | Mislabeled as "betlang" | Correct name | Sync + rename on GitLab |
| betlang | Correct | Correct | Verify sync |
| julia-the-viper | Original | Check status | Sync if needed |
| my-lang (solo/duet/ensemble) | Original | Check status | Sync if needed |

### Phase 2: Full Audit
Run reconciliation script to categorize all 400+ repos:
```bash
export GITLAB_TOKEN="glpat-..."
export GITHUB_TOKEN="ghp_..."
python scripts/repo-reconcile.py > audit-results.json
```

Categories:
- **gitlab-only**: Create on GitHub, sync content
- **github-only**: Keep (new work) or archive (abandoned)
- **synced**: Set up mirror
- **diverged**: Manual review needed

### Phase 3: Consolidate
For each diverged repo:
1. Compare commits on both sides
2. If GitLab has original work + GitHub has useful additions → merge
3. If GitHub work is just parallel implementation → replace with GitLab
4. If GitHub work is better → keep GitHub, archive GitLab

### Phase 4: Mirror Setup
Once GitHub is authoritative:
```
GitHub (source)
    ├── → GitLab (push mirror)
    ├── → Codeberg (push mirror)
    └── → Bitbucket (push mirror)
```

GitHub Settings → Branches → Add mirror for each target.

---

## Instructions for Claude Sessions

**READ THIS BEFORE WORKING ON ANY REPO IN THIS ECOSYSTEM**

1. **GitHub is source of truth** - always pull from GitHub first
2. **Check this document** for repo status before making changes
3. **Never create parallel implementations** - if GitLab has content, sync it first
4. **Update MIGRATION-STATUS.md** after completing any sync operation
5. **Preserve original work** - GitLab content represents months of human effort

## Immediate Priority
**Sync eclexia first** - it has a 70-page white paper and Rust compiler that only exists on GitLab.

```bash
# Preserve GitLab work to GitHub
git clone git@gitlab.com:maa-framework/4a-languages/eclexia.git
cd eclexia
git remote add github git@github.com:hyperpolymath/eclexia.git
git push github main --force
```
