# Migration Status Tracker

Last updated: 2025-12-16

## Phase 1: Language Repos

| Repo | Status | Notes |
|------|--------|-------|
| eclexia | NOT SYNCED | Priority 1 - has 70-page white paper on GitLab |
| phronesis | DIVERGED | GitLab: original, GitHub: Claude Elixir impl |
| wokelang | DIVERGED | GitLab: original, GitHub: Claude Rust impl |
| oblibeny | DIVERGED | Needs manual merge |
| anvomidav | NAMING ISSUE | GitLab calls it "betlang" incorrectly |
| betlang | TO VERIFY | Check if synced |
| julia-the-viper | TO VERIFY | Check if synced |
| solo | TO VERIFY | Part of my-lang family |
| duet | TO VERIFY | Part of my-lang family |
| ensemble | TO VERIFY | Part of my-lang family |

## Completed Syncs

| Repo | Date | By | Notes |
|------|------|----|-------|
| (none yet) | | | |

## Repos Needing Manual Review

| Repo | Reason | Decision |
|------|--------|----------|
| phronesis | Both have implementations | TBD |
| wokelang | Both have implementations | TBD |
| oblibeny | Significant divergence | TBD |

---

## Update Instructions

When you complete a sync:
1. Move repo from "Phase 1" to "Completed Syncs"
2. Add date and notes
3. Commit this file with message: `docs: update migration status - synced {repo}`
