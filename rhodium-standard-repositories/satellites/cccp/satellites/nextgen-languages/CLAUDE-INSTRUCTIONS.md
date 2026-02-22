# Instructions for Claude Sessions

## STOP - Read Before Working

This repository is the **hub** for 8 programming languages in the MAAF ecosystem. Before making any changes to satellite repos, you MUST understand the current state.

## Critical Rules

### 1. GitHub is Source of Truth
All work should be on GitHub. GitLab/Codeberg/Bitbucket are mirrors.

### 2. Check Migration Status First
Before working on any language repo, check `MIGRATION-STATUS.md`:
- If status is "NOT SYNCED" → sync from GitLab first
- If status is "DIVERGED" → consult user before making changes
- If status is "SYNCED" → safe to work on GitHub version

### 3. Never Create Parallel Implementations
**WRONG:** "I'll implement this feature from scratch on GitHub"
**RIGHT:** "Let me check if this exists on GitLab first, then build on that"

### 4. Preserve Original Work
The GitLab repos contain months of human effort from Aug-Oct 2025. This includes:
- Design documents
- White papers
- Original implementations
- Research notes

Do not overwrite this with new implementations.

## The Language Ecosystem

```
nextgen-languages (this repo - hub)
├── My-Language Family (progressive complexity, ages 8-18)
│   ├── me (base)
│   ├── solo (age 8-10)
│   ├── duet (age 11-14)
│   └── ensemble (age 15-18, dialect of my-lang)
├── Foundational
│   ├── betlang (Racket, probabilistic programming)
│   └── julia-the-viper (Rust, Harvard Architecture)
└── Specialized
    ├── phronesis (practical wisdom, Elixir)
    ├── eclexia (creative synthesis, Rust)
    ├── oblibeny (favorite things modeling)
    ├── anvomidav (real-time systems)
    └── wokelang (social awareness)
```

## Related Repos (not languages)
- `7-tentacles` - orchestration
- `me-dialect-playground` - experimentation
- `my-newsroom` - content pipeline
- `my-ssg` - static site generator

## Before Starting Work

1. Read `MIGRATION-STRATEGY.md` for the overall plan
2. Check `MIGRATION-STATUS.md` for current state
3. If repo is not synced, sync it first
4. Update status file after completing work
5. Commit to the correct branch

## Questions?
If unclear about anything, ask the user rather than guessing.
