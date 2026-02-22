# Reconciliation Audit Report

**Date:** 2025-12-08
**Action:** GitHub/GitLab Reconciliation
**Canonical Source:** GitHub

---

## Executive Summary

GitHub is now the single source of truth. GitLab content has been audited, useful items merged, and the implementation stack consolidated to **OCaml with Melange** for browser support.

---

## 1. What Was DITCHED (Removed)

| Item | Location | Reason |
|------|----------|--------|
| Julia License Parser | `TOOLKIT_v0.4/Julia_License_Parser/` | Only 7 lines, inferior to OCaml parser |
| Python validation | `ARCHIVE/python-validation-replaced-2024-11/` | Deprecated, replaced by OCaml |
| README backup | `README.md.v0.3.bak` | Unnecessary backup file |
| TypeScript widget | GitLab `src/widget-ts/` | Not imported - OCaml/Melange preferred |
| post-sync.yml | GitLab workflows | GitHub is source of truth, no sync needed |
| Elixir ignores | `.gitignore` | Elixir not in stack |
| Jupyter ignores | `.gitignore` | Jupyter not in stack |

---

## 2. What Was KEPT (Superior/Essential)

| Item | Source | Reason |
|------|--------|--------|
| OCaml parser/validator | GitHub `ocaml/` | Full-featured (256 lines), Melange browser support |
| Consolidated workflows | PR #41 | Clean, minimal, annotated |
| RSR compliance docs | GitHub | More complete than GitLab |
| Bilingual v0.3 license | GitHub | Full EN/NL legal text |
| Governance structure | GitHub | Complete council framework |
| Press/lobby kit | GitHub | More developed than GitLab `outreach/` |

---

## 3. What Was ASSIMILATED (Merged In)

| Item | From GitLab | Added To |
|------|-------------|----------|
| FUNDING.md content | Root | `/FUNDING.md` |
| scaffold.sh | `src/scripts/` | `/TOOLS/scripts/scaffold.sh` |

---

## 4. What Is DEBATABLE (Needs Decision)

### 4.1 Haskell Validator → OCaml Migration

**Current State:** `TOOLS/validation/haskell/` contains ~15 Haskell files doing:
- License structure validation
- Bilingual consistency checking
- Cross-reference validation
- Metadata schema validation

**Decision Made:** Migrate to OCaml
**Rationale:** Single-language stack, Melange browser support, avoid language sprawl

**Migration Plan:**
1. Port `Palimpsest.Validator.License` → `ocaml/lib/validator_license.ml`
2. Port `Palimpsest.Validator.Bilingual` → `ocaml/lib/validator_bilingual.ml`
3. Port `Palimpsest.Validator.Reference` → `ocaml/lib/validator_reference.ml`
4. Add CLI interface via Cmdliner
5. Deprecate Haskell after migration complete

### 4.2 Empty Placeholder Files

These files exist but are empty (0 bytes):
- `LICENSES/v0.4/palimpsest-v0.4.txt` - **CRITICAL: License text needed**
- `METADATA_v0.4/spdx_example.jsonld` - Example needed
- `METADATA_v0.4/lineage_tag_example.xml` - Example needed
- `TOOLKIT_v0.4/Smart_Contract_Stub.sol` - Needs content or removal
- `TOOLKIT_v0.4/Audit_Template.csv` - Needs content or removal
- `PRESENTATION/Palimpsest_License_Overview_v0.4.pptx` - Needs content or removal
- Various asset placeholders

**Decision Needed:** Fill these or remove them?

### 4.3 Legacy JS Parser

**Current State:** `ARCHIVE/license-parser.js.v0.3-legacy` (6.4KB) - WASM-based parser

**Options:**
1. Keep in ARCHIVE for historical reference
2. Delete entirely (OCaml is the implementation)
3. Port useful parts to OCaml

**Recommendation:** Keep in ARCHIVE, document as superseded

---

## 5. What Is OUT OF DATE

| Item | Issue | Action |
|------|-------|--------|
| GitLab repo | Diverged from GitHub | Mirror from GitHub (read-only) |
| v0.4 license text | Stub only | **CRITICAL: Write full text** |
| MAINTAINERS.md | All placeholders | Recruit council members |
| CI workflow | Had `actions/checkout@v6` | Fixed to v4 |

---

## 6. Current Implementation Stack

```
┌─────────────────────────────────────────────────────┐
│                 IMPLEMENTATION STACK                 │
├─────────────────────────────────────────────────────┤
│  OCaml + Melange     │ Primary: Parser, Validator   │
│                      │ Browser: via Melange → JS    │
├─────────────────────────────────────────────────────┤
│  Haskell             │ DEPRECATED → Migrate to OCaml│
│  (validation/)       │ CLI validation suite         │
├─────────────────────────────────────────────────────┤
│  Deno                │ JS linting/formatting        │
│                      │ Task runner                  │
├─────────────────────────────────────────────────────┤
│  SCSS → CSS          │ Styling                      │
│                      │ Built via sass               │
├─────────────────────────────────────────────────────┤
│  Nix                 │ Reproducible builds          │
│                      │ Development environments     │
├─────────────────────────────────────────────────────┤
│  GitHub Actions      │ CI/CD                        │
│                      │ Workflows consolidated       │
└─────────────────────────────────────────────────────┘
```

---

## 7. File Count Summary

- **Total files:** ~440
- **Repository size:** ~5.5MB
- **Languages:** OCaml (primary), Haskell (migrating), JavaScript (embed)

---

## 8. Recommendations

1. **URGENT:** Complete v0.4 license text
2. **HIGH:** Port Haskell validator to OCaml
3. **MEDIUM:** Fill or remove empty placeholder files
4. **LOW:** Set up GitLab as read-only mirror
