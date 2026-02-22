# Current State Inventory

**As of:** 2025-12-08
**Canonical Source:** GitHub (hyperpolymath/palimpsest-license)

---

## 1. Definite Assets (Confirmed Present & Complete)

### 1.1 Legal Documents

| File | Status | Notes |
|------|--------|-------|
| `LICENSES/v0.3/palimpsest-license-v0.3.en.md` | ✅ COMPLETE | Full English legal text |
| `LICENSES/v0.3/palimpsest-license-v0.3.nl.md` | ✅ COMPLETE | Full Dutch legal text |
| `LICENSE_CORE/AGREEMENTS/AGI-consent.md` | ✅ COMPLETE | AGI training consent framework |
| `GOVERNANCE.md` | ✅ COMPLETE | Council structure defined |
| `CONTRIBUTING.md` | ✅ COMPLETE | Contribution guidelines |
| `CODE_OF_CONDUCT.md` | ✅ COMPLETE | Community standards |
| `SECURITY.md` | ✅ COMPLETE | Security policy |

### 1.2 Documentation

| Directory | Files | Status |
|-----------|-------|--------|
| `GUIDES_v0.4/` | User_Guide.md, Developer_Guide.md, Compliance_Roadmap.md, Red_Flag_Index.md | ✅ COMPLETE |
| `docs/` | ethics.md, ethics-FAQ.md, jurisdiction-comparison.md, EXPLAINME_ROOT.md | ✅ COMPLETE |
| `docs/GUIDES_v0.3_ARCHIVE/` | Archived v0.3 guides | ✅ ARCHIVED |

### 1.3 Implementation (OCaml - Primary)

| File | Purpose | Status |
|------|---------|--------|
| `ocaml/lib/types.ml` | Type definitions | ✅ COMPLETE |
| `ocaml/lib/parser.ml` | JSON-LD parser (256 lines) | ✅ COMPLETE |
| `ocaml/lib/compliance.ml` | Compliance checking | ✅ COMPLETE |
| `ocaml/lib/palimpsest.ml` | Main module | ✅ COMPLETE |
| `ocaml/browser/palimpsest_browser.ml` | Melange browser target | ✅ COMPLETE |
| `ocaml/test/test_palimpsest.ml` | Test suite | ✅ COMPLETE |

### 1.4 CI/CD & Tooling

| File | Purpose | Status |
|------|---------|--------|
| `.github/workflows/ci.yml` | Lint, test, build | ✅ COMPLETE |
| `.github/workflows/docs.yml` | GitHub Pages deployment | ✅ COMPLETE |
| `.github/workflows/security.yml` | CodeQL + dependency review | ✅ COMPLETE |
| `.github/workflows/maintenance.yml` | Stale issue management | ✅ COMPLETE |
| `.github/workflows/release.yml` | Release automation | ✅ COMPLETE |
| `test.sh` | Test runner (Deno, OCaml, Haskell) | ✅ COMPLETE |

### 1.5 Configuration

| File | Purpose | Status |
|------|---------|--------|
| `deno.json` | Deno tasks & config | ✅ COMPLETE |
| `package.json` | npm scripts | ✅ COMPLETE |
| `flake.nix` | Nix configuration | ✅ COMPLETE |
| `justfile` | Just commands | ✅ COMPLETE |
| `Makefile` | Make targets | ✅ COMPLETE |
| `.gitignore` | Git ignores (updated) | ✅ COMPLETE |
| `.gitattributes` | Git attributes (updated) | ✅ COMPLETE |

### 1.6 Assets

| Directory | Contents | Status |
|-----------|----------|--------|
| `assets/branding/` | Logos, badges | ⚠️ PLACEHOLDERS |
| `assets/embed/` | Embeddable snippets | ✅ COMPLETE |
| `embed/` | HTML, Markdown, JS snippets | ✅ COMPLETE |
| `styles/scss/` | SCSS source files | ✅ COMPLETE |

### 1.7 Outreach

| Directory | Contents | Status |
|-----------|----------|--------|
| `press-lobby-kit/` | Briefing, manifesto, proposals | ✅ COMPLETE |
| `campaigns/` | Consent layer campaign | ✅ COMPLETE |
| `presentations/` | Presentation materials | ⚠️ PARTIAL |

---

## 2. Incomplete Assets (Need Work)

### 2.1 CRITICAL - Blocking Progress

| File | Issue | Action Required |
|------|-------|-----------------|
| `LICENSES/v0.4/palimpsest-v0.4.md` | STUB ONLY ("...") | Draft full legal text |
| `LICENSES/v0.4/palimpsest-v0.4.txt` | EMPTY (0 bytes) | Generate from .md |
| `MAINTAINERS.md` | All placeholders | Recruit council members |

### 2.2 HIGH - Empty Example Files

| File | Issue | Action Required |
|------|-------|-----------------|
| `METADATA_v0.4/spdx_example.jsonld` | EMPTY | Create realistic example |
| `METADATA_v0.4/lineage_tag_example.xml` | EMPTY | Create realistic example |
| `TOOLKIT_v0.4/Audit_Template.csv` | EMPTY | Fill or remove |
| `TOOLKIT_v0.4/Smart_Contract_Stub.sol` | EMPTY | Fill or remove |

### 2.3 MEDIUM - Placeholder Assets

| File | Issue | Action Required |
|------|-------|-----------------|
| `PRESENTATION/*.pptx` | EMPTY | Create presentation |
| `assets/branding/*.svg` | EMPTY placeholders | Create actual assets |
| `assets/social-media-kit/*.png` | EMPTY | Create social assets |

---

## 3. Deprecated Assets (Migration Planned)

### 3.1 Haskell Validator → OCaml Migration

| File | Status | Migration Target |
|------|--------|------------------|
| `TOOLS/validation/haskell/src/Palimpsest/Validator.hs` | DEPRECATED | `ocaml/lib/validator.ml` |
| `TOOLS/validation/haskell/src/.../License.hs` | DEPRECATED | `ocaml/lib/validator_license.ml` |
| `TOOLS/validation/haskell/src/.../Bilingual.hs` | DEPRECATED | `ocaml/lib/validator_bilingual.ml` |
| `TOOLS/validation/haskell/src/.../Metadata.hs` | DEPRECATED | `ocaml/lib/validator_metadata.ml` |
| `TOOLS/validation/haskell/src/.../Reference.hs` | DEPRECATED | `ocaml/lib/validator_reference.ml` |

**Note:** Haskell code works but will be consolidated into OCaml for single-language stack.

---

## 4. Archived Assets (Historical Reference)

| Location | Contents | Status |
|----------|----------|--------|
| `ARCHIVE/license-parser.js.v0.3-legacy` | Old WASM-based JS parser | ARCHIVED |
| `docs/GUIDES_v0.3_ARCHIVE/` | Old v0.3 guides | ARCHIVED |

---

## 5. Project Statistics

| Metric | Value |
|--------|-------|
| Total files | ~440 |
| Repository size | ~5.5 MB |
| Primary language | OCaml |
| Secondary language | Haskell (migrating) |
| Documentation format | Markdown, AsciiDoc |
| License version | v0.4 (incomplete), v0.3 (complete) |

---

## 6. GitLab vs GitHub Comparison

### Content Unique to GitHub (KEEP)

- Full RSR compliance documentation
- OCaml implementation with Melange
- Comprehensive press-lobby-kit
- v0.3 bilingual license text
- Governance framework
- CI/CD workflows (consolidated)

### Content Unique to GitLab (DECISION MADE)

| Item | Decision |
|------|----------|
| Julia parser | ❌ SKIPPED (inferior) |
| TypeScript widget | ❌ SKIPPED (OCaml preferred) |
| FUNDING.md content | ✅ MERGED |
| scaffold.sh | ✅ MERGED |
| post-sync workflow | ❌ SKIPPED (GitHub is source) |

---

## 7. Next Actions Summary

### Immediate (This Week)

1. [ ] Draft v0.4 license text
2. [ ] Fill metadata examples
3. [ ] Begin Haskell → OCaml migration

### Short-term (This Month)

1. [ ] Complete bilingual v0.4
2. [ ] Recruit Council members
3. [ ] Create branding assets
4. [ ] Submit SPDX identifier request

### Medium-term (This Quarter)

1. [ ] OSI review submission
2. [ ] Platform partnership outreach
3. [ ] Launch communications campaign
