<!--
SPDX-License-Identifier: AGPL-3.0-or-later
SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
-->

# Estate License Policy — Canonical (2026-06-02)

> Owner directive 2026-06-02:
>
> > "mpl-2.0 is for my sole repos, all rights reserved is for 007, agpl-3.0-or-later is for those shared with my son, and leave other people's forked stuff alone … only palimpsest license for obvious reasons should be talking about palimpsest and palimpsest plasma, and consent-aware-http, but in that case prospectively"

This document is the canonical reference for license classification across the `hyperpolymath/*` estate. It is the source of truth for:

- Repository LICENSE files
- Source-file `SPDX-License-Identifier:` headers
- `Cargo.toml` / `package.json` / `deno.json` / `mix.exs` `license` fields
- Audit tooling exclusions (hypatia recipes, gitbot remediation sweeps)

## The five-way classification

The classification is **mutually exclusive and exhaustive** — every repo in the estate falls into exactly one row.

| # | Category | License | Applies to |
|---|---|---|---|
| **1** | **Sole owner repos (default)** | `MPL-2.0` | Every repo authored by the owner alone. Default for ~all of `hyperpolymath/*` unless one of the other rows applies. |
| **2** | **All Rights Reserved** | ARR | `hyperpolymath/007` specifically (dual-use classified). Out of scope for any normalisation, scanning, or labelling. Surface to owner only. |
| **3** | **Shared with son (Joshua)** | `AGPL-3.0-or-later` | Repos with son as co-author/maintainer. **Confirmed**: `idaptik`, `burble`, `standards`, `rattlescript`, `vcl-ut`. Permanent. |
| **4** | **Third-party / forks / not yours** | LEAVE ALONE | Whatever upstream chose. Never sweep, never normalise. Flag as out-of-scope if it surfaces in an audit. |
| **5** | **Palimpsest carve-out (extremely narrow)** | `PMPL-1.0-or-later` (palimpsest-license + palimpsest-plasma) / **hybrid** (consent-aware-http) | TWO repos as `PMPL-1.0-or-later`: `palimpsest-license` + `palimpsest-plasma`. `consent-aware-http` (currently subdir of `standards/consent-aware-http/`, possibly future standalone) has **hybrid** licensing — see below. NOWHERE ELSE PMPL. |

## consent-aware-http hybrid licensing

`consent-aware-http` is **not** licensed under `PMPL-1.0-or-later`. The newer design (owner directive 2026-06-02) is:

| Aspect | License |
|---|---|
| Source files (`.rs`, `.ml`, etc.) | `MPL-2.0` |
| Doc/spec files (`.md`, `.adoc`, RFC drafts) | `CC-BY-4.0` (IETF-compatible) |
| In-text reference | Spec text **references** `PMPL-2.0-or-later` for cultural/ethical/post-quantum framing. PMPL is referenced as a sibling framework, NOT applied as the file SPDX. |

Rationale: no existing licence covers both IETF tooling-compatibility AND cultural/ethical/post-quantum scaffolding. The hybrid gives both.

## How the policy is enforced

### Memory layer
Canonical source for agents: `~/.claude/projects/-home-hyperpolymath-developer-repos/memory/feedback_estate_license_policy_umbrella.md` and the per-repo sibling memories (`feedback_burble_agpl_intentional.md`, `feedback_standards_agpl_intentional.md`, etc.).

### Hypatia recipes
- `data/verisim/recipes/recipe-fix-pmpl-drift.json` — detects legacy `PMPL-1.0-or-later` SPDX stamps that drift from each repo's category target; auto-classifies via LICENSE file; sweeps PMPL→target (MPL or AGPL).
- `data/verisim/recipes/recipe-fix-spdx-license.json` — older AGPL→MPL recipe, now scoped via `exclude_repos` to skip the confirmed AGPL category-3 repos.
- `scripts/fix-scripts/fix-pmpl-drift.sh` — category-aware fix script with submodule and sub-path exclusions.

### Validation
- `scripts/fix-scripts/fix-pmpl-drift.sh --dry-run <repo>` lists drift without changes.
- `hooks/validate-spdx.sh` blocks committing workflow files without an SPDX header.

### Hard exclusions (never sweep)
- Submodule paths (per `.gitmodules`)
- Sub-paths: `rescript-tea/`, `rescript-vite/`, `affinescript-vite/` (possible upstream forks)
- Sub-paths: `idaptik-rescript13-staging/` (AGPL inheritance from son-shared idaptik)
- Sub-paths: `consent-aware-http/` (hybrid licensing per above)
- Check-in copies of standalone repos (e.g. `dev-ecosystem/affinescript-ecosystem/rattlescript/` → fix in `hyperpolymath/rattlescript` standalone)

## Process rules

1. **License/SPDX changes are MANUAL, file-by-file, owner-only, third-party untouchable.** Never auto-PR a licence change even if policy-correct without explicit owner approval.
2. **Per-repo scoping required.** Bulk-sweep PRs across multiple repos are blocked by classifier; each repo gets its own PR with explicit scope description.
3. **Per-subdir scoping for monorepos.** For repos like `developer-ecosystem`, `standards`, file separate PRs per top-level subtree, not one giant PR.
4. **LICENSE replacement is more invasive than SPDX flip.** Where a sub-project has its own PMPL LICENSE file (not just source SPDX drift), the LICENSE replacement gets its own phase/PR with explicit notice.
5. **Discovered category-3 repos go in memory immediately.** Always update `feedback_estate_license_policy_umbrella.md` (the five-way table) plus a new `feedback_<repo>_agpl_intentional.md` sibling file when a new AGPL category-3 repo is confirmed.

## 2026-06-02 initial sweep — reference PRs

The PMPL→target sweep that surfaced this canonical policy landed via these PRs across 25 repos (≈14k files swept):

| Repo | PR(s) | Files | Target |
|---|---|---|---|
| neurophone | #102 (canonical revert), #120 (docs reintro) | 100 + 8 | MPL-2.0 |
| developer-ecosystem | #103/#104/#105/#106 | 7,033 | MPL-2.0 |
| nextgen-databases | #29 / #30 | 226 | MPL-2.0 |
| standards | #344 / #345 | 3,417 | **AGPL-3.0-or-later** |
| stapeln | #86 / #87 | 875 | MPL-2.0 |
| reposystem | #90 (Phase 1) | 401 | MPL-2.0 |
| ephapax | #285 | 343 | MPL-2.0 |
| sanctify-php | #47 | 123 | MPL-2.0 |
| ubicity | #73 | 131 | MPL-2.0 |
| zerotier-k8s-link | #63 | 110 | MPL-2.0 |
| verisimiser | #156 | 274 | MPL-2.0 |
| squisher-corpus | #26 | 126 | MPL-2.0 |
| rattlescript | #29 | 240 | **AGPL-3.0-or-later** |
| pimcore-fortress | #22 | 87 | MPL-2.0 |
| tree-navigator | #41 | 75 | MPL-2.0 |
| snapcreate | #29 | 65 | MPL-2.0 |
| php-aegis | #43 | 92 | MPL-2.0 |
| nafa-app | #26 | 103 | MPL-2.0 |
| vscode-k9 | #22 | 219 | MPL-2.0 |
| supernorma | #38 | 72 | MPL-2.0 |
| sdp-hkdf-deployment | #26 | 81 | MPL-2.0 |
| robot-vacuum-cleaner | #64 | 81 | MPL-2.0 |
| gitbot-fleet | #250 | 148 | MPL-2.0 |
| vcl-ut | #46 | 32 | **AGPL-3.0-or-later** |
| resource-record-fluctuator | #38 | 55 | MPL-2.0 |
| proof-of-work | #83 | 118 | MPL-2.0 |
| project-wharf | #49 | 68 | MPL-2.0 |
| julia-professional-registry | #25 | 16 | MPL-2.0 |
| hypatia | #423 (governance recipe) | 3 (recipe+script+exclude_repos update) | – |

## Known deferred work

- `consent-aware-http` hybrid handling — 39 files in `standards/consent-aware-http/` need file-type-aware sweep (source vs spec vs reference)
- `reposystem` Phase 2 — 7 sub-projects with own PMPL LICENSE pending standalone-vs-local classification
- `panll` — owner-managed, never sweep without explicit per-touch greenlight
- Tail repos with 1-2 PMPL stamps each that didn't make the original audit cap

## Related memory references

- `feedback_estate_license_policy_umbrella.md` — canonical 5-way table
- `feedback_no_automated_licence_edits.md` — meta-rule against bulk auto-PR
- `feedback_pr_sweep_title_keyword_exclusion.md` — guardrail against sweeping license PRs
- Per-repo: `feedback_{burble,standards,rattlescript,vcl_ut,idaptik}_agpl_intentional.md`
- Carve-out specifics: `feedback_consent_aware_http_hybrid_licensing.md`
