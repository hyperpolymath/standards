<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Changelog

All notable changes to `standards` will be documented in this file.

This file is generated from conventional commits by the
[`changelog-reusable.yml`](https://github.com/hyperpolymath/standards/blob/main/.github/workflows/changelog-reusable.yml)
workflow (`hyperpolymath/standards#206`). Adopt the workflow in this repo's CI to keep this file in sync automatically — see
[`templates/cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml)
for the canonical config.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- feat(governance): add scorecard-reusable.yml — close 5-candidate convergence set (#205)
- feat(changelog): add git-cliff config + reusable workflow (#206)
- feat(cartridges): canonicalise BoJ cartridge format in standards/ (#200)
- feat(governance): add secret-scanner-reusable.yml — propagate shell-secrets to 281 repos (#190)
- feat(governance): add mirror-reusable.yml — consolidate 289-repo mirror.yml drift (#187)
- feat(launcher-standard): reference impls for soft-attach + gui-dialog-chain (#179)
- feat(launcher-standard): require --version mode with machine-greppable format (#173)
- feat: consume .hypatia-baseline.json in governance gate (#166)

### Fixed

- fix(governance): eradicate inline Python from governance-reusable.yml (#189)
- fix(launcher-standard): resolve 3 cross-doc contradictions (#170)
- fix(baseline): file_pattern glob matching + jq scoping bugs (#180)
- fix(launcher-standard): move PID/log to XDG dirs (security: symlink-attack hardening) (#175)
- fix(keepopen): honour NO_COLOR and auto-strip ANSI for non-TTY stdout (#176)
- fix: checkout caller's repo in governance-reusable workflow (#165)
- fix: checkout caller's repo in governance-reusable workflow
- fix: use canonical STATE completion field
- fix(security): enforce SSH-only git remotes estate-wide (standards#69) (#147)
- fix(licence): #3 isolated — clear scaffold-placeholder leak (standards) (#139)

### Changed

- refactor(governance): subsume language-policy.yml + add deno-ci-reusable (semantics-level fix for estate-template drift) (#168)

### Documentation

- docs(policies): trusted-base reduction policy for proof debt (#203)
- docs: launcher-standard review 2026-05-26 — prose + a2ml campaign manifest (#182)
- docs(audits): admin-merge wrapper sweep 2026-05-26 (human + a2ml) (#202)
- docs: exempt palimpsest plasma licensing repo
- docs: add scaffold-stub guix.scm debt tracker (Refs standards#102)
- docs(nix-retirement): closure report + machine-readable record (#102 #103) (#149)
- docs(licence-policy): restore A6+A7 dropped by #143/#144 merge race (#146)
- docs(licence-policy): A8 — explicit owner-sanctioned scoped carve-outs (#144)
- docs(licence-policy): A6 hard-exclusions + A7 multi-SPDX FP ignore-list (ledger #2/#3) (#143)
- docs(licence-policy): A5 — scaffold-placeholder leak is NOT licence debt (#140)

### CI

- ci: add launcher-standard prose↔a2ml lock-step gate (#172)
- ci(tooling): promote standards R4 lint to strict (#159)
- ci(spark): SPARK Theatre Gate reusable workflow (#135) (#141)

## Pre-history

Prior commits to this file's introduction are recorded in git history but not formally classified into Keep-a-Changelog sections. To backfill, run `git cliff -o CHANGELOG.md` locally using the canonical [`cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml) — this is one-shot mechanical work.

---

<!-- This file was seeded by the 2026-05-26 estate tech-debt audit follow-up (Row-2 Phase 3); see [`hyperpolymath/standards/docs/audits/2026-05-26-estate-documentation-debt.md`](https://github.com/hyperpolymath/standards/blob/main/docs/audits/2026-05-26-estate-documentation-debt.md). -->
