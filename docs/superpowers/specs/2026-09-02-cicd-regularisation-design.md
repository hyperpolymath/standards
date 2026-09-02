<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- Design spec — CI/CD regularisation. Approved by owner 2026-09-02 (plan mode). Execution order §10; owner decisions §12 tracked in the linked issue. -->

# CI/CD regularisation — register, canonical set, universal settings, owner split

Plan file = the design spec for this architectural task (plan mode forbids writing
`docs/superpowers/specs/`; commit this content there as step 0 of execution).

## 1. Context

The estate has 435 canonical repos (389 hyperpolymath, 46 metadatastician), 428 with
workflows, 6,008 workflow files, mean 14 per repo. They were never designed as a set:
`rsr-template-repo` is canonical only because it was copied, there is no ratified
MUST list, and four ruleset waves were hand-applied per repo because a User account
has no inheritance. Result: the most-required gate (governance) is red on 78% of
repos, CodeQL exists in 246 logic variants, three secret scanners overlap, four Pages
deployers overlap, and rulesets look random because they *are* independent copies.

Owner asks (verbatim intent): list every CI/CD procedure with viable / not viable /
redundant; remake from scratch for use everywhere; universal repo settings with
per-repo deltas; what lives in metadatastician vs hyperpolymath; why rulesets differ
repo to repo; what the "links/connections" settings item is.

### Horizon of every number below
- **Run census**: latest 100 runs per repo, 428 repos, pulled 2026-09-02
  (`$CLAUDE_JOB_DIR/tmp/runs-latest100.tsv`, 40,199 rows; `latest.tsv` = latest
  conclusion per (repo, workflow), 5,363 rows).
- **File census**: local canonical checkouts, deduped by origin URL (agent report
  2026-09-02). Local disk is stale for *content* (memory: half of AGENTIC fixes
  already on remote); it is adequate for *presence* counts.
- **Disabled census**: `dev-notes/cicd/census-workflows-2026-08-26.tsv`
  (5,189 active / 781 disabled_manually). Disabled workflows never appear in a run
  census, so both instruments are needed.
- **Reconciled discrepancy**: labels.yml absent from the 08-26 census but in 413
  repos now = an undocumented 413-repo sweep starting 2026-08-27T13:32Z (all 1,741
  runs named `Labels`, 1,328 push-triggered). Both instruments were right.

## 2. Rulings recorded this session (owner)

| # | Ruling | Effect on plan |
|---|---|---|
| R1 | Remove CodeFactor, Snyk, Mergify, ImgBot, Codacy | §9 never-re-add list; strip bypass actor 56611 (codacy) |
| R2 | Pin posture = **SHA pins + `actions.lock` everywhere** | Canonical set carries lockfile + verify step; same-PR regen rule is a MUST (§6.4) |
| R3 | Bypass actors to KEEP: claude, dependabot, github-actions, coderabbitai, gitguardian, copilot-swe-agent, codex-connector, sonarqubecloud, **oikosbot** | §7.3 |
| R4 | Bypass actors to GO: gitar-bot (827041), codecov (254), + R1 set | §7.3 |
| R5 | Renovate: owner asked for a ruling. **Ruling: remove, with one caveat.** Dependabot gained native Julia (`package-ecosystem: julia`) in Dec 2025. 52 Julia repos, 0 Julia Dependabot entries, 0 Renovate configs today → add `julia` to Dependabot in those 52. **Caveat**: the estate hosts a custom registry (`hyper-repos/_JULIA_LIBRARIES _SET/julia-professional-registry/Registry.toml`). Whether Dependabot's Julia support resolves packages from a private registry, not only General, is unverified. Pilot one Julia repo that depends on that registry (§10 step 4) before removing Renovate's install; if Dependabot cannot see it, Renovate stays **for those repos only** | §5 register, §7.5, §12 O7 |
| R6 | "Links thing" = **Autolink references** (confirmed). Six-prefix set was a one-repo trial that got copied (28/40 sampled repos identical, 12 none); needs an audit to expand per repo type | §7.6 |
| R7 | Games, gimmicks, applications → metadatastician; proofs/maths, estate-wide tooling (echidna, reposystem, panic-attack, pons-asinus, hypatia, gitbot-fleet, .git-private-farm) → hyperpolymath; fine detail deferred | §8 |

**Owner actions (not blocking, batch into one issue):**
- O1 Open https://education.github.com dashboard → "Upgrade your academic organizations": does it list `metadatastician`? If yes, click upgrade (free). Also apply for GitHub for Nonprofits (Team free, no education requirement). Until either lands, metadatastician stays on per-repo rulesets (live 403 confirmed 2026-09-02: "Upgrade to GitHub Team").
- O2 Confirm the Settings App (Probot) is actually installed; if not, `.github/settings.yml` is dead weight and gets deleted.
- O3 Dispositions in §5 marked **OWNER?** (BoJ trigger, mirror targets, rhodibot, ClusterFuzzLite scope).

## 3. Verdict rubric (applied to every row of §5)

Three independent bits plus one relation. "Latest run green" measures only bit C.

| Bit | Question | Evidence used |
|---|---|---|
| A. Purpose still in force? | Does estate doctrine still want this outcome? | Rulings (Deno banned, Python banned, Bun runtime, #39 replaces mail), disabled census |
| B. Can it fail? | Has it ever gone red for a real reason, or is it structurally green? | Fake-gate memories (11/26 cicd-suite composites cannot fail; e2e no-op; `grep -q` NUL; fail-open) |
| C. Does it pass now? | Latest conclusion per repo | Run census |
| R. Redundant? | Another procedure produces the same signal | Co-occurrence + caller census |

Verdicts: **KEEP** (A✓ B✓, fix C) · **REMAKE** (A✓, B or C broken by implementation) ·
**DROP** (A✗) · **FOLD→X** (R: merge into X) · **OWNER?** (A unknown).

## 4. Why rulesets look random, and what the "links thing" is

**Rulesets.** hyperpolymath is a User account: no org rulesets, no inheritance, so every
ruleset is a hand-made copy. Four waves (04-10 launch-scaffolder audit, 08-04 backup,
08-07 pages-deadlock, 08-27 Optimus-Branch) each applied a different template to a
different subset, and third-party apps each added their own bypass actor and check
context. Required contexts are job names typed by hand, so renames produced 7 CodeQL
and 4 Hypatia spellings; `paint-type` requires 31 contexts. Every Optimus-Branch copy
carries rules nothing can satisfy (coverage 95%, github-pages deployment, Copilot
review), which forces `--admin` merges, which bypass everything. Cure = one canonical
ruleset JSON, one applier, one verifier, contexts *derived* from emitted job names.

**Links thing.** Settings → Integrations → **Autolink references**: prefix → URL
templates that turn `RUSTSEC-2024-0001` in issues/PRs into links. Six prefixes
(GHSA-, PROV-, CVE-, ADR-, RUSTSEC-, RFC-) on 28/40 sampled repos, none on 12. Never
audited; §7.6 does that.

## 5. Procedure register

Columns: repos with file (F), latest ✅/❌ (run census), disabled (D), verdict.
Thin = calls `hyperpolymath/standards/...-reusable.yml`.

### 5.1 The six thin callers of `standards` reusables (the spine)

| Procedure | F | ✅/❌ | Bits | Verdict |
|---|---|---|---|---|
| `scorecard.yml` → scorecard-reusable | 414 (410 thin) | 196/84 +28 startup_failure | A✓ B✓ | **KEEP · PERIODIC**. Fix the 3 `name:` variants (211/108/91) → one name. Fold `openssf-compliance.yml` (55, co-occurs 55) and `scorecard-enforcer.yml` (12, 1 green) into it |
| `secret-scanner.yml` → secret-scanner-reusable | 400 (369 thin, 31 inline) | 274/123 +10 sf | A✓ B✓ | **KEEP · GATE**. Root-cause the 123 reds (gitleaks `[extend]` wipe, blind `.adoc`). Drop `cicd-suite/secrets-check` (same job, 60 repos) |
| `governance.yml` → governance-reusable | 352 (350 thin) | 78/251 +24 sf | A✓ B✓ C✗ | **REMAKE · GATE**. 78% red = unsatisfiable sub-checks (SPDX line-1, `.md→.adoc` 56 dead gates, AGENTIC wording stalled sweep). Rebuild as N named jobs (one context each) so a red names its cause |
| `hypatia-scan.yml` → hypatia-scan-reusable | 355 (313 thin, 42 inline) | 256/75 +28 sf | A✓ B½ | **KEEP · GATE** after engine fix: 34 rule modules, CLI runs 14; blind to `.github/`. Until fixed it is a partial gate; say so in the tier doc |
| `mirror.yml` → mirror-reusable | 312 (272 thin) | 66/81, **142 skipped**, 19 sf | A? | **OWNER?** 142 skipped = forge secrets absent. Keep PERIODIC on repos with targets; disable elsewhere. `instant-sync.yml` (275 F, **266 disabled**, 2 active) → **DROP** |
| `rust-ci.yml` → rust-ci-reusable | 119 (105 thin) | 61/46 +8 sf | A✓ B✓ | **KEEP · GATE (Rust repos)**. Fold `cargo-audit.yml` (22 F, 3 green) via `enable_audit`; fold `rust.yml` (5), `coverage.yml` (5) |

### 5.2 Security / static analysis

| Procedure | F | ✅/❌ | Verdict |
|---|---|---|---|
| `codeql.yml` inline (349 inline, **246 variants**) + CodeQL default setup (51 dynamic) | 350 | 230/52 +58 sf; dynamic 34/0 +17 sf | **REMAKE · GATE**: thin caller of `codeql-reusable.yml` (exists, 1 caller). One mode estate-wide (advanced via reusable; turn default setup off — memory: default-vs-advanced conflict, no-JS crash) |
| `static-analysis-gate.yml` | 94 (co-occurs with codeql in 90) | 84/8 | **FOLD→codeql** |
| `semgrep.yml` | 6 | 3/0 +3 sf | **DROP** (CodeQL covers; 50% startup_failure) |
| `sonarqube.yml` | 21 | 9/13 | **KEEP · ADVISORY opt-in** (owner keeps Sonar). Never a required context (pin rule ⊥ lockfile memory) |
| `workflow-linter.yml` | 141 | 69/66 +6 sf | **FOLD→governance** as one job; it misses inline `uses:` today, fix while folding |
| `allowlist-preflight-reusable.yml` | 0 callers, but `Allowlist Preflight` is a REQUIRED context red 331/332 | — | **DROP as required context**; keep PERIODIC only where estate PAT exists |
| `oikosbot.yml` | 18 | 6/2 +8 sf | **KEEP · CHECK** (owner's own). Green must prove SARIF uploaded; digest orphan cured 09-02 |
| `github-advanced-security` (native) | 14 | 3/2 +9 sf | **KEEP native** (push protection, secret scanning) — settings, not workflow |
| `security-policy.yml` 61 · `runtime-policy.yml` 47 · `wellknown-enforcement.yml` 60 · `estate-rules.yml` 40 (8 ✅/30 ❌) · `language-policy.yml` 12 · `guix-policy.yml` 25 + `guix-nix-policy.yml` 35 (presence-only = fake) · `container-policy.yml` 7 · `npm-bun-blocker.yml` 7 (contradicts Bun ruling) · `ts-blocker.yml` 7 · `rsr-antipattern.yml` 24 (calls a reusable that **does not exist**) | policy family | mixed | **FOLD→governance**: each becomes one named job in governance-reusable; every job must have a planted-positive test. Delete the files |
| `dogfood-gate.yml` | 240 inline, **164 variants** | 142/91 +9 sf | **REMAKE→governance** (RSR self-checks; 164 variants = bespoke rot) |
| `main-estate-audit.yml` + `cicd-suite` 26 composites | 61 | 1/29 +6 sf | **REMAKE · PERIODIC**: audit each composite for can-fail (11/26 cannot); keep the library, drop it as a gate |
| `quality.yml` 61 · `comprehensive-quality.yml` 4 | | 40/14 · 0/3 | **FOLD→ language caller** |

### 5.3 Build / test (language gates)

| Procedure | F | ✅/❌ | Verdict |
|---|---|---|---|
| `ci.yml` (73 variants) · `CI.yml` · `ci-cd.yml` · `build.yml` · `test.yml` | 85 · 4 · 3 · 4 · 6 | 27/50 · 0/2 · 0/2 · 0/2 · 1/4 | **REMAKE**: one thin caller per language (`rust-ci`, `elixir-ci`, `ada-ci`, `julia-ci`, `bun-ci`, `zig-ci`); delete the generic files |
| `elixir-ci.yml` (thin) + `elixir.yml` | 7 + 3 | 4/1 · 1/2 | **KEEP thin; FOLD `elixir.yml`** |
| `ada.yml` 3 · `spark-theatre-gate.yml` 16 (11/3) · `proofs.yml` 8 (6/1) · `abi-ffi-gate.yml` 20 (18/0) · `k9-svc-validation.yml` 8 · `echidna-validation.yml` 3 | | | **KEEP · GATE opt-in by repo type** — these are the hyperpolymath core (proofs, ABI, SPARK). `proofs` becomes GATE on every proof repo |
| `rescript-deno-ci.yml` 12 (0/11) · `deno-ci.yml` 3 · `deno-ci-reusable.yml` (0 callers) | | | **DROP** (Deno banned; Bun runtime) |
| `boj-build.yml` "BoJ Server Build Trigger" | 255 inline | 214/14 | **OWNER?** BoJ is in the archived index; if BoJ is retired this is 255 dead triggers → DROP |
| `e2e.yml` | 57 | 11/33 | **DROP** unless a real suite exists (memory: placeholder no-op) |
| `cflite_batch.yml` 24 · `cflite_pr.yml` 23 | | 7/15 · 4/6 | **OWNER?** ClusterFuzzLite: keep opt-in on fuzz-worthy Rust/Zig repos only |
| `container-build.yml` 25 · `container.yml` 3 | | 1/2 · 0/3 | **REMAKE opt-in** (one container reusable) |

### 5.4 Publishing / Pages / release

| Procedure | F | ✅/❌ / D | Verdict |
|---|---|---|---|
| `casket-pages.yml` (Ddraig SSG) | 213 | **6/115**, 77 cancelled | **REMAKE**: one `pages.yml` opt-in only where a site exists; drop the `required_deployments: github-pages` ruleset rule that deadlocks it |
| `pages.yml` | 221 | 15/13, **178 disabled** | FOLD into the remade one |
| `jekyll-gh-pages.yml` 13 · `jekyll.yml` 4 | | 10/2 · 1/3 | FOLD |
| `release.yml` | 139 | 1/5 (+1 queued) | **REMAKE**: one release reusable built on `changelog-reusable.yml` (exists, 0 callers) + `proven/publish-*.yml` pattern |
| `generator-generic-ossf-slsa3-publish.yml` | 21 | — | KEEP opt-in (SLSA provenance) |

### 5.5 Estate plumbing / notifications / bots

| Procedure | F | ✅/❌ / D | Verdict |
|---|---|---|---|
| `labels.yml` (08-27 sweep) | 413 | 411/1 | **KEEP · PERIODIC**; prove it can fail (planted positive). FOLD `label-triage.yml` (24) |
| `push-email-notify.yml` | 341 | 0 green, **325 disabled**, 18 skipped, 11 sf | **REMAKE · dormant** (owner ruling 2026-09-02: "replace it everywhere"). Keep the file and its `vars.PUSH_EMAIL_ENABLED == 'true'` gate unchanged; repoint `uses:` from `dawidd6/action-send-mail` to `hyperpolymath/smtp-notify-action@1b3b752d39a4fe4c0f28f10905e4608789d3e050 # v0.1.0` (#39; drop-in inputs) with `actions.lock` regenerated in the same PR (§6.4). The 325 disabled copies stay disabled until the owner re-arms them (no SMTP secrets exist). Sweep engine = first run of step 5 |
| `dependabot-updates` (native) | 269 | 165/85, 18 cancelled | **KEEP native**; fix configs: 247 `nix` + 21 `guix` entries are INVALID ecosystems (memory), 276 `pip` in a Python-banned estate, 0 `julia` (add to 52) |
| `dependabot-automerge.yml` | 137 | 21/12, **100 skipped** | **REMAKE → one thin arming job**. `allow_auto_merge=true` only *permits* auto-merge; something must still call `gh pr merge --auto --squash` per PR, which is what this file does (`tree-navigator` copy, line 120). The 100 skipped are the `if: github.actor == 'dependabot[bot]'` guard on non-Dependabot PRs: expected, not a defect. Keep the arming step as one job inside `governance-reusable` (or `gitbot-fleet`), drop the 137 bespoke copies |
| `rhodibot.yml` | 93 | 20/69 | **OWNER?** 78% red; retire or remake |
| `stale.yml` | 4 | 4/0 | KEEP optional |
| `inbox-steward*.yml` | 4 disabled | — | DROP |
| `.git-private-farm` `_rsr_{lint,policy,security,test}.yml` hookset | 0 callers | — | DROP (superseded by standards reusables) |

### 5.6 Shared building blocks

| Item | Verdict |
|---|---|
| `standards/.github/actions/signed-push` | **KEEP** — required by `required_signatures` for any bot commit |
| `a2ml-validate-action` (52) vs `a2ml-ecosystem/validate-action` (100); same for k9 (52 vs 98) | **REDUNDANT**: keep the `*-ecosystem/validate-action` coordinate, sweep the 52 |
| `readme-derive-reusable.yml` (2 callers) | KEEP opt-in |
| `cicd-suite/spdx-license-check` vs `palimpsest-license` action | FOLD into governance SPDX job |
| Broken refs: `absolute-zero/deno-ci.yml` → `-ci-reusable.yml` (blank token); 3 repos → non-existent `rsr-antipattern-reusable.yml`; 2 repos → `panic-attacker` (repo is `panic-attack`) | FIX in sweep |
| Third-party apps (R1/R4) | remove app installs + bypass entries + any workflow that exists only to feed them |

### 5.8 Maintenance layer (scripts that *apply* CI/CD, not workflows that run it)

The ask says "every procedure"; these run from a laptop or a bot, never in Actions.

| Family | What it is | Verdict |
|---|---|---|
| `gitbot-fleet/scripts/fix-*.sh` (~60) + registry JSON | per-defect estate fixers driven by the fleet | **KEEP as the sweep engine for §10 step 5**, after the two known blockers: Justfile unparseable on main and `fixer.rs` corrupt (memories). Any `fix-*` whose defect class is DROPped in §5 is deleted with it |
| `metadatastician-governance/scripts/ci-health/{sweep,detect,remediate,fix-pages-deadlock}.sh` | ad-hoc CI health remediation | **FOLD→ §7.4** applier + `settings-drift.sh`; `fix-pages-deadlock` becomes moot once `required_deployments` leaves the ruleset |
| `.git-private-farm/scripts/ci-deadlock-*.sh` | phantom-context and deadlock audit | **KEEP** as the verifier core (§7.4) |
| `cicd-suite/rollout_estate.sh` | rolls the 26 composites out estate-wide | **DROP** (the composites are demoted to PERIODIC library, no rollout) |
| seven branch-protection appliers | see §7.4 | RETIRE to one |
| `scripts/estate-board.sh` | rename-proof CI board | **KEEP**; commit into `standards/` (memory: lives outside any repo) |

### 5.7 Redundancy map (one line each)

CodeQL ×3 (inline, default-setup, static-analysis-gate) · Scorecard ×3 · Secret scan ×4
(reusable, cicd-suite, GHAS, GitGuardian) · Pages ×4 · Policy ×11 files → governance ·
CI ×5 generic files → per-language · a2ml/k9 validate ×2 coordinates · Dependency
bots ×2 (Dependabot, Renovate) · Mail ×2 (dawidd6/action-send-mail → smtp-notify-action, one caller file) ·
Sync ×2 (mirror, instant-sync) · Auto-merge ×2 (workflow, native).

## 6. The canonical set (from scratch)

### 6.1 Architecture choice

Three options considered; **recommended = A + C-lite**:

- **A. Thin SHA-pinned callers of `standards` reusables** (logic in one place; contexts
  stable; propagation = pin-bump PR from Dependabot `github-actions`). This is what
  already works for 6 procedures across 400 repos.
- **B. `workflow-templates/` in each owner's `.github` repo** (copy-on-create, zero
  propagation, only for new repos). Exists in neither owner today.
- **C. Runtime-fetched scripts** (instant propagation, supply-chain surface;
  contradicts R2).

Use A for logic; use B only as the *distribution* of the thin callers for new repos
(same files, no logic); reject C except inside reusables that already run under the
lockfile.

### 6.2 Tiers and MUST list (ratified here; lint = `standards/scripts/check-gate-tiers.sh`)

| Tier | Meaning | Required context? | Members |
|---|---|---|---|
| 🔴 GATE | blocks merge **for non-bypass actors** (see §7.3 consequence); must be able to fail; planted positive on file | yes | `governance/*` (N jobs), `secret-scanner`, `hypatia-scan`, `codeql`, `actions-lock-verify`, language gate for the repo's language(s), `proofs`/`abi-ffi`/`spark` where applicable |
| 🟡 CHECK | runs on PR, visible, not required | no | `oikosbot`, `readme-derive`, container build |
| ⚪ ADVISORY | app-driven comments | no | CodeRabbit, Sonar, GitGuardian, Copilot review |
| 📅 PERIODIC | schedule/push only | no | `scorecard`, `labels`, `mirror` (where targets), `main-estate-audit`, `stale` |

Rules: a GATE with no recorded red is unproven → CHECK until it has one. Context names
= job ids, machine-derived (`gh api .../actions/runs/{id}/jobs`), never typed.
Keep **filenames stable** (renames create phantom registrations — memory); fix `name:`
and job ids instead.

### 6.3 File set per repo (target ≤ 8 files, from 14)

```
.github/workflows/
  governance.yml        thin → governance-reusable   (GATE, N jobs)
  secret-scanner.yml    thin                          (GATE)
  hypatia-scan.yml      thin                          (GATE)
  codeql.yml            thin → codeql-reusable        (GATE)
  <lang>-ci.yml         thin, one per language        (GATE)
  scorecard.yml         thin                          (PERIODIC)
  labels.yml            thin/inline                   (PERIODIC)
  actions.lock          gh actions-lock output        (R2)
opt-in: pages.yml, release.yml, mirror.yml, proofs.yml, abi-ffi-gate.yml,
        spark-theatre-gate.yml, oikosbot.yml, sonarqube.yml, cflite_*.yml
```

### 6.4 Lockfile contract (R2, the highest-risk piece)

- Every workflow `uses:` pinned to 40-hex SHA + `# vX.Y.Z` comment.
- `actions.lock` regenerated in the **same PR** as any `uses:` change (memory:
  vexometer manifest contract). Caller entry for a reusable is `[]`.
- New GATE job `actions-lock-verify`: `gh actions-lock --no-fix` (the authoritative
  tool, not a hand-rolled check — AGENTS.md §5.3).
- **Who bumps pins.** Dependabot pin-bump PRs break the lock (memory), and a repair job
  on the Dependabot PR **cannot run**: workflows triggered by Dependabot get a
  read-only `GITHUB_TOKEN` and see only Dependabot secrets, while `signed-push`
  requires an App id + private key (`standards/.github/actions/signed-push/action.yml`
  lines 9–15). On a User account that would mean one secret per repo × 389.
  **Decision: the lock tool owns pin bumps.** Turn the `github-actions` ecosystem OFF
  in every `dependabot.yml`; add one PERIODIC `lock-refresh` workflow in `standards`
  that iterates the estate with the single App credential it already holds, runs
  `gh actions-lock` (fix mode) per repo, and opens a signed PR via `signed-push`. The
  PR then passes `actions-lock-verify` like any other. One credential, no repair
  loop, and the estate's top startup_failure cause (`Invalid lockfile`) is removed by
  construction. **Planted positive required**: one refreshed PR must go red when the
  lock is hand-corrupted.
- Fix the 44% of estate-internal `uses:` still on `@main` (1,643 lines) in the sweep.

### 6.5 Hosting

All reusables and composites stay in `hyperpolymath/standards` (estate-wide tooling →
hyperpolymath, R7). metadatastician repos call across owner; no metadatastician-hosted
reusables. **Distribution of thin callers to new repos**: GitHub's docs describe
`workflow-templates/` only for an *organization's* `.github` repo, so the
metadatastician half uses `workflow-templates/`; the hyperpolymath half is
`rsr-template-repo` (copy-on-create), which is what happens today, now ratified.
Verify with one planted template whether a User-account `.github` surfaces it
before building the hyperpolymath half (§12 O8).

## 7. Universal settings baseline

### 7.1 Three profiles (one JSON cannot serve all)

| Profile | Mechanism | Why |
|---|---|---|
| **P-pub**: hyperpolymath public | repo ruleset | rulesets work on public Free |
| **P-priv**: hyperpolymath private | classic branch protection (+ ruleset if a planted test shows it applies — a GET returned `[]` not 403 on 09-02, contrary to April) | Free-private may 403 on rulesets |
| **P-org**: metadatastician | per-repo ruleset now; **one org ruleset** the day Team lands (O1) | org rulesets 403 on Free (live 09-02) |

### 7.2 Repo settings (all profiles) — `PATCH /repos/{o}/{r}`

`allow_squash_merge=true`, `allow_merge_commit=false`, `allow_rebase_merge=false`
(rebase cannot be signed; merge breaks linear history — memory), `delete_branch_on_merge=true`,
`allow_auto_merge=true`, `allow_update_branch=true`, `web_commit_signoff_required=true`,
`squash_merge_commit_title=PR_TITLE`, `squash_merge_commit_message=PR_BODY`,
`security_and_analysis: secret_scanning + push_protection enabled` (public),
Dependabot alerts + security updates enabled. Actions: `allowed_actions=selected` with
the estate allowlist, default `GITHUB_TOKEN` = read, `can_approve_pull_request_reviews=false`.
Org-level (metadatastician): keep `sha_pinning_required=true`.
Per-repo deltas allowed: `has_wiki`, `has_projects`, `has_discussions`, Pages source,
topics, description. Nothing else.

### 7.3 Ruleset `Base` (canonical JSON, one file, applied everywhere)

Target `~DEFAULT_BRANCH`; rules: `pull_request` (0 approvals, `allowed_merge_methods:
["squash"]`), `required_signatures`, `required_linear_history`, `deletion`,
`non_fast_forward`, `required_status_checks` (contexts derived per repo type, §6.2).
**Removed**: `code_coverage` 95, `required_deployments` github-pages, Copilot review.
Bypass actors (mode `pull_request` unless noted): RepositoryRole admin (5); apps
claude 1236702, dependabot 29110, github-actions 15368, coderabbitai 347564,
gitguardian 46505, copilot-swe-agent 1143301, chatgpt-codex-connector 1144995,
sonarqubecloud 12526, oikosbot 2538504. **Removed**: codacy 56611, gitar-bot 827041,
codecov 254, renovate 2740, github-advanced-security 57789 (needs no bypass),
unresolved 1561 / 85455 / 946600 (`/user/installations` returns 403 to a `gh` OAuth
token, so ID→slug is owner-UI only: Settings → Applications; §12 O5).
Tag ruleset: keep `tag-protection.json` (8 byte-identical copies already).
Classic fallback for P-priv: same rules via `branches/{b}/protection`.

**Consequence the owner is approving.** Bypass applies to the *whole* ruleset,
`required_status_checks` included: every actor above can merge around every GATE.
With nine apps listed, "GATE" binds humans and unlisted bots only. If the AI
reviewers (coderabbitai, copilot-swe-agent, codex-connector, sonarqubecloud) should
be held to the gates, the design is a **second ruleset carrying only
`required_status_checks`** with bypass = claude, dependabot, github-actions,
oikosbot. That is O6 in §12; the plan ships the single-ruleset form unless ruled.

**metadatastician app installs (live 2026-09-02, 11 installed)**: uninstall
codacy-production, gitar-bot, renovate, semgrep-app (R1/R4/R5, §5.2 semgrep DROP);
keep claude, sonarqubecloud; **OWNER?** slack, microsoft-teams-for-github,
thanks-dev, linear-data-importer, linear-code (§12 O9).

### 7.4 One applier, one verifier

- Applier: `hyper-repos/git-scripts/scripts/branch-protection-apply.sh` (idempotent,
  GET-merge preserves contexts, admin bypass). Extend with: repo-settings PATCH,
  autolinks, classic fallback, profile switch. Retire the other seven appliers
  (`git-scripts/scripts/fix/branch-protection.sh`, `personal-sysadmin/.../configure-all-repos.sh`,
  `setup-new-repo.sh`, `conative-gating/scripts/github-settings.sh`,
  `echidnabot/.../apply-branch-protection.sh`, `launch-scaffolder/.../wave{1,2}-apply.sh`,
  `.git-private-farm/scripts/migrate-classic-to-rulesets.sh`).
- Verifier: `.git-private-farm/scripts/ci-deadlock-audit.sh` (phantom contexts) +
  new `settings-drift.sh` diffing live ruleset/settings/autolinks against canonical.
  Weekly, output into `scripts/estate-board.sh` snapshots.
- Committed ruleset JSON directories (`scripts/ruleset-backups/`,
  `dev-notes/.../ruleset-backups-2026-08-04/`, `meta-repos/*/.github/rulesets/`) become
  **archive only**; canonical lives in `standards/rulesets/base.json`.

### 7.5 Community health / `.github` repos

- Resolve the two diverged `.github` checkouts (`hyper-repos/.github` e77a196 vs the
  rogue `repos/.github` 6c94ba3 — AGENTS.md forbids `repos/`; keep hyper-repos, port
  the 08-23 delta). Fix `FUNDING.yml` casing, replace the stub `dependabot.yml`
  (`package-ecosystem: ""`), make `.github` dogfood the thin callers, add
  `workflow-templates/`.
- Create a real `metadatastician/.github` (none exists locally; de facto =
  `meta-repos/metadatastician-governance/.github/`).
- Dependabot canonical template (md5 `571343d3`, 69 repos) → regenerate with valid
  ecosystems only (no `nix`/`guix`), `julia` where `Project.toml`, `github-actions`
  everywhere, `groups` to replace Renovate grouping.

### 7.6 Autolinks (R6)

Live templates on `standards` (2026-09-02): GHSA- → github.com/advisories, CVE- → NVD,
RUSTSEC- → rustsec.org, RFC- → rfc-editor, **ADR- → `hyperpolymath/standards/docs/decisions/ADR-<num>.adoc`**
(estate-central, not per-repo), **PROV- → `hyperpolymath/proven/issues/<num>`** (proven
issue tracker, not a proof index).
Baseline set on every repo: GHSA-, CVE-, RFC-, ADR- (kept estate-central as today).
Per-language additions: RUSTSEC- (Rust), OSV-, PROV- (repos that depend on proven),
JLSEC/GHSA for Julia, HEX- (Elixir advisories), ZIG- if an advisory DB exists.
Audit step: `gh api repos/{o}/{r}/autolinks` for all 435 → table → owner picks
the per-type set → applier writes it. Alphanumeric keys allowed (`is_alphanumeric`).

## 8. hyperpolymath vs metadatastician (R7)

**Rule.** hyperpolymath = proofs, mathematics, languages/type theory, and every
estate-wide tool or standard (anything other repos `uses:` or depend on).
metadatastician = games, gimmicks, applications, anything with an end-user surface.

First cut (unambiguous only; everything else deferred per owner):
- → metadatastician: airborne-submarine-squadron and all games; befunge-cracker and the
  gimmick family; stealth-glider series (already minted there); application repos.
- → hyperpolymath (stay): standards, echidna, reposystem, panic-attack, pons-asinus,
  hypatia, gitbot-fleet, .git-private-farm, cicd-suite, rsr-template-repo,
  proven, affinescript, a2ml/k9 ecosystems, all proof repos.

Transfer discipline per move: Actions ignores renames (memory) → re-run applier post-
transfer; author email must be the noreply form (GH007); update local remotes without
the worktree-rewrite trap. Note for owner: the April transfer plan's rationale was
billing (minutes/storage/GHAS), which pulls the *other* way from subject matter; O1
resolves the tension if Team lands on metadatastician.

## 9. Never re-add

CodeFactor · Snyk · Mergify · ImgBot · Codacy (R1) · Renovate (R5) · Codecov ·
gitar-bot as bypass actor (R4) · Deno anything · `instant-sync` · `dawidd6/action-send-mail` (smtp-notify-action replaces it; owner ruling 2026-09-02) ·
`dependabot-automerge.yml` (native replaces) · `required_deployments` / coverage-95 /
Copilot-review ruleset rules · hand-typed status-check contexts.

## 10. Execution order (each step = its own PR train; nothing starts before spec commit)

0. Commit this spec to `standards/docs/superpowers/specs/2026-09-02-cicd-regularisation-design.md`; write the R1–R7 rulings to memory; file the owner-action issue (O1–O3).
1. **Canonical artefacts in `standards`**: `rulesets/base.json`, `rulesets/base-classic.json`, `settings/repo.json`, `autolinks/{base,rust,proof,julia,elixir}.json`, tier doc update (`docs/CICD-SIGNAL-DISCIPLINE.adoc`), MUST list.
2. **Reusables**: remake `governance-reusable.yml` as N jobs (fold §5.2 policy family + workflow-linter + dogfood + SPDX); add `actions-lock-verify` + Dependabot lock-sync job; delete `deno-ci-reusable.yml`; create `julia-ci`, `bun-ci`, `ada-ci`, `zig-ci`, `pages`, `release` reusables. Each new job ships with a planted-positive test in `standards` CI.
3. **Applier/verifier**: extend `branch-protection-apply.sh`; write `settings-drift.sh`; retire the seven other appliers (delete, with a pointer).
4. **Pilot on 6 repos** (2 P-pub, 1 P-priv, 2 P-org, 1 proof repo): apply, run verifier, plant a positive per GATE, record the first real red.
5. **Sweep** (bot-driven, alphabetical, resumable, with a stall detector — the 08-28 sweep died silently mid-alphabet): delete DROP files, replace inline with thin callers, fix 3 broken refs, pin `@main` refs, regen lockfiles, apply settings/ruleset/autolinks, remove app installs. Track in one issue with per-repo checkboxes.
6. **`.github` repos** (§7.5) + `workflow-templates/`.
7. **Owner-gated moves** (§8) after O1.

## 11. Verification

- **Applier idempotence**: run twice on a pilot repo; second run = zero API writes.
- **Live ≡ canonical**: `settings-drift.sh` reports 0 diffs on pilots, then estate-wide; diffs are the weekly number alongside `scripts/estate-board.sh`.
- **Every required context is emitted**: for each repo, contexts ⊆ job ids of the latest run (`ci-deadlock-audit.sh` extended); phantom count must be 0.
- **Every GATE can fail**: one PR per gate type that violates it goes red; recorded in `standards/docs/gate-proofs.adoc` with run URLs. Never `2>/dev/null` the thing under test.
- **Lockfile**: `gh actions-lock --no-fix` green on pilots; one `lock-refresh` PR from `standards` lands green on a pilot with a signed commit and a regenerated lock; a hand-corrupted lock on a pilot PR goes red (planted positive for §6.4). Dependabot no longer opens `github-actions` PRs on pilots.
- **Auto-merge arming**: a Dependabot dependency PR on a pilot is armed by the §5.5 job and merges without a human once gates are green.
- **startup_failure = 0** on pilots via the banner reader (`curl` the run page — memory), not `?status=failure`.
- **Merges without `--admin`**: pilot PRs merge through the ruleset with no bypass; `mergeStateStatus ∈ {CLEAN, UNSTABLE}` captured *before* the merge (UNSTABLE = every required context green while a non-required one is red; proven on double-track-browser #98, 2026-09-02), and the resulting push's entry in `GET repos/{o}/{r}/rulesets/rule-suites?ref=main` carries `result: pass`, not `bypass`. The rule-suite result is the discriminator; the merge exit code is not.
- **Census re-run** after the sweep: workflow files per repo ≤ 8 (+ opt-ins); governance latest-green ≥ 90%; casket/pages/release no longer in the bottom-5.
- Horizon statement attached to every count (AGENTS.md §5).

## 12. Open decisions for the owner (batched, non-blocking)

O1 Team via Education/Nonprofits · O2 Settings App installed? · O3 BoJ trigger, mirror
targets, rhodibot, ClusterFuzzLite scope · O4 which repos beyond the first cut move ·
O5 unresolved bypass app IDs 1561 / 85455 / 946600: API is 403 for this token, so look
them up in Settings → Applications; remove if unrecognised ·
O6 **gates vs bypass**: single ruleset (nine apps bypass every GATE) or a second
checks-only ruleset with bypass = claude/dependabot/github-actions/oikosbot (§7.3) ·
O7 **Julia private registry**: if Dependabot cannot read `julia-professional-registry`,
Renovate stays on those repos (R5 caveat) ·
O8 **squash-only, knowingly**: on 08-31 the owner replaced a ruleset on two repos to
allow merge commits. §7.2/§7.3 lock squash because it is the only method that
survives `required_signatures` + `required_linear_history` (rebase cannot be signed;
merge commits break linearity, memory). Keeping merge commits means dropping
`required_linear_history` estate-wide; say which ·
O9 metadatastician installs with no ruling: slack, microsoft-teams-for-github,
thanks-dev, linear-data-importer, linear-code (keep or uninstall) ·
O10 `workflow-templates/` on a User-account `.github` (plant one, check the "New
workflow" page); if invisible, hyperpolymath distribution = rsr-template-repo only.

## 13. Amendments from step 1 (2026-09-02, same day)

Facts found while writing the canonical artefacts; each overrides the section it names.

| § | Was | Now | Evidence |
|---|---|---|---|
| 10 step 1 paths | `standards/rulesets/`, `settings/`, `autolinks/` at repo root | **`config/rulesets/`, `config/settings/`, `config/autolinks/`** + `config/README.adoc` | `config/` already holds the estate gitleaks baseline; no new root directories; Mustfile root rules constrain only loose `.contractile` files and `REGISTRY.a2ml` |
| 7.1 P-priv | classic branch protection fallback, `base-classic.json` | **Dropped.** Rulesets work on private Free repos | Planted POST+DELETE on `dev-notes-vault` (private), 2026-09-02 |
| 7.3 name | ruleset `Base` | Identity = active branch ruleset targeting exactly `["~DEFAULT_BRANCH"]`; name irrelevant. Live name everywhere sampled is `Optimus-Branch`; no `Base` exists | `gh api repos/{o}/{r}/rulesets` on standards, hypatia, verisimdb |
| 7.3 rules | `required_linear_history`; `update` unmentioned | **No `required_linear_history`** (live has none; O8 decides squash-only vs merge commits). **`update` dropped** (live has it; it makes main writable by bypass actors only) | live ruleset 14285635 |
| 7.3 bypass modes | unspecified | all Integration + admin = `pull_request`; RepositoryRole 2 (maintain) dropped; `always` nowhere on the branch ruleset | `config/README.adoc` |
| 7.3 strict | unspecified | `strict_required_status_checks_policy: false`, decided | PR #714 sat BEHIND |
| 7.3 tags | "keep `tag-protection.json`" | `config/rulesets/immutable-tags.json`: drop `required_status_checks`, `required_deployments`, `required_linear_history` (unsatisfiable at tag creation → no workflow could create tags); bypass = admin + OikosBot `always` | live ruleset 18110117 |
| 6.4 credential | "the single App credential it already holds" | **standards holds no App credential.** No `APP_ID` variable, no `APP_PRIVATE_KEY` secret; `signed-push-smoke.yml` red on every run since 2026-08-24. `lock-refresh` and App-created tags are blocked on **O11** | `gh secret list`, `gh variable list`, run 32717664038 |
| 7.6 ADR | ADR- "estate-central → standards/docs/decisions" | **ADR- is repo-local** on all 336 repos with autolinks; 10 point at a renamed repo (rename residue). Templated `{{OWNER}}/{{REPO}}` | full audit `$CLAUDE_JOB_DIR/tmp/autolinks-all.tsv`, 428 repos, 0 errors |
| 7.6 audit | 28/40 sampled | 335 repos identical six-prefix set, 1 minus RFC (`cloudguard-cli`), **92 none** (57 hyperpolymath, 35 metadatastician = almost the whole org) | same |
| 7.6 profiles | JLSEC/HEX prefixes proposed | `julia.json` and `elixir.json` ship **empty** with a stated reason: no verified advisory prefix with a stable URL; GHSA-/OSV- in base cover both. `OSV-` added to base | rustsec/osv URL shapes verified; nothing invented |
| 7.2 allowlist | prune list | 118 → 92 patterns; `hyperpolymath/*` subsumes 20 explicit entries; Python-adjacent trio and `ad-m/github-push-action` listed as review candidates, not pruned | `config/settings/actions-allowlist.json` |
| 12 | O1–O10 | **O11 added**: create or pick the estate GitHub App (OikosBot 2538504 is the owner's own), plant `APP_ID` (variable) + `APP_PRIVATE_KEY` (secret) on `hyperpolymath/standards` | — |
| 7.6 ADR template | `docs/decisions/ADR-<num>.adoc` | ADR files are `ADR-<num>-<slug>.adoc`; the live template 404s on every repo. Canon = code-search URL `search?q=ADR-<num>+path%3Adocs%2Fdecisions&type=code` | `ADR-003.adoc` on standards = HTTP 404, 2026-09-02 |
| 7.2 allowlist enforcement | "prune" implied enforcement | `verified_allowed` is true, so verified creators bypass the list; prune is hygiene, R1 is enforced by deleting workflows. **O12 added**: flip `verified_allowed` to false after a `uses:` census | `config/settings/actions-allowlist.json` |
| 7.3 direct push | unstated | With every bypass at `pull_request` and no `always` actor, main is PR-only for everyone including the owner; emergency path = disable the ruleset. FYI posted on #715 | `config/rulesets/base.json` |

## 14. Fold table — estate policy files → governance-reusable jobs (step 2f)

Doc only; step 5 deletes the left column once the right column is green on the pilots.
Job names are the frozen contexts (§6.2; guard = `tests/test_governance_reusable_shape.sh`).

| Estate file (repos) | Governance job (context) | Fold decision |
|---|---|---|
| `security-policy.yml` (61) | `Security policy checks` | Fold. Weak crypto / HTTP URL findings are ADVISORY `::warning`; hardcoded secrets FAIL. Delete file |
| `runtime-policy.yml` (47), `language-policy.yml` (12), `ts-blocker.yml` (7) | `Language / package anti-pattern policy` | Fold. Delete files |
| `npm-bun-blocker.yml` (7) | — | DROP: contradicts the Bun ruling; no job carries it |
| `rsr-antipattern.yml` (24) | `Language / package anti-pattern policy` | DROP the file (its reusable does not exist); the job already covers the anti-pattern list |
| `wellknown-enforcement.yml` (60) | `Well-Known (RFC 9116 + RSR)` | Fold. Open: is `security.txt` MUST or SHOULD → owner ruling **O13** (#715). Until ruled the job keeps today's severity |
| `estate-rules.yml` (40) | `Trusted-base reduction policy` + `Licence consistency` | Fold; the two jobs are the surviving halves. Delete file |
| `guix-policy.yml` (25), `guix-nix-policy.yml` (35) | `Guix packaging policy (Nix retired)` | Fold. Presence-only checks in the files were fake; the job's checks must keep a planted positive |
| `container-policy.yml` (7) | `Security policy checks` | Fold the digest-pin rule into the job as a new sub-check with a planted positive; delete file |
| `workflow-linter.yml` (141) | `Workflow security linter` + `Actions lockfile verify` | Fold. Inline `uses:` are linted too (the file missed them). Lock verification is its own context (PR 2a) |
| `dogfood-gate.yml` (240, 164 variants): invisible-character job | new governance job (name TBD in PR 2b, e.g. `Invisible characters`) | REMAKE as a GATE. Reference implementation = double-track-browser #90: `grep -aP '(*UTF)[…]'`, in-step probe that the pattern fires on a planted NBSP, `::error` + exit 1 on findings |
| `dogfood-gate.yml`: groove / A2ML / K9 checks | — | Keep as the `*-ecosystem/validate-action` opt-ins (§5.6); not governance |
| `cicd-suite/spdx-license-check`, `palimpsest-license` action | `Licence consistency` | Fold. SPDX line-1 rule lives in one place |
| `Validate Hypatia Baseline` (governance) vs `hypatia-scan` threshold | `Validate Hypatia Baseline` + `scan / Hypatia Neurosymbolic Analysis` | Align: governance validates baseline file shape only (info); the severity threshold (high) is owned by `hypatia-scan-reusable`. One owner per rule, no double jeopardy |
| `Live Actions policy (credentialed advisory)`, `Code quality + docs`, `Allowlist Preflight` | (advisory) | Never required contexts (`config/rulesets/gates.json`). They warn, they do not gate |
