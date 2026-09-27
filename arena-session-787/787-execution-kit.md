# #787 execution kit — 2026-09-26 owner rulings

> Re-verified 2026-09-27: #787 body unchanged since 2026-09-23T17:28:50Z; all five
> Find-blocks byte-match the live body; `standards` main still at `2479cf7`.
> Stored on session branch `arena/01a0dd13-standards` because workspace-root files do not
> persist between turns in this environment.

Three steps, in order. Step 1 uses the comment draft beside this file; steps 2–3 are below.
(Agent sessions with write access may execute these on the owner's behalf — the rulings
were given by the owner via selection UI on 2026-09-26.)

---

## STEP 1 — post the ruling comment on #787

Paste the full contents of **`owner-rulings-comment-2026-09-26.md`** (beside this file) as a
comment on https://github.com/hyperpolymath/standards/issues/787 — then copy the comment's
URL (`#issuecomment-…`) for use in STEP 2.

---

## STEP 2 — strike the five rows in the #787 issue body

Edit the issue body and replace each row below with its struck counterpart
(F4a convention: strike, never delete; answer beside the row).
Replace `COMMENT-URL` in all five with the STEP 1 comment URL.

Find:

**D12. O1** GitHub Team for `metadatastician` — is it listed at education.github.com? Until then org rulesets 403 and it stays on per-repo rulesets.

Replace with:

~~**D12. O1** GitHub Team for `metadatastician` — is it listed at education.github.com? Until then org rulesets 403 and it stays on per-repo rulesets.~~

→ RULED 2026-09-26: **D12 — NOT listed → apply for GitHub for Nonprofits (Team free).** Per-repo rulesets until Team is active; the ratified Branch-Floor / Tag-Floor org rulesets (D94–D96) execute the moment it lands. ([ruling comment](COMMENT-URL))

Find:

**D14. O3** Dispositions: `boj-build.yml` (255 repos — drop if BoJ is retired) · `mirror.yml` (142 repos skip for missing forge secrets — keep on which?) · `rhodibot.yml` (93 repos, 78% red — retire or remake?) · ClusterFuzzLite `cflite_*` (keep on which Rust/Zig repos?).

Replace with:

~~**D14. O3** Dispositions: `boj-build.yml` (255 repos — drop if BoJ is retired) · `mirror.yml` (142 repos skip for missing forge secrets — keep on which?) · `rhodibot.yml` (93 repos, 78% red — retire or remake?) · ClusterFuzzLite `cflite_*` (keep on which Rust/Zig repos?).~~

→ RULED 2026-09-26: **D14 — all four families stay; repair, not prune.** **BoJ is ALIVE** (*"in boj-server and boj-server-cartridges in hyperpolymath"*), so `boj-build.yml` stays; the KYAML migration (step 1 of 6 done, #1020 closed; steps 2–6 open) is **piloted on `standards` first** and **boj-server / boj-server-cartridges take updates as part of that critical path**. `mirror.yml`: provision the missing forge secrets (secret inventory first). `rhodibot.yml`: remake — 78% red is the bot's defect, not a retirement signal. `cflite_*`: keep on the Rust/Zig repos. ([ruling comment](COMMENT-URL))

Find:

**D24. #306** Pages across **48** repos — enable, or delete the Pages workflow? Pages enablement is API/UI-only, so there is no implementable middle path.

Replace with:

~~**D24. #306** Pages across **48** repos — enable, or delete the Pages workflow? Pages enablement is API/UI-only, so there is no implementable middle path.~~

→ RULED 2026-09-26: **D24 — class-based disposition** (census corrected 2026-08-26: **40 repos, not 48**; 345 carry a Pages workflow; metadatastician has 0 defects). Genuine site repos (`pages.yml` / casket-ssg docs class) → enable Pages (`POST /repos/{o}/{r}/pages -f build_type=workflow`); `casket-pages.yml` on repos with nothing to publish → remove the workflow; `jekyll*` → always remove (Jekyll banned estate-wide); libraries/internal tooling → remove the workflow. Re-verify per the "an issue body is a dated record" rule before each write. ([ruling comment](COMMENT-URL))

Find:

**D29. #245** Which plugin hosts to bind **at all** — WordPress / WebExtensions / Thunderbird MailExt / React-Next — given WP-PHP cannot load WASM? Four questions posted 2026-05-28, none answered. Blocks #246 and #280.

Replace with:

~~**D29. #245** Which plugin hosts to bind **at all** — WordPress / WebExtensions / Thunderbird MailExt / React-Next — given WP-PHP cannot load WASM? Four questions posted 2026-05-28, none answered. Blocks #246 and #280.~~

→ RULED 2026-09-26: **D29 — REOPENED.** #245 had been closed not_planned 2026-08-26 ("no TypeScript left to port"); the owner rules the plugin-host binding work back on. The old #245 / #246 / #280 stay closed as records of their own scope; a fresh issue carries the reopened work (filed with current evidence — note wordpress-tools is now 138 `.php` + Rust + PowerShell, so the WordPress question is a PHP/WASM question, not a TS-porting one). ([ruling comment](COMMENT-URL))

Find:

**D43. When is the A2ML / `.deed` agent clear?** #19 (161 repos left, resumable at line 153 `lcb-website`) and #35 are held on this and nothing else.

Replace with:

~~**D43. When is the A2ML / `.deed` agent clear?** #19 (161 repos left, resumable at line 153 `lcb-website`) and #35 are held on this and nothing else.~~

→ RULED 2026-09-26: **D43 — the hold clears when #837 steps 1–3 have landed** (normative ABNF · per-family mapping specs · canonical translator + conformance lane). Until then the held sweeps stay parked; when they resume they resume **as conversions, never as in-place `.a2ml` edits**. Baseline at ruling: `launcher/launcher-standard_praxis.deed` landed (D73-C), 222 `.a2ml` remain in `standards`, no general translator yet, no Rust `.deed` reader anywhere (deed-ecosystem#67). ([ruling comment](COMMENT-URL))

---

## STEP 3 — file the fresh D29 issue (standards)

Title:

Plugin-host bindings, second opening: WordPress (PHP/WASM) · WebExtensions · Thunderbird MailExt · React-Next

Body:

Reopened by the D29 ruling on #787 (2026-09-26). The first opening (#245, closed not_planned
2026-08-26; children #246 / #280 closed completed) was closed as moot on the finding that all
five plugin repos then held **zero TypeScript files** — there was nothing left to port. The
owner has ruled the underlying question — which plugin hosts to bind **at all** — back open.

## Current state of the candidate hosts (re-measured 2026-08-26)

| Host | Estate repo(s) | What is actually there now | The real question |
|---|---|---|---|
| WordPress | wordpress-tools | **138 `.php`**, Rust, PowerShell — no TS | PHP plugin entry points cannot load WASM; can AffineScript reach the Gutenberg-blocks JS surface at all, or is this a PHP-only zone? |
| WebExtensions | universal-chat-extractor, double-track-browser | `.idr`, `.affine`, `.rs` | `browser.tabs.*` / `browser.runtime.*` surface (~200+ fns) — JS-loaded, workable in principle |
| Thunderbird MailExt | thunderbird-template-reloaded | `.idr`, `.affine` | MailExtensions API surface |
| React-Next | polyglot-i18n | 79 `.js`, 31 `.affine` | React component model, Next.js Pages Router |

## Acceptance criteria (per the original #245 shape)

- Each of the four hosts gets either **a bindings PR in the affinescript repo** or an explicit
  **"won't bind" rationale** recorded here.
- WordPress's PHP/WASM ceiling is decided first — it gates whether wordpress-tools has any
  AffineScript path or stays PHP/Rust.
- This issue closes when every plugin-blocked repo has at least one recorded path forward.

## Relations

- Supersedes the *scope* of #245 / #246 / #280 (all stay closed as their own records).
- Parent decision surface: #787 (D29 ruling comment).
- Book as a new D-row at `max(ledger, issue)` at write time; never renumber.

---

## Owner-only browser steps (cannot be delegated to any agent session)

### D12 — GitHub for Nonprofits application (after STEP 1–3, or in parallel)

1. Sign in at **education.github.com** with the account that owns `metadatastician`.
2. Teachers/organisations path → **GitHub for Nonprofits** → start the application.
3. Applicant type: the organisation (`metadatastician`), not an individual.
4. Have ready: the org's mission description, a request letter on org letterhead (GitHub's
   application form names what it accepts), and evidence of nonprofit status if held.
5. On approval (typically days–weeks), Team activates → org rulesets stop 403ing →
   **execute Branch-Floor / Tag-Floor (D94–D96) per the ratified staging**: create scoped to
   one repo → verify by `GET /repos/{o}/{r}/rules/branches/{default}` → widen to `~ALL` by
   full-body PUT → re-verify → `apply-protection-floor.sh` report-only must return
   **0 WOULD-CREATE** across all 68 repos.
6. Until then: per-repo floors remain the mechanism (PR #1034's applier is merged and
   rsr-template-repo already carries a live `Branch-Floor` ruleset — the per-repo arm is
   executing).

### D16 — the bypass-actor app IDs (one glance, then the ruling executes)

1. GitHub → **Settings → Applications** (Installed GitHub Apps / Authorized OAuth Apps).
2. Find the names behind IDs **1561**, **85455**, **946600** (the API will not resolve them;
   only the UI shows them).
3. Recognised → keep and record the name. Unrecognised → remove; the D16 ruling is already
   made ("remove if unrecognised"), so removal can then proceed estate-wide.
   (Status check 2026-09-27: the `standards` and `rsr-template-repo` rulesets show empty
   bypass lists, but the three IDs lived on other rulesets — no signal either way; the glance
   is still owed.)

---

## State of the surrounding work, measured 2026-09-27

- **#787**: no comments or body edits since the 2026-09-23T17:28Z batch. Nobody has pasted
  this ruling, struck the rows, or filed the D29 issue yet.
- **`standards` main**: still `2479cf7` (2026-09-24) — three quiet days; this kit was
  verified against that state.
- **KYAML** (couples to the D14/BoJ ruling): step 1/6 (#1020) CLOSED; steps 2–6 OPEN, and
  **step 4 (#1023) is itself a pending owner ruling** — deciding KYAML scope is now the
  next owner decision queued behind this one.
- **#837 (.deed conversion, gates D43)**: no activity since 2026-09-19; steps 1–3 not
  started. The hold stands as ruled.
- **#306 (Pages, gates D24)**: unchanged since the 2026-08-26 census — the 40-repo list is
  still the operative one.
- **New since the sheet was last worked**: #1049 (2026-09-26 — 35 repos below the RSR
  7-topic minimum) and #1050 (2026-09-27 — hypatia-scan-reusable at `2479cf7` produces no
  `hypatia.sarif`; caller-visible failure at current main). Neither touches the five rows.
- **Succession rule**: after the 09-23 mass strike, open rows sit well below the ~30
  re-compilation threshold — answering these five will not trigger re-compilation.
