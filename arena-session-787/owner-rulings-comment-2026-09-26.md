## Rulings — the five owner-only rows, 2026-09-26

All five rows held back from the 2026-09-23 batch, answered together. Rows ruled here can be struck; owner's words quoted verbatim where given.

**~~D12~~ — metadatastician is NOT listed at education.github.com → apply for GitHub for Nonprofits (Team free).**
Owner checked the dashboard ("Upgrade your academic organizations"): not listed. The nonprofits application is owner-and-browser-only — no agent can file it. Until Team is active, org rulesets stay 403, so the ratified **Branch-Floor / Tag-Floor org rulesets (D94–D96) execute the moment Team lands**, and until then the per-repo floor remains the mechanism for the 68 metadatastician repos (report-only baseline stands: 67 WOULD-CREATE + 1 ARCHIVED).

**~~D14~~ — all four families stay; repair, not prune.**
- **BoJ: ALIVE.** Owner, verbatim: *"boj is alive, in boj-server and boj-server-cartridges in hyperpolymath, migration to kyaml as per standards repo to be piloted on the standards repo first and will need updates to boj-server / cartridges for this to happen effectively."* → `boj-build.yml` stays (consistent with D11's boj-server badge-pilot ruling). New dependency recorded: the KYAML migration (standards #1021–#1025) is **piloted on `standards` first**, and boj-server / boj-server-cartridges will need updates for the KYAML route to work — the cartridges side is now coupled to the KYAML step sequence, so those updates belong on the KYAML critical path, not after it.
- **`mirror.yml`: keep — repair.** Provision the missing forge secrets on the 142 skipping repos (secret inventory first, then per-forge enablement).
- **`rhodibot.yml`: keep — remake.** 78% red is a defect of the bot, not a signal to retire the function.
- **ClusterFuzzLite `cflite_*`: keep broadly** on the Rust/Zig repos.

**~~D24~~ — class-based disposition, on the re-verified census (40 repos, not 48).**
- Genuine site repos (the `pages.yml` / casket-ssg docs class) → **enable Pages** (`gh api -X POST /repos/{o}/{r}/pages -f build_type=workflow`, one call per repo).
- `casket-pages.yml` on repos with nothing to publish → **remove the workflow**.
- `jekyll*` → **always remove** — Jekyll is banned estate-wide; no per-repo consideration.
- Libraries / internal tooling → **remove the workflow**.
- metadatastician needs nothing (0 defects there). The 2026-08-26 census comment on #306 is the enumerated list; re-verify before each write per the standing "an issue body is a dated record" rule.

**~~D29~~ — REOPENED.**
#245 was closed not_planned 2026-08-26 on the re-verification "no TypeScript left to port"; the owner rules plugin-host binding work back **on**. The old #245 / #246 / #280 stay closed as records of their own scope. A fresh issue is to be filed with current evidence (host list unchanged: WordPress / WebExtensions / Thunderbird MailExt / React-Next — and note wordpress-tools is now 138 `.php` + Rust + PowerShell, so the WordPress question is a PHP/WASM question, not a TS-porting one). Its scope books as a new row at `max(ledger, issue)` at write time.

**~~D43~~ — the A2ML/`.deed` agent hold clears when #837 steps 1–3 have landed.**
That is: (1) one normative ABNF, (2) per-family mapping specs, (3) the canonical translator + conformance lane. Until then: no new `.a2ml` writes, and the held sweeps (#19, 161 repos left, resumable at line 153 `lcb-website`; #35) stay parked. When they resume, they resume **as conversions, never as in-place `.a2ml` edits**. Progress marker at ruling time: `launcher/launcher-standard_praxis.deed` has landed (D73-C); 222 `.a2ml` remain in `standards`; no general translator exists yet and no Rust `.deed` reader exists anywhere (deed-ecosystem#67).

**Still owed from the owner (unchanged):** the D16 glance — Settings → Applications, the names behind app IDs **1561, 85455, 946600** (ruled "remove if unrecognised"; the API will not resolve them, only the UI shows them).

**Not owner-blocked:** D97's scope & cost study is agent work; the D29 fresh-issue filing likewise.
