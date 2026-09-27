# RULESET B-PROTOCOL — merging PR #1051 past the `main gate` (2026-09-27)

**Status:** the merge was instructed twice by the owner and refused by both routes
(normal and admin): ruleset `23787415` ("main gate: append-only + required checks +
signatures + scanning") is active with `bypass_actors: []`, and **19 of 19 workflow
runs on the PR head are `startup_failure`** (D39's `allowed_actions=selected` with
empty patterns + the codeql-action v4.38.1 startup-killer, #1037), so the 22 required
contexts never produce check-runs. No actor — owner included — can merge until the
ruleset is relaxed or the startup deaths are cured.

This file makes Option B (relax → merge → byte-exact restore) turnkey for any session
holding `administration:write` (the owner's Claude Code sessions have done ruleset PUTs
before — see the D84 re-scope). The Arena session that authored PR #1051 cannot: its
token gets 404 on the ruleset write route (probed 2026-09-27, no-op PATCH; backup
verified byte-identical after the probe).

**Auto-merge is ARMED on PR #1051** (`--auto --squash`). If the startup deaths are
cured first (Option A), the PR merges itself with no ruleset edit at all.

---

## Path 1 — session with administration:write

```bash
# 0. Fresh backup (never trust a file over the live state)
gh api repos/hyperpolymath/standards/rulesets/23787415 > /tmp/fresh-backup.json

# 1. Confirm no drift since the committed backup
diff <(jq -S . /tmp/fresh-backup.json) \
     <(jq -S . arena-session-787/ruleset-23787415-backup-2026-09-27.json)

# 2. Relax: drop required_status_checks + code_scanning, keep the rest
jq '.rules |= map(select(.type != "required_status_checks" and .type != "code_scanning"))' \
  /tmp/fresh-backup.json > /tmp/relaxed.json
gh api -X PUT repos/hyperpolymath/standards/rulesets/23787415 --input /tmp/relaxed.json

# 3. Verify the PUT actually applied (a ruleset PUT has returned 200-with-empty-body
#    and not applied before — always re-GET)
gh api repos/hyperpolymath/standards/rulesets/23787415 --jq '[.rules[].type]'
#    expect: ["deletion","non_fast_forward","required_signatures"]

# 4. Merge (squash, per D19a)
gh pr merge 1051 --repo hyperpolymath/standards --squash

# 4b. ONLY if refused on required_signatures (unsigned squash commit):
#     also drop that rule (jq select != "required_signatures"), re-PUT, retry merge.

# 5. Restore — byte-exact, immediately
gh api -X PUT repos/hyperpolymath/standards/rulesets/23787415 --input /tmp/fresh-backup.json

# 6. Verify restoration
diff <(gh api repos/hyperpolymath/standards/rulesets/23787415 | jq -S .) \
     <(jq -S . /tmp/fresh-backup.json) && echo RESTORED

# 7. Record the timeline (relax → merge → restore, with clock times) on the PR and #787.
```

## Path 2 — owner, browser only

1. `standards` → **Settings → Rules → Rulesets** → `main gate: append-only + required
   checks + signatures + scanning`.
2. Toggle **Enforcement: Active → Disabled** (or Edit → remove the two rules above).
3. Merge [PR #1051](https://github.com/hyperpolymath/standards/pull/1051) — **Squash and merge**.
4. Toggle enforcement back / restore the rules.
5. Verify the ruleset reads 5 rules again.

## The real cure (no ruleset edit needed)

Relaxing the ruleset is the expedient. The durable fix is curing the startup deaths:
apply the **D39 canon `allowed_actions` payload** (88 patterns, on `origin/main` at
`d1bd7f42`) and the **#1037 codeql-action pin fix**, at which point the 19 workflows
run, the 22 contexts report, and auto-merge fires on its own. Note: re-scoping the
required contexts alone (the #1040 fix) is NOT sufficient — startup-dead workflows
produce no check-runs to require.

## Doctrine note

The estate records "disable-to-merge" as a defect shape (375 rulesets were disabled on
2026-09-22; D94–D96 added zero-bypass floors deliberately). This protocol is a one-off
under explicit, repeated owner instruction, with a pre-verified byte-exact restore and
mandatory post-restore verification — document it where the merge is recorded.
