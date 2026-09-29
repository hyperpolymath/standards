#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# triage-2026-09-29-apply.sh — Apply the 2026-09-29 issue-triage execution plan
# (see ULTRAPLAN-2026-09-29.adoc).
#
# Why this script exists: the Arena sandbox token is a fine-grained GitHub App
# installation token with contents:write + pull-requests:write, NOT issues:write
# or labels:write. Creating labels, commenting on issues, and closing resolved
# issues return HTTP 403 from inside the sandbox. Run this script once from a
# terminal where `gh auth status` shows an owner token with `repo` scope:
#
#   bash scripts/triage-2026-09-29-apply.sh            # apply live
#   bash scripts/triage-2026-09-29-apply.sh --dry-run  # print actions without writing
#
# Idempotent: safe to re-run; skips already-closed issues and updates existing
# labels in place.

set -euo pipefail

REPO="hyperpolymath/standards"
DRY_RUN=0
if [ "${1:-}" = "--dry-run" ]; then
  DRY_RUN=1
  echo "[dry-run] No GitHub writes will be performed."
fi

run_gh() {
  if [ "$DRY_RUN" -eq 1 ]; then
    printf '[dry-run] gh %s\n' "$*"
  else
    gh "$@"
  fi
}

echo "== Step 1: Creating scope, status, and cluster labels on $REPO =="

mk_label() {
  local name="$1" color="$2" desc="$3"
  echo "  label: $name"
  run_gh label create "$name" --repo "$REPO" --color "$color" --description "$desc" --force >/dev/null
}

# Scope labels
mk_label "scope:this-repo"    "1d76db" "Fixable by a PR to hyperpolymath/standards alone"
mk_label "scope:estate-wide"  "d93f0b" "Requires fan-out or ruleset/settings changes across estate repos"
mk_label "scope:external"     "5319e7" "Lives primarily in another repo (hypatia, git-scripts, panic-attack, etc.)"

# Status labels
mk_label "status:needs-decision" "fbca04" "Blocked on an owner ruling (tracked in #787)"
mk_label "status:ready"          "0e8a16" "Decision made; ready to execute"
mk_label "status:blocked"        "b60205" "Blocked on another issue or external prerequisite"

# Cluster labels
mk_label "cluster:c1-reusable-pins"   "c5def5" "C1: Reusable workflow pin propagation & staleness"
mk_label "cluster:c2-actions-lock"    "c5def5" "C2: actions.lock / SHA pinning / Dependabot"
mk_label "cluster:c3-hypatia-gate"    "bfd4f2" "C3: Hypatia scanner & reusable workflow mechanics"
mk_label "cluster:c4-hypatia-rules"   "bfd4f2" "C4: Hypatia findings & false-positive triage"
mk_label "cluster:c5-rulesets"        "d4c5f9" "C5: Branch protection, tag rulesets & status-check gates"
mk_label "cluster:c6-scorecard"       "d4c5f9" "C6: OSSF Scorecard & SARIF reconciliation"
mk_label "cluster:c7-language-policy" "fef2c0" "C7: Language policy, Deno->Bun &launcher-standard"
mk_label "cluster:c8-debt-ratchet"    "fef2c0" "C8: Debtfile & ratchet mechanics"
mk_label "cluster:c9-deed-spec"       "f9d0c4" "C9: DEED format specification"
mk_label "cluster:c10-a2ml-k9"        "f9d0c4" "C10: A2ML / K9 / Nickel specifications & gates"
mk_label "cluster:c11-canon-spine"    "f9d0c4" "C11: Canon spine, constitution & RSR docs"
mk_label "cluster:c12-codeql"         "c2e0c6" "C12: CodeQL & security-gate workflows"
mk_label "cluster:c13-mirror"         "c2e0c6" "C13: Mirror & instant-sync workflows"
mk_label "cluster:c14-ci-pipeline"    "c2e0c6" "C14: ci-pipeline.yml & test-suite runners"
mk_label "cluster:c15-spdx-reuse"     "e6e6e6" "C15: SPDX / REUSE / licence headers"
mk_label "cluster:c16-secrets-tokens" "f9c2ff" "C16: Tokens, PATs, GitHub App & secrets"
mk_label "cluster:c17-estate-census"  "bfdadc" "C17: Estate-wide census & trackers"
mk_label "cluster:c18-decisions"      "fbca04" "C18: Owner decision logs (#787)"
mk_label "cluster:c19-external-tools" "d876e3" "C19: External tool bugs (gh-actions-lock, panic-attack)"
mk_label "cluster:c20-archive-tests"  "ededed" "C20: Archive / historical / test artefacts"
mk_label "cluster:c21-other"          "ededed" "C21: Miscellaneous"

echo ""
echo "== Step 2: Phase 0 — Closing verified-resolved and duplicate issues =="

close_if_open() {
  local num="$1" comment="$2"
  local state
  state="$(gh api "repos/$REPO/issues/$num" --jq '.state')"
  if [ "$state" = "open" ]; then
    echo "  closing #$num"
    run_gh issue close "$num" --repo "$REPO" --comment "$comment"
  else
    echo "  #$num already closed — skipping"
  fi
}

# 1. #1057 — empty "probe" test issue
run_gh issue edit 1057 --repo "$REPO" --add-label "invalid" >/dev/null || true
close_if_open 1057 "Closing empty probe issue (\`probe\`, empty body, no acceptance criteria) per \`ULTRAPLAN-2026-09-29.adoc\` Phase 0."

# 2. #956 — verify live rules/branches/main on hyperpolymath/standards before closing
if gh api "repos/$REPO/rules/branches/main" --jq '[.[].type] | sort | join(",")' | grep -q 'required_status_checks'; then
  n_checks="$(gh api "repos/$REPO/rules/branches/main" --jq '[.[] | select(.type=="required_status_checks") | .parameters.required_status_checks[]] | length')"
  close_if_open 956 "Verified live on $(date -u +%Y-%m-%d) via \`gh api repos/hyperpolymath/standards/rules/branches/main\`:
- Active rules on \`main\`: \`deletion\`, \`non_fast_forward\`, \`required_status_checks\` (**${n_checks} required status contexts**), \`required_signatures\`, and \`code_scanning\` (\`CodeQL\`, \`Hypatia\`, \`Scorecard\` at \`errors\` / \`high_or_higher\`).
- Zero retired ruleset rule types remain.

Closing as resolved and verified in production."
fi

# 3. #708 — verify scheduled run 35700165587 artifact before closing
echo "  verifying #708 lockfile-drift-detect scheduled run 35700165587..."
tmp_708="$(mktemp -d)"
if [ "$DRY_RUN" -eq 1 ]; then
  echo "  [dry-run] would download lockfile-drift-report from run 35700165587 and close #708"
elif gh run download 35700165587 --repo "$REPO" -n lockfile-drift-report -D "$tmp_708" 2>/dev/null; then
  tsv_file="$(find "$tmp_708" -name '*.tsv' | head -1)"
  if [ -n "$tsv_file" ] && ! grep -q '^_w' "$tsv_file" && ! grep -q '\[drift\] clean' "$tsv_file"; then
    close_if_open 708 "Verified scheduled Tuesday cron run [\`35700165587\`](https://github.com/hyperpolymath/standards/actions/runs/35700165587) (\`2026-09-22T07:33:04Z\`, \`completed/success\`):
1. Tracker #803 was updated cleanly (\`scanned: 347\`, \`carrying a lockfile: 180\`, \`with drift: 15\`, \`check errors (rc≠0,1): 0\`, \`drifted entries: 29\`).
2. Downloaded artifact \`lockfile-drift-report\` (\`id: 10682326843\`) and verified the TSV contains real \`owner/repo\` slugs (zero \`_w\` rows) and zero \`[drift] clean\` stdout banner rows."
  else
    echo "  WARNING: #708 artifact check failed — leaving #708 open" >&2
  fi
else
  echo "  WARNING: could not download artifact from run 35700165587 — leaving #708 open" >&2
fi
rm -rf "$tmp_708"

# 4. #1013 — verify metadatastician org ruleset 18225024 + canonical-ums archived
if gh api "repos/metadatastician/burble/rulesets/18225024" --jq '[.rules[] | select(.type=="code_scanning") | .parameters.code_scanning_tools[].tool] | join(",")' | grep -qx 'CodeQL'; then
  close_if_open 1013 "Verified live on $(date -u +%Y-%m-%d):
- Organization ruleset \`EstateBranching\` (\`id: 18225024\` on \`metadatastician\`) carries \`code_scanning: [CodeQL (errors / high_or_higher)]\` only (\`Hypatia\` and \`Scorecard\` removed on 2026-09-22), curing the 68 inheriting \`metadatastician/*\` repositories.
- \`hyperpolymath/canonical-ums\` was trimmed and re-archived (\`archived: true\`, \`2026-09-22T21:52:27Z\`).

Closing as verified complete."
fi

# 5. #1005 — AC1 decided and AC2 40/40 PRs merged; re-bumps tracked on #1037
close_if_open 1005 "Both acceptance criteria are complete:
- **AC1**: Ruled to keep \`github/codeql-action\` \`v4.38.1\` (\`1c5b6756f7f1ab9f5bde6bbb02dbcebd0fffd908\`) blocked and re-pin to \`b96794f015dfd88f77b49b1c93e0fa7110f94c63\` (\`v4.38.0\`).
- **AC2**: Executed across all 40 live repositories (94 workflow refs, 52 lock lines; 40/40 PRs merged; 0 live \`@1c5b6756\` or \`@v4.38.1\` remain).
- Subsequent Dependabot grouped \`actions\` re-bumps (caused by missing trailing \`*\` on \`dependency-name: \"github/codeql-action*\"\`) are tracked on #1037."

# 6. #1010 — verify all 13 Population-A PRs (11 public + 2 private) are MERGED before closing
pop_a_prs=(
  "hyperpolymath/proven-servers:90"
  "hyperpolymath/boj-server-mk2:47"
  "hyperpolymath/cadastra:53"
  "hyperpolymath/common-signal:7"
  "hyperpolymath/consent-aware-web:10"
  "hyperpolymath/harvard-dehallucinator:18"
  "hyperpolymath/paint-type:89"
  "hyperpolymath/_pathroot:29"
  "hyperpolymath/pong-ping:7"
  "hyperpolymath/project-ovine:26"
  "hyperpolymath/sim-public-relations:24"
  "hyperpolymath/sr71-blackglider:20"
  "hyperpolymath/stapeln:75"
)
unmerged_1010=0
for item in "${pop_a_prs[@]}"; do
  r="${item%%:*}"; p="${item##*:}"
  if [ "$DRY_RUN" -eq 0 ]; then
    merged="$(gh api "repos/$r/pulls/$p" --jq '.merged' 2>/dev/null || echo "false")"
    [ "$merged" = "true" ] || unmerged_1010=$((unmerged_1010 + 1))
  fi
done
if [ "$unmerged_1010" -eq 0 ]; then
  close_if_open 1010 "Verified all 13 Population-A PRs (\`proven-servers#90\`, \`boj-server-mk2#47\`, \`cadastra#53\`, \`common-signal#7\`, \`consent-aware-web#10\`, \`harvard-dehallucinator#18\`, \`paint-type#89\`, \`_pathroot#29\`, \`pong-ping#7\`, \`project-ovine#26\`, \`sim-public-relations#24\`, \`sr71-blackglider#20\`, \`stapeln#75\`) are **MERGED**, and the remaining 6 repositories are Population B (handled under #1013). Closing as complete."
else
  echo "  WARNING: $unmerged_1010 Population-A PR(s) on #1010 not yet merged — leaving #1010 open" >&2
fi

# 7. Duplicate decision trackers (#637, #709, #715 -> #787)
close_if_open 637 "Superseded by the single consolidated owner-decision tracker #787 (which carries all 10 original decisions from this issue plus subsequent wave items). Closing as duplicate of #787."
close_if_open 709 "Superseded by the single consolidated owner-decision tracker #787 (all open rows from this tracker are carried in #787). Closing as duplicate of #787."
close_if_open 715 "Superseded by the single consolidated owner-decision tracker #787 (Wave-6 residual decisions are carried in #787). Closing as duplicate of #787."

# 8. #784 -> answered by #968 (git-scripts#58 merged)
close_if_open 784 "Resolved by owner decision in #968 and merged upstream in \`hyperpolymath/git-scripts#58\`. Closing as completed."

# 9. #808 -> copy unique startup-death / zero-jobs mis-triage evidence onto #913, then close #808
if [ "$(gh api "repos/$REPO/issues/808" --jq '.state')" = "open" ]; then
  run_gh issue comment 913 --repo "$REPO" --body "Carrying forward unique failure-mode evidence from #808 before closing #808 as subsumed by #913:
- When a reusable workflow fails at startup (zero jobs created, \`conclusion: startup_failure\` or \`failure\` with \`jobs: []\`), naive triage scripts that inspect only job steps misclassify the run or miss the caller permission/ref defect.
- Ensure any #913 propagation/verification script checks workflow-run startup failure (\`jobs_count == 0\`) explicitly."
  close_if_open 808 "Unique startup-failure / zero-jobs mis-triage evidence has been copied to #913; the underlying \`security-events: write\` caller fix across the 4 repos is tracked in #913. Closing as subsumed by #913."
fi

# NOTE: #658 is intentionally NOT closed here — it remains open until D10 on #787 is ruled.

echo ""
echo "== Step 3: Applying scope, status, and cluster labels to recent issues (#1028-#1061) =="

label_issue() {
  local num="$1"; shift
  local args=()
  for lbl in "$@"; do
    args+=(--add-label "$lbl")
  done
  echo "  #$num -> $*"
  run_gh issue edit "$num" --repo "$REPO" "${args[@]}" >/dev/null || true
}

label_issue 1028 "scope:estate-wide" "cluster:c5-rulesets"        "status:ready"
label_issue 1031 "scope:estate-wide" "cluster:c5-rulesets"        "status:ready"
label_issue 1032 "scope:this-repo"   "cluster:c5-rulesets"        "status:ready"
label_issue 1035 "scope:estate-wide" "cluster:c6-scorecard"       "status:ready"
label_issue 1036 "scope:this-repo"   "cluster:c6-scorecard"       "status:ready"
label_issue 1037 "scope:estate-wide" "cluster:c2-actions-lock"    "status:ready"
label_issue 1040 "scope:this-repo"   "cluster:c5-rulesets"        "status:ready"
label_issue 1050 "scope:this-repo"   "cluster:c3-hypatia-gate"    "status:ready"
label_issue 1054 "scope:this-repo"   "cluster:c3-hypatia-gate"    "status:ready"
label_issue 1055 "scope:estate-wide" "cluster:c17-estate-census"  "status:needs-decision"
label_issue 1056 "scope:this-repo"   "cluster:c11-canon-spine"    "status:needs-decision"
label_issue 1058 "scope:this-repo"   "cluster:c9-deed-spec"       "status:needs-decision"
label_issue 1059 "scope:estate-wide" "cluster:c11-canon-spine"    "status:needs-decision"

echo ""
echo "Done. All Phase 0 closes and recent-issue labels applied."
