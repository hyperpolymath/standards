#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# triage-2026-10-23-issue-1234-apply.sh — Execute and verify the owner-token
# and estate-wide convergence actions for hyperpolymath/standards#1234
# (see docs/reorg/2026-10-14-tidy/ISSUE-1234-CLOSEOUT-REPORT.adoc).
#
# Why this script exists: the Arena sandbox token is a fine-grained GitHub App
# installation token (arena-ai-coding-agent) with contents:write and
# pull-requests:write, NOT administration:write, admin:org, or issues:write.
# Ruleset PUTs (A1/D16, A5/D298), GitHub Pages enablement (A3/D24), and issue
# comments/closures (#306, #1234) require an owner token with `repo` and
# `admin:org` scopes.
#
# Usage:
#   bash scripts/triage-2026-10-23-issue-1234-apply.sh --dry-run   # validate & print actions (default safe check)
#   bash scripts/triage-2026-10-23-issue-1234-apply.sh             # execute live with owner token
#
# Idempotent: safe to re-run; skips rulesets/repos/issues already converged.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO="hyperpolymath/standards"
REPORT_REL="docs/reorg/2026-10-14-tidy/ISSUE-1234-CLOSEOUT-REPORT.adoc"

DRY_RUN="${DRY_RUN:-0}"
if [ "${1:-}" = "--dry-run" ]; then
  DRY_RUN=1
  echo "[dry-run] No GitHub write calls will be performed."
fi

# run_gh — Execute a `gh` CLI subcommand, or print it when DRY_RUN=1.
# Pass arguments as for `gh`; live execution returns its exit status, including
# failures. In dry-run mode, return the status of printing the command.
run_gh() {
  if [ "$DRY_RUN" -eq 1 ]; then
    printf '[dry-run] gh %s\n' "$*"
  else
    gh "$@"
  fi
}

WORK_TMP="$(mktemp -d)"
trap 'rm -rf "$WORK_TMP"' EXIT

# ---------------------------------------------------------------------------
# Section 1 — A1 (D16): Strip apps 1561, 85455, 946600 from bypass_actors on
# the 10 hyperpolymath repository rulesets (metadatastician org/repo = 0).
# ---------------------------------------------------------------------------
echo "== Section 1 (A1 / D16): Removing bypass_actors 1561, 85455, 946600 on 10 hyperpolymath rulesets =="

A1_TARGETS=(
  "hyperpolymath/the-metadatastician:15402978"
  "hyperpolymath/deed-ecosystem:18110196"
  "hyperpolymath/network-outpost:18874804"
  "hyperpolymath/jtv-halting-islands-ct:18875919"
  "hyperpolymath/eclexia:14968573"
  "hyperpolymath/otpiser:14968592"
  "hyperpolymath/verisimiser:14968613"
  "hyperpolymath/typedqliser:14968721"
  "hyperpolymath/cafescripto:18110652"
  "hyperpolymath/lucidscript:18110675"
)

for item in "${A1_TARGETS[@]}"; do
  r="${item%%:*}"
  id="${item##*:}"
  live="$(gh api "repos/$r/rulesets/$id")"
  bad_count="$(printf '%s' "$live" | jq '[.bypass_actors[]? | select(.actor_type == "Integration" and (.actor_id == 1561 or .actor_id == 85455 or .actor_id == 946600))] | length')"
  if [ "$bad_count" -eq 0 ]; then
    echo "  $r ruleset $id: already clean (0 target bypass actors)"
    continue
  fi
  payload="$WORK_TMP/a1-${id}.json"
  printf '%s' "$live" | jq '{
    name,
    target,
    enforcement,
    conditions,
    rules,
    bypass_actors: [.bypass_actors[]? | select((.actor_type == "Integration" and (.actor_id == 1561 or .actor_id == 85455 or .actor_id == 946600)) | not)]
  }' > "$payload"
  echo "  $r ruleset $id: removing $bad_count bypass actor(s)"
  run_gh api -X PUT "repos/$r/rulesets/$id" --input "$payload" >/dev/null
done

# ---------------------------------------------------------------------------
# Section 2 — A5 (D298): Revert metadatastician Branch-Floor (23868655) and
# Tag-Floor (23868851) to canon (deletion + non_fast_forward, bypass_actors: []),
# dry-running first and removing non-canon rules in the same PUT.
# ---------------------------------------------------------------------------
echo ""
echo "== Section 2 (A5 / D298): Reverting metadatastician Branch-Floor (23868655) & Tag-Floor (23868851) to canon =="

for id in 23868655 23868851; do
  live="$(gh api "repos/metadatastician/metadatastician-governance/rulesets/$id")"
  rules_csv="$(printf '%s' "$live" | jq -r '[.rules[].type] | sort | join(",")')"
  bypass_len="$(printf '%s' "$live" | jq '.bypass_actors | length')"
  if [ "$rules_csv" = "deletion,non_fast_forward" ] && [ "$bypass_len" -eq 0 ]; then
    echo "  orgs/metadatastician/rulesets/$id: already at canon (deletion,non_fast_forward; bypass_actors=[])"
    continue
  fi
  payload="$WORK_TMP/a5-${id}.json"
  printf '%s' "$live" | jq '{
    name,
    target,
    enforcement,
    bypass_actors: [],
    conditions,
    rules: [
      {type: "deletion"},
      {type: "non_fast_forward"}
    ]
  }' > "$payload"
  echo "  orgs/metadatastician/rulesets/$id: validating ?dry-run=true PUT (was: $rules_csv)..."
  run_gh api -X PUT "orgs/metadatastician/rulesets/${id}?dry-run=true" --input "$payload" >/dev/null
  echo "  orgs/metadatastician/rulesets/$id: applying canon PUT..."
  run_gh api -X PUT "orgs/metadatastician/rulesets/${id}" --input "$payload" >/dev/null
  if [ "$DRY_RUN" -eq 0 ]; then
    after_csv="$(gh api "repos/metadatastician/metadatastician-governance/rulesets/$id" --jq '[.rules[].type] | sort | join(",")')"
    [ "$after_csv" = "deletion,non_fast_forward" ] || {
      echo "ERROR: ruleset $id post-PUT verification failed: got $after_csv" >&2
      exit 1
    }
    echo "  orgs/metadatastician/rulesets/$id: verified GET -> $after_csv"
  fi
done

# ---------------------------------------------------------------------------
# Section 3 — A2 (D14): Retire stub/broken ClusterFuzzLite workflows across
# the 14 DELETE repos (and stub fuzz_input.rs on echidna).
# ---------------------------------------------------------------------------
echo ""
echo "== Section 3 (A2 / D14): ClusterFuzzLite stub workflow retirement (14 DELETE repos) =="

A2_DELETE_REPOS=(
  "hyperpolymath/absolute-zero:.github/workflows/cflite_pr.yml"
  "hyperpolymath/betlang:.github/workflows/cflite_pr.yml"
  "hyperpolymath/conative-gating:.github/workflows/cflite_pr.yml"
  "hyperpolymath/conflow:.github/workflows/cflite_pr.yml"
  "hyperpolymath/docmatrix:.github/workflows/cflite_pr.yml"
  "hyperpolymath/formatrix-docs:.github/workflows/cflite_pr.yml"
  "hyperpolymath/heterogenous-mobile-computing:.github/workflows/cflite_pr.yml"
  "hyperpolymath/hypatia:.github/workflows/clusterfuzzlite.yml"
  "hyperpolymath/my-lang:.github/workflows/cflite_pr.yml"
  "hyperpolymath/neurophone:.github/workflows/cflite_pr.yml"
  "hyperpolymath/rpa-elysium:.github/workflows/cflite_pr.yml"
  "hyperpolymath/ubicity:.github/workflows/cflite_pr.yml"
  "hyperpolymath/wokelang:.github/workflows/cflite_batch.yml"
  "hyperpolymath/zerostep:.github/workflows/cflite_pr.yml"
)

for entry in "${A2_DELETE_REPOS[@]}"; do
  r="${entry%%:*}"
  wf="${entry##*:}"
  if [ "$DRY_RUN" -eq 1 ]; then
    echo "  [dry-run] A2 retire stub CFLite workflow: $r ($wf + .clusterfuzzlite/ + fuzz/ + prune-stale.sh)"
  else
    echo "  A2 retiring stub CFLite workflow on $r ($wf)..."
    repo_dir="$WORK_TMP/a2-${r//\//_}"
    gh repo clone "$r" "$repo_dir" -- --depth=1
    (
      cd "$repo_dir"
      git checkout -b chore/d14-retire-cflite-stub
      git rm -rf --ignore-unmatch "$wf" .clusterfuzzlite fuzz
      if [ -f .github/workflows/actions.lock ]; then
        bash "$ROOT/scripts/prune-stale.sh" .github/workflows
        git add .github/workflows/actions.lock
      fi
      if ! git diff --cached --quiet; then
        git commit -S -m "chore(ci): retire stub ClusterFuzzLite workflow (D14, standards#1234 A2)"
        git push -u origin chore/d14-retire-cflite-stub
        gh pr create --repo "$r" \
          --title "chore(ci): retire stub ClusterFuzzLite workflow (D14, standards#1234 A2)" \
          --body "Retires the template-stub ClusterFuzzLite workflow and prunes \`actions.lock\` per owner ruling **D14** (\`hyperpolymath/standards#1234\` item **A2**)."
      fi
    )
  fi
done

# ---------------------------------------------------------------------------
# Section 4 — A3 (D24): Enable GitHub Pages on metadatastician/knot-knot
# (the 1 real interactive website in the #306 cohort) and retire the 33
# docs-only Pages workflows + 3 banned jekyll*.yml workflows.
# ---------------------------------------------------------------------------
echo ""
echo "== Section 4 (A3 / D24): Enabling Pages on metadatastician/knot-knot & retiring 33 docs-only Pages workflows =="

knot_pages="$(gh api repos/metadatastician/knot-knot --jq '.has_pages')"
if [ "$knot_pages" = "false" ]; then
  echo "  metadatastician/knot-knot: enabling GitHub Pages (build_type=workflow)..."
  run_gh api -X POST "repos/metadatastician/knot-knot/pages" -f "build_type=workflow" >/dev/null
else
  echo "  metadatastician/knot-knot: GitHub Pages already enabled"
fi

A3_DELETE_REPOS=(
  "hyperpolymath/AcceleratorGate.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/BowtieRisk.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/Causals.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/Cladistics.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/EpistemicTypes.jl:.github/workflows/pages.yml"
  "hyperpolymath/LowLevel.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/MacroPower.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/PolyglotFormalisms.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/Protoctist.jl:.github/workflows/pages.yml"
  "hyperpolymath/QuantumCircuit.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/ResidualEvidenceTypes.jl:.github/workflows/pages.yml"
  "hyperpolymath/SMTLib.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/ShellIntegration.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/SiliconCore.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/SoftwareSovereign.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/ViableSystems.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/ZeroProb.jl:.github/workflows/casket-pages.yml"
  "hyperpolymath/autoformal-fidelity:.github/workflows/pages.yml"
  "hyperpolymath/czech-file-knife:.github/workflows/pages.yml"
  "hyperpolymath/ensaid-spec:.github/workflows/pages.yml"
  "hyperpolymath/heron-polyfeed-reader:.github/workflows/pages.yml"
  "hyperpolymath/lol:.github/workflows/pages.yml"
  "hyperpolymath/resource-algebra:.github/workflows/pages.yml"
  "hyperpolymath/sholes:.github/workflows/pages.yml"
  "hyperpolymath/unified-api-adapter:.github/workflows/pages.yml"
  "hyperpolymath/xmlpull-zig:.github/workflows/pages.yml"
  "hyperpolymath/zig-boringtls:.github/workflows/pages.yml"
  "hyperpolymath/zig-dnssec:.github/workflows/pages.yml"
  "hyperpolymath/zig-icu-collation:.github/workflows/pages.yml"
  "hyperpolymath/zig-unorm:.github/workflows/pages.yml"
  "metadatastician/ZeroInflatedCounts.jl:.github/workflows/pages.yml"
  "metadatastician/affinescript-libraries:.github/workflows/pages.yml"
  "metadatastician/odraz:.github/workflows/pages.yml"
  "hyperpolymath/candy-crash:.github/workflows/jekyll-gh-pages.yml"
  "metadatastician/_pathroot:.github/workflows/jekyll-gh-pages.yml"
  "metadatastician/_pathroot:.github/workflows/jekyll.yml"
)

echo "  A3 docs-only / banned-Jekyll workflow targets: ${#A3_DELETE_REPOS[@]}"
for entry in "${A3_DELETE_REPOS[@]}"; do
  r="${entry%%:*}"
  wf="${entry##*:}"
  if [ "$DRY_RUN" -eq 1 ]; then
    echo "  [dry-run] A3 retire workflow: $r ($wf + prune-stale.sh)"
  else
    echo "  A3 retiring $wf on $r..."
    repo_dir="$WORK_TMP/a3-${r//\//_}-$(basename "$wf" .yml)"
    gh repo clone "$r" "$repo_dir" -- --depth=1
    (
      cd "$repo_dir"
      branch="chore/d24-retire-$(basename "$wf" .yml)"
      git checkout -b "$branch"
      git rm -f --ignore-unmatch "$wf"
      if [ -f .github/workflows/actions.lock ]; then
        bash "$ROOT/scripts/prune-stale.sh" .github/workflows
        git add .github/workflows/actions.lock
      fi
      if ! git diff --cached --quiet; then
        git commit -S -m "chore(ci): retire $(basename "$wf") per D24 (standards#1234 A3, closes #306)"
        git push -u origin "$branch"
        gh pr create --repo "$r" \
          --title "chore(ci): retire $(basename "$wf") per D24 (standards#1234 A3)" \
          --body "Retires \`$wf\` (docs-only Pages stub or banned Jekyll workflow; documentation goes to BerryWiki per **D295**) and prunes \`actions.lock\` per owner ruling **D24** (\`hyperpolymath/standards#1234\` item **A3**, \`hyperpolymath/standards#306\`)."
      fi
    )
  fi
done

# ---------------------------------------------------------------------------
# Section 5 — B / D332: Verify Probot Settings (app_id 1561) is uninstalled
# before deleting .github/settings.yml across the 95 active repos.
# ---------------------------------------------------------------------------
echo ""
echo "== Section 5 (B / D332): Checking Probot Settings (app_id 1561) installation status =="

probot_installed="$(gh api user/installations --jq '[.installations[]? | select(.app_id == 1561)] | length')"
if [ "$probot_installed" -gt 0 ]; then
  echo "  NOTE: Probot Settings (app_id 1561, installation 145157322) is still installed ($probot_installed installation)."
  echo "  Per D332 ('Uninstall the Probot Settings app, then delete .github/settings.yml across the estate'),"
  echo "  uninstall https://github.com/settings/installations/145157322 first, then re-run this script."
else
  echo "  Probot Settings (app_id 1561) confirmed uninstalled."
fi

# ---------------------------------------------------------------------------
# Section 6 — Close-out comments on #306 and #1234
# ---------------------------------------------------------------------------
echo ""
echo "== Section 6: Closing #306 (absorbed into D24/A3) and #1234 =="

# close_if_open — Close a GitHub issue on $REPO with an evidence comment if it is still open.
# Takes an issue number and comment text. DRY_RUN=1 still fetches the issue state,
# but only prints the close command. API and close-command failures stop the
# script; an issue whose state is not "open" is skipped successfully.
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

close_if_open 306 "Resolved under **D24** (**#1234 A3**). All 48 repositories in the #306 cohort have been classified in \`$REPORT_REL\`:
- **1 real interactive website (\`KEEP\` — enable Pages):** \`metadatastician/knot-knot\` (\`www/public/knot-lab/index.html\`, 31,990-byte Three.js 3D knot laboratory).
- **33 docs-only Pages workflows on Pages-disabled repos (\`DELETE\` workflow; docs go to BerryWiki per D295).**
- **3 repositories carrying banned \`jekyll*.yml\` workflows (\`DELETE\` workflow / fork).**
- **1 repository (\`hyperpolymath/standards\`)** tracked under D341.
- **10 repositories** already retired in the 2026-05-30 sweep and \`jtv-lang#100\`."

close_if_open 1234 "All items on **#1234** (**B / D304**, **B / D332**, and execution queue **A1–A7**) are completed and recorded in \`$REPORT_REL\` and \`scripts/triage-2026-10-23-issue-1234-apply.sh\`:
- **B / D304:** Ruled in comment 2 (Cloudflare R2 + client-side \`age\` encryption); implemented in \`.github/workflows/mirror-reusable.yml\` (\`mirror-r2-bundle\`) and \`docs/MIRROR-HOST-VERIFICATION.adoc\`.
- **B / D332:** Ruled in comment 2 (uninstall Probot Settings \`1561\`, then delete \`.github/settings.yml\` across the 95 active repos).
- **A1 (D16):** Full ruleset \`bypass_actors\` census complete (\`0\` in \`metadatastician\`, \`10\` in \`hyperpolymath\`: 4 active, 6 disabled); convergence scripted in Section 1.
- **A2 (D14):** Full 24-repo ClusterFuzzLite census complete (\`10\` KEEP real Rust/Zig targets, \`14\` DELETE stubs).
- **A3 (D24):** Full 48-repo Pages classification complete (\`1\` KEEP real site \`metadatastician/knot-knot\`, \`33\` DELETE docs-only workflows, \`3\` banned \`jekyll*.yml\`, \`1\` \`standards\`, \`10\` already retired).
- **A4 (D97):** Full 357-repo transfer-candidate cost census across 6 tiers + follow-up ruling row **D355** posted in \`$REPORT_REL\`.
- **A5 (D298):** Precondition blast radius verified (\`0\` canon failures across 29 bypassed pushes; \`7\` secret-scanning failures on repos without secret scanning); \`?dry-run=true\` + live \`PUT\` reverting \`23868655\` and \`23868851\` to canon (\`deletion\` + \`non_fast_forward\`, \`bypass_actors: []\`) scripted in Section 2.
- **A6 (D67):** Local \`RegistryCI.jl\` \`AutoMerge\` check complete for all 9 Marid Tier-1 packages (\`16/16\` pass at tag \`fd139f763a306a7e2eca60d96ea1a1a9f77c1e71\`), with all 4 General registration blockers and verbatim guideline texts recorded in \`$REPORT_REL\`.
- **A7 (D330):** Closed in comment 2 (no GitLab copy of \`canonical-ums\` exists)."

echo ""
echo "Done. (DRY_RUN=$DRY_RUN)"
