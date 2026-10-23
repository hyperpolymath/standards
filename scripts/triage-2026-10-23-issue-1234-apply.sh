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
# Idempotent: safe to re-run; skips rulesets/repos/branches/issues already converged.

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

# ensure_open_pr — Ensure an open pull request exists for a pushed branch on a repository.
# Takes owner/repo, head branch, title, and body; creates the PR only when no
# open PR for that head branch exists yet, making interrupted runs resumable.
# Does not check DRY_RUN; callers must guard creation. Failures from `gh pr list`
# or `gh pr create` stop the script under `set -e`.
ensure_open_pr() {
  local repo="$1" branch="$2" title="$3" body="$4"
  local open_count
  open_count="$(gh pr list --repo "$repo" --head "$branch" --state open --json number --jq 'length')"
  if [ "$open_count" -gt 0 ]; then
    echo "  $repo: open PR already exists for branch $branch — skipping PR create"
  else
    gh pr create --repo "$repo" --head "$branch" --title "$title" --body "$body"
  fi
}

# check_probot_installed — Check Probot Settings (app_id 1561) for one owner.
# The personal account uses user/installations, filtered by account login;
# the organisation uses orgs/{owner}/installations. Paginate both endpoints.
# Prints the matching installation count and returns 0.
# If the query fails or returns invalid output, DRY_RUN=1 prints 1 and returns 0
# so callers treat the app as installed. In live mode, reports the query failure
# on stderr and returns 1.
check_probot_installed() {
  local owner="$1" endpoint count=""
  case "$owner" in
    hyperpolymath) endpoint=user/installations ;;
    metadatastician) endpoint=orgs/metadatastician/installations ;;
    *) echo "ERROR: unsupported installation owner: $owner" >&2; return 1 ;;
  esac
  if count="$(gh api "$endpoint" --paginate --slurp --jq "[.[].installations[]? | select(.app_id == 1561 and .account.login == \"$owner\")] | length" 2>/dev/null)" && [[ "$count" =~ ^[0-9]+$ ]]; then
    printf '%s\n' "$count"
    return 0
  fi
  if [ "$DRY_RUN" -eq 1 ]; then
    printf '1\n'
    return 0
  fi
  echo "ERROR: unable to query $endpoint for $owner (requires owner access)" >&2
  return 1
}

WORK_TMP="$(mktemp -d)"
trap 'rm -rf "$WORK_TMP"' EXIT

A1_PENDING=0
A5_PENDING=0
A2_PENDING=0
A3_PENDING=0
# General registrations remain blocked (A6/D67); this script does not resolve
# them. Clear this pending item only after their resolution is verified.
A6_PENDING=1
D332_PENDING=0

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
  if [ "$DRY_RUN" -eq 1 ]; then
    A1_PENDING=$((A1_PENDING + 1))
  fi
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
  else
    A5_PENDING=$((A5_PENDING + 1))
  fi
done

# ---------------------------------------------------------------------------
# Section 3 — A2 (D14): Retire stub/broken ClusterFuzzLite workflows across
# the 14 DELETE repos, and remove the leftover stub fuzz_input.rs on echidna.
# ---------------------------------------------------------------------------
echo ""
echo "== Section 3 (A2 / D14): ClusterFuzzLite stub workflow retirement (14 DELETE repos + echidna stub) =="

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
  branch="chore/d14-retire-cflite-stub"
  pr_title="chore(ci): retire stub ClusterFuzzLite workflow (D14, standards#1234 A2)"
  pr_body="Retires the template-stub ClusterFuzzLite workflow and prunes \`actions.lock\` per owner ruling **D14** (\`hyperpolymath/standards#1234\` item **A2**)."
  if [ "$DRY_RUN" -eq 1 ]; then
    echo "  [dry-run] A2 retire stub CFLite workflow: $r ($wf + .clusterfuzzlite/ + fuzz/ + prune-stale.sh)"
    A2_PENDING=$((A2_PENDING + 1))
  else
    echo "  A2 checking $r ($wf)..."
    repo_dir="$WORK_TMP/a2-${r//\//_}"
    gh repo clone "$r" "$repo_dir" -- --depth=1
    if [ ! -e "$repo_dir/$wf" ] && [ ! -d "$repo_dir/.clusterfuzzlite" ] && [ ! -d "$repo_dir/fuzz" ]; then
      echo "  $r: already converged on default branch"
      continue
    fi
    A2_PENDING=$((A2_PENDING + 1))
    if git -C "$repo_dir" ls-remote --exit-code --heads origin "$branch" >/dev/null 2>&1; then
      echo "  $r: remote branch $branch already exists"
      ensure_open_pr "$r" "$branch" "$pr_title" "$pr_body"
      continue
    fi
    (
      cd "$repo_dir"
      git checkout -b "$branch"
      git rm -rf --ignore-unmatch "$wf" .clusterfuzzlite fuzz
      if [ -f .github/workflows/actions.lock ]; then
        bash "$ROOT/scripts/prune-stale.sh" .github/workflows
        git add .github/workflows/actions.lock
      fi
      if ! git diff --cached --quiet; then
        git commit -S -m "$pr_title"
        git push -u origin "$branch"
        ensure_open_pr "$r" "$branch" "$pr_title" "$pr_body"
      fi
    )
  fi
done

ECHIDNA_REPO="hyperpolymath/echidna"
ECHIDNA_STUB="fuzz/fuzz_targets/fuzz_input.rs"
ECHIDNA_BRANCH="chore/d14-retire-echidna-fuzz-stub"
ECHIDNA_TITLE="chore(fuzz): remove leftover template stub fuzz_input.rs (D14, standards#1234 A2)"
ECHIDNA_BODY="Removes the leftover template stub \`fuzz/fuzz_targets/fuzz_input.rs\` while retaining the 3 real fuzz targets (\`fuzz_aspect_tagging.rs\`, \`fuzz_concept_Arena.rs\`, \`fuzz_dispatch.rs\`) per owner ruling **D14** (\`hyperpolymath/standards#1234\` item **A2**)."

if [ "$DRY_RUN" -eq 1 ]; then
  echo "  [dry-run] A2 remove leftover stub target on $ECHIDNA_REPO ($ECHIDNA_STUB)"
  A2_PENDING=$((A2_PENDING + 1))
else
  echo "  A2 checking $ECHIDNA_REPO ($ECHIDNA_STUB)..."
  echidna_dir="$WORK_TMP/a2-echidna"
  gh repo clone "$ECHIDNA_REPO" "$echidna_dir" -- --depth=1
  if [ ! -e "$echidna_dir/$ECHIDNA_STUB" ]; then
    echo "  $ECHIDNA_REPO: $ECHIDNA_STUB already absent on default branch"
  else
    A2_PENDING=$((A2_PENDING + 1))
    if git -C "$echidna_dir" ls-remote --exit-code --heads origin "$ECHIDNA_BRANCH" >/dev/null 2>&1; then
      echo "  $ECHIDNA_REPO: remote branch $ECHIDNA_BRANCH already exists"
      ensure_open_pr "$ECHIDNA_REPO" "$ECHIDNA_BRANCH" "$ECHIDNA_TITLE" "$ECHIDNA_BODY"
    else
      (
        cd "$echidna_dir"
        git checkout -b "$ECHIDNA_BRANCH"
        git rm -f "$ECHIDNA_STUB"
        git commit -S -m "$ECHIDNA_TITLE"
        git push -u origin "$ECHIDNA_BRANCH"
        ensure_open_pr "$ECHIDNA_REPO" "$ECHIDNA_BRANCH" "$ECHIDNA_TITLE" "$ECHIDNA_BODY"
      )
    fi
  fi
fi

# ---------------------------------------------------------------------------
# Section 4 — A3 (D24): Update metadatastician/knot-knot pages.yml to publish
# www/public/, enable GitHub Pages on metadatastician/knot-knot, and retire
# the 33 docs-only Pages workflows + 3 banned jekyll*.yml workflows.
# ---------------------------------------------------------------------------
echo ""
echo "== Section 4 (A3 / D24): Publishing metadatastician/knot-knot 3D knot lab & retiring 33 docs-only Pages workflows =="

KNOT_REPO="metadatastician/knot-knot"
KNOT_BRANCH="chore/d24-publish-knot-lab-pages"
KNOT_TITLE="chore(pages): publish www/public 3D knot laboratory in pages.yml (D24, standards#1234 A3)"
KNOT_BODY="Updates \`.github/workflows/pages.yml\` to overlay \`www/public/\` (\`knot-lab/index.html\`) and \`www/.well-known/\` into \`_site/\` with \`include-hidden-files: true\` per owner ruling **D24** (\`hyperpolymath/standards#1234\` item **A3**, \`hyperpolymath/standards#306\`)."

if [ "$DRY_RUN" -eq 1 ]; then
  echo "  [dry-run] A3 update $KNOT_REPO .github/workflows/pages.yml to publish www/public/ and www/.well-known/"
  A3_PENDING=$((A3_PENDING + 1))
else
  knot_dir="$WORK_TMP/a3-knot-knot"
  gh repo clone "$KNOT_REPO" "$knot_dir" -- --depth=1
  if grep -q 'www/public' "$knot_dir/.github/workflows/pages.yml" 2>/dev/null; then
    echo "  $KNOT_REPO: .github/workflows/pages.yml already publishes www/public/"
  else
    A3_PENDING=$((A3_PENDING + 1))
    if git -C "$knot_dir" ls-remote --exit-code --heads origin "$KNOT_BRANCH" >/dev/null 2>&1; then
      echo "  $KNOT_REPO: remote branch $KNOT_BRANCH already exists"
      ensure_open_pr "$KNOT_REPO" "$KNOT_BRANCH" "$KNOT_TITLE" "$KNOT_BODY"
    else
      (
        cd "$knot_dir"
        git checkout -b "$KNOT_BRANCH"
        awk '
          /cabal run casket-ssg -- build \.\.\/site \.\.\/_site/ {
            print
            print "          if [ -d ../www/public ]; then cp -a ../www/public/. ../_site/; fi"
            print "          if [ -d ../www/.well-known ]; then cp -a ../www/.well-known ../_site/.well-known; fi"
            next
          }
          /path: \x27_site\x27/ {
            print
            print "          include-hidden-files: true"
            next
          }
          { print }
        ' .github/workflows/pages.yml > .github/workflows/pages.yml.new
        mv .github/workflows/pages.yml.new .github/workflows/pages.yml
        git add .github/workflows/pages.yml
        if ! git diff --cached --quiet; then
          git commit -S -m "$KNOT_TITLE"
          git push -u origin "$KNOT_BRANCH"
          ensure_open_pr "$KNOT_REPO" "$KNOT_BRANCH" "$KNOT_TITLE" "$KNOT_BODY"
        fi
      )
    fi
  fi
fi

knot_pages="$(gh api repos/metadatastician/knot-knot --jq '.has_pages')"
if [ "$knot_pages" = "false" ]; then
  echo "  metadatastician/knot-knot: enabling GitHub Pages (build_type=workflow)..."
  run_gh api -X POST "repos/metadatastician/knot-knot/pages" -f "build_type=workflow" >/dev/null
  if [ "$DRY_RUN" -eq 1 ]; then
    A3_PENDING=$((A3_PENDING + 1))
  fi
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
  branch="chore/d24-retire-$(basename "$wf" .yml)"
  pr_title="chore(ci): retire $(basename "$wf") per D24 (standards#1234 A3)"
  pr_body="Retires \`$wf\` (docs-only Pages stub or banned Jekyll workflow; documentation goes to BerryWiki per **D295**) and prunes \`actions.lock\` per owner ruling **D24** (\`hyperpolymath/standards#1234\` item **A3**, \`hyperpolymath/standards#306\`)."
  if [ "$DRY_RUN" -eq 1 ]; then
    echo "  [dry-run] A3 retire workflow: $r ($wf + prune-stale.sh)"
    A3_PENDING=$((A3_PENDING + 1))
  else
    echo "  A3 checking $wf on $r..."
    repo_dir="$WORK_TMP/a3-${r//\//_}-$(basename "$wf" .yml)"
    gh repo clone "$r" "$repo_dir" -- --depth=1
    if [ ! -e "$repo_dir/$wf" ]; then
      echo "  $r: $wf already absent on default branch"
      continue
    fi
    A3_PENDING=$((A3_PENDING + 1))
    if git -C "$repo_dir" ls-remote --exit-code --heads origin "$branch" >/dev/null 2>&1; then
      echo "  $r: remote branch $branch already exists"
      ensure_open_pr "$r" "$branch" "$pr_title" "$pr_body"
      continue
    fi
    (
      cd "$repo_dir"
      git checkout -b "$branch"
      git rm -f --ignore-unmatch "$wf"
      if [ -f .github/workflows/actions.lock ]; then
        bash "$ROOT/scripts/prune-stale.sh" .github/workflows
        git add .github/workflows/actions.lock
      fi
      if ! git diff --cached --quiet; then
        git commit -S -m "chore(ci): retire $(basename "$wf") per D24 (standards#1234 A3, closes #306)"
        git push -u origin "$branch"
        ensure_open_pr "$r" "$branch" "$pr_title" "$pr_body"
      fi
    )
  fi
done

# ---------------------------------------------------------------------------
# Section 5 — B / D332: Verify Probot Settings (app_id 1561) is uninstalled
# before deleting .github/settings.yml across the 95 active repos.
# ---------------------------------------------------------------------------
echo ""
echo "== Section 5 (B / D332): Checking Probot Settings (app_id 1561) installation & retiring .github/settings.yml (95 repos) =="

D332_SETTINGS_REPOS=(
  "hyperpolymath/.github"
  "hyperpolymath/affinescript-vite"
  "hyperpolymath/anytype"
  "hyperpolymath/Apodeixis.jl"
  "hyperpolymath/atoma"
  "hyperpolymath/autoformal-fidelity"
  "hyperpolymath/bactrian"
  "hyperpolymath/bofj-kitt"
  "hyperpolymath/boj-server"
  "hyperpolymath/cafescripto"
  "hyperpolymath/cargo-zigbuild"
  "hyperpolymath/cicd-squabbler"
  "hyperpolymath/contractiles"
  "hyperpolymath/CoprocessorRuntime.jl"
  "hyperpolymath/czech-file-knife"
  "hyperpolymath/dictask"
  "hyperpolymath/echidna"
  "hyperpolymath/ephapax"
  "hyperpolymath/EpistemicTypes.jl"
  "hyperpolymath/fraying-model-computational-testbed"
  "hyperpolymath/haec"
  "hyperpolymath/hermeneia"
  "hyperpolymath/heron-polyfeed-reader"
  "hyperpolymath/ideas-to-alphas"
  "hyperpolymath/jaffascript"
  "hyperpolymath/kategoria"
  "hyperpolymath/kitchenspeak"
  "hyperpolymath/krl"
  "hyperpolymath/lithoglyph"
  "hyperpolymath/llm-grace"
  "hyperpolymath/lucidscript"
  "hyperpolymath/natsci-studio"
  "hyperpolymath/nesy-solver"
  "hyperpolymath/nextgen-typing"
  "hyperpolymath/occupancy-types"
  "hyperpolymath/panoply"
  "hyperpolymath/patch-bridge"
  "hyperpolymath/proof-burrower"
  "hyperpolymath/Protoctist.jl"
  "hyperpolymath/proven"
  "hyperpolymath/pseudoscript"
  "hyperpolymath/quintessence"
  "hyperpolymath/rattlescript"
  "hyperpolymath/reasonably-good-token-vault"
  "hyperpolymath/recon-silly-ation"
  "hyperpolymath/ResidualEvidenceTypes.jl"
  "hyperpolymath/resource-algebra"
  "hyperpolymath/rsr-template-repo"
  "hyperpolymath/scaffoldia"
  "hyperpolymath/secret-types"
  "hyperpolymath/sholes"
  "hyperpolymath/snifs"
  "hyperpolymath/somethings-fishy"
  "hyperpolymath/stateful-artefacts"
  "hyperpolymath/system-et"
  "hyperpolymath/trigger"
  "hyperpolymath/trope-checker"
  "hyperpolymath/typed-wasm"
  "hyperpolymath/unified-api-adapter"
  "hyperpolymath/vocarium"
  "hyperpolymath/xmlpull-zig"
  "hyperpolymath/zig-boringtls"
  "hyperpolymath/zig-dnssec"
  "hyperpolymath/zig-icu-collation"
  "hyperpolymath/zig-unorm"
  "metadatastician/_pathroot"
  "metadatastician/affinescript-libraries"
  "metadatastician/berrywiki"
  "metadatastician/bowtie-workbench"
  "metadatastician/burble"
  "metadatastician/cadastra"
  "metadatastician/cerro-torre"
  "metadatastician/cleave"
  "metadatastician/consent-aware-web"
  "metadatastician/first-post"
  "metadatastician/gossamer"
  "metadatastician/groove"
  "metadatastician/hvn-dehallucinator"
  "metadatastician/IDApTIK"
  "metadatastician/knot-knot"
  "metadatastician/kyq"
  "metadatastician/lintel"
  "metadatastician/marid"
  "metadatastician/metadatastician-governance"
  "metadatastician/odraz"
  "metadatastician/paint-type"
  "metadatastician/rokur"
  "metadatastician/selur"
  "metadatastician/sim-public-relations"
  "metadatastician/spline"
  "metadatastician/stapeln"
  "metadatastician/svalinn"
  "metadatastician/vordr"
  "metadatastician/ZenodoDeposits.jl"
  "metadatastician/ZeroInflatedCounts.jl"
)

echo "  D332 .github/settings.yml targets: ${#D332_SETTINGS_REPOS[@]}"
d332_branch="chore/d332-retire-settings-yml"
d332_title="chore(governance): delete .github/settings.yml after Probot Settings uninstall (D332, standards#1234)"
d332_body="Deletes \`.github/settings.yml\` now that the Probot Settings GitHub App (\`1561\`) is uninstalled; repository and organisation rulesets are the sole settings authority per owner ruling **D332** (\`hyperpolymath/standards#1234\`)."
for owner in hyperpolymath metadatastician; do
  probot_installed="$(check_probot_installed "$owner")"
  if [ "$probot_installed" -gt 0 ]; then
    echo "  $owner: Probot Settings (app_id 1561) is still installed or unverified; uninstall it before settings cleanup (D332)."
  else
    echo "  $owner: Probot Settings (app_id 1561) confirmed uninstalled — retiring .github/settings.yml..."
  fi
  for r in "${D332_SETTINGS_REPOS[@]}"; do
    [ "${r%%/*}" = "$owner" ] || continue
    if [ "$probot_installed" -gt 0 ]; then
      D332_PENDING=$((D332_PENDING + 1))
      continue
    fi
    if [ "$DRY_RUN" -eq 1 ]; then
      echo "  [dry-run] D332 retire .github/settings.yml: $r"
      D332_PENDING=$((D332_PENDING + 1))
    else
      repo_dir="$WORK_TMP/d332-${r//\//_}"
      gh repo clone "$r" "$repo_dir" -- --depth=1
      if [ ! -e "$repo_dir/.github/settings.yml" ]; then
        echo "  $r: .github/settings.yml already absent on default branch"
        continue
      fi
      D332_PENDING=$((D332_PENDING + 1))
      if git -C "$repo_dir" ls-remote --exit-code --heads origin "$d332_branch" >/dev/null 2>&1; then
        echo "  $r: remote branch $d332_branch already exists"
        ensure_open_pr "$r" "$d332_branch" "$d332_title" "$d332_body"
        continue
      fi
      (
        cd "$repo_dir"
        git checkout -b "$d332_branch"
        git rm -f .github/settings.yml
        git commit -S -m "$d332_title"
        git push -u origin "$d332_branch"
        ensure_open_pr "$r" "$d332_branch" "$d332_title" "$d332_body"
      )
    fi
  done
done

# ---------------------------------------------------------------------------
# Section 6 — Gated close-out comments on #306 and #1234
# ---------------------------------------------------------------------------
echo ""
echo "== Section 6: Verifying convergence before closing #306 (absorbed into D24/A3) and #1234 =="

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

# D329 requires deletion of the explore fork, not just its Pages workflow.
# Only an explicit HTTP 404 counts as absence; query failures stay pending.
if explore_response="$(gh api --include repos/hyperpolymath/explore 2>/dev/null)"; then
  echo "  D329: hyperpolymath/explore still exists."
  A3_PENDING=$((A3_PENDING + 1))
elif printf '%s\n' "$explore_response" | grep -Eq '^HTTP/[0-9.]+ 404([[:space:]]|$)'; then
  echo "  D329: hyperpolymath/explore is absent."
else
  echo "  D329: unable to verify deletion of hyperpolymath/explore."
  A3_PENDING=$((A3_PENDING + 1))
fi

# D341 requires all three standards Pages workflows to be absent on the
# default branch. A failed contents query stops closure under set -e.
d341_pending="$(gh api "repos/$REPO/contents/.github/workflows" --jq '[.[] | select(.name == "casket-pages.yml" or .name == "pages-archive.yml" or .name == "pages.yml")] | length')"
A3_PENDING=$((A3_PENDING + d341_pending))
echo "  D341: $d341_pending standards Pages workflow(s) remain."

TOTAL_PENDING=$((A1_PENDING + A5_PENDING + A2_PENDING + A3_PENDING + A6_PENDING + D332_PENDING))
echo "  Pending convergence items: total=$TOTAL_PENDING (A1=$A1_PENDING, A5=$A5_PENDING, A2=$A2_PENDING, A3=$A3_PENDING, A6=$A6_PENDING, D332=$D332_PENDING)"

if [ "$A3_PENDING" -eq 0 ]; then
  close_if_open 306 "Resolved under **D24** (**#1234 A3**). All 48 repositories in the #306 cohort have been classified in \`$REPORT_REL\` and converged:
- **1 real interactive website (\`KEEP\` — Pages enabled & \`www/public/\` published):** \`metadatastician/knot-knot\` (\`www/public/knot-lab/index.html\`, 31,990-byte Three.js 3D knot laboratory).
- **33 docs-only Pages workflows on Pages-disabled repos (\`DELETE\` workflow merged; docs go to BerryWiki per D295).**
- **3 repositories carrying banned \`jekyll*.yml\` workflows (\`DELETE\` workflow / fork).**
- **D329:** \`hyperpolymath/explore\` confirmed absent.
- **D341:** All 3 Pages workflows (\`casket-pages.yml\`, \`pages-archive.yml\`, \`pages.yml\`) confirmed absent from \`hyperpolymath/standards\`.
- **10 repositories** already retired in the 2026-05-30 sweep and \`jtv-lang#100\`."
else
  echo "  #306 kept open until all A3 workflow deletions, knot-knot Pages publication, D329, and D341 converge ($A3_PENDING pending)."
fi

if [ "$TOTAL_PENDING" -eq 0 ]; then
  close_if_open 1234 "Execution results for **#1234** (**B / D304**, **B / D332**, and execution queue **A1–A7**) are recorded in \`$REPORT_REL\` and \`scripts/triage-2026-10-23-issue-1234-apply.sh\`. A6 General registrations remain blocked; the local check passed:
- **B / D304:** Ruled in comment 2 (Cloudflare R2 + client-side \`age\` encryption); implemented in \`.github/workflows/mirror-reusable.yml\` (\`mirror-r2-bundle\`), wired in \`.github/workflows/mirror.yml\`, and documented in \`docs/MIRROR-HOST-VERIFICATION.adoc\`.
- **B / D332:** Ruled in comment 2 (Probot Settings \`1561\` confirmed uninstalled and \`.github/settings.yml\` deleted across the 95 active repos).
- **A1 (D16):** Full ruleset \`bypass_actors\` census complete (\`0\` in \`metadatastician\`, \`10\` in \`hyperpolymath\`: 4 active, 6 disabled) and converged in Section 1.
- **A2 (D14):** Full 24-repo ClusterFuzzLite census complete (\`10\` KEEP real Rust/Zig targets, \`14\` DELETE stubs + \`echidna\` stub target removed).
- **A3 (D24):** Full 48-repo Pages classification complete and converged (\`1\` KEEP real site \`metadatastician/knot-knot\`, \`33\` DELETE docs-only workflows, \`3\` banned \`jekyll*.yml\`, \`1\` \`standards\`, \`10\` already retired).
- **A4 (D97):** Full 357-repo transfer-candidate cost census across 6 tiers + follow-up ruling row **D355** posted in \`$REPORT_REL\`.
- **A5 (D298):** Precondition blast radius verified (\`0\` canon failures across 29 bypassed pushes; \`7\` secret-scanning failures on repos without secret scanning); \`?dry-run=true\` + live \`PUT\` reverting \`23868655\` and \`23868851\` to canon (\`deletion\` + \`non_fast_forward\`, \`bypass_actors: []\`) verified in Section 2.
- **A6 (D67) — pending:** Local \`RegistryCI.jl\` \`AutoMerge\` check passed for all 9 Marid Tier-1 packages (\`16/16\` at tag \`fd139f763a306a7e2eca60d96ea1a1a9f77c1e71\`); General registrations remain blocked, with all 4 blockers and verbatim guideline texts recorded in \`$REPORT_REL\`.
- **A7 (D330):** Closed in comment 2 (no GitLab copy of \`canonical-ums\` exists)."
else
  echo "  #1234 kept open until all pending convergence items finish (total=$TOTAL_PENDING: A1=$A1_PENDING, A5=$A5_PENDING, A2=$A2_PENDING, A3=$A3_PENDING, A6=$A6_PENDING, D332=$D332_PENDING)."
fi

echo ""
echo "Done. (DRY_RUN=$DRY_RUN)"
