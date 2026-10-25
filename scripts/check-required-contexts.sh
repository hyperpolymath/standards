#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-required-contexts.sh — is every required status context a name something
# can actually publish?
#
# WHY
# ---
# GitHub matches a required status check by *string equality* against the name it
# publishes for that check run. It never checks that the name is producible. So a
# rule requiring a name nothing publishes is an unsatisfiable requirement: every
# pull request stays BLOCKED, the CI board shows green, and nothing on the CI
# side names the cause. Observed in hyperpolymath/tropical-types#17 (the bare
# `Hypatia Neurosymbolic Analysis` required while the wrapper published
# `scan / Hypatia Neurosymbolic Analysis`; that repository now names the wrapper
# caller `hypatia`, so it publishes the prefixed name, and nothing requires the
# bare one), and catalogued for the wrapper case in
# docs/audits/audit-hypatia-pin-orphan-2026-05-27.adoc.
#
# LIMIT — READ THIS BEFORE ACTING ON A VERDICT
# --------------------------------------------
# A verdict here is derived from the repository's files. GitHub also generates
# workflows server-side (code scanning default setup and friends) and those
# publish check names nothing in the repository mentions. A name this script
# calls unsatisfiable may therefore be reporting on every commit. Confirm any
# such verdict against the check-run history of the default branch, and never
# delete a required context on this script's word alone.
#
# THE RULE IS TWO-DIMENSIONAL
# ---------------------------
# A name must be published, AND it must be published on the event that gates the
# merge (`pull_request` / `pull_request_target`). A name whose only publisher is
# a default-branch push/schedule workflow is producible and absent from every pull
# request: the PR waits forever for a check run that cannot happen. That is the
# PR-UNPRODUCIBLE verdict, and its repair is a trigger or a different required
# name — never a rename.
#
# Measured 2026-10-24 in hyperpolymath/standards: `scan / Hypatia Neurosymbolic
# Analysis` is required, and its only publisher (.github/workflows/hypatia-scan.yml,
# job `scan`) deliberately has no `pull_request` trigger because the PR-time scan
# runs inside fork-governance.yml. Every PR was therefore BLOCKED with all checks
# green; the only way through was the main-gate ruleset admin bypass
# (`RepositoryRole:5`, `bypass_mode: always`), and a bypass applies to the whole
# ruleset — so the other nineteen required checks were discarded with it.
#
# THE RULE THIS ENCODES
# ---------------------
#   * a plain job publishes its `name:` — or `<job id> (<matrix values>)` when it
#     has a matrix and no `name:`, or the job id alone otherwise;
#   * a job that calls a reusable workflow publishes
#         <caller display name or job id> / <inner job display name or job id>
#     and therefore can NEVER publish the reusable's bare inner name;
#   * a context bound to a third-party app integration (CodeQL scanning,
#     CodeRabbit, SonarCloud, …) is that app's to publish, not the repository's;
#   * a bare `*` is NOT a universal wildcard. A job whose display name is
#     `${{ matrix.runtime }}` degrades to `*` after templating, and one such job
#     made every required context look publishable — masking the very names this
#     gate exists to find. Only `<literal> / *` and `<literal> (*)` match.
#
# USAGE
#   scripts/check-required-contexts.sh [REPO_ROOT] [--json FILE] [--print-commands]
#                                      [--no-network] [--self-test]
#
# ENV
#   GITHUB_REPOSITORY         owner/repo (default: `gh repo view`)
#   GH_TOKEN / GITHUB_TOKEN   read-only credential; rulesets need no admin
#   REQUIRED_CONTEXTS_JSON    hermetic input: JSON array of
#                             {"context": "...", "app_id": 0, "source": "..."}
#                             (used by the fixture test, and for offline audits)
#   REUSABLE_CACHE_DIR        where fetched reusable files are cached; also the
#                             test seam that makes reusable resolution hermetic
#   REQUIRED_CONTEXTS_STRICT  1 = exit 1 on an unsatisfiable required context.
#                             Default 0 = ::warning, per the estate rule that a
#                             check which reds nobody yet is a warning, not a red.
#   APP_NAMED_CONTEXTS        space-separated extra context names published by
#                             GitHub apps rather than by a workflow
#
# EXIT  0 = every required context is satisfiable (or nothing is required);
#       1 = at least one required context cannot be published (strict mode), or
#           the repository could not be inspected at all.
set -uo pipefail

ROOT="."
JSON_OUT=""
PRINT_COMMANDS=0
OFFLINE=0
SELF_TEST=0
# A while-loop with real shifting: `shift` inside `for a in "$@"` cannot consume
# the *next* argument, which silently mis-binds an option value to the following
# flag. (Caught by the smoke run, not by the happy-path test.)
while [ $# -gt 0 ]; do
  case "$1" in
    --json)           JSON_OUT="${2:-}"; shift 2 ;;
    --print-commands) PRINT_COMMANDS=1; shift ;;
    --no-network)     OFFLINE=1; shift ;;
    --self-test)      SELF_TEST=1; shift ;;
    -h|--help)        sed -n '2,50p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    -*)               printf 'unknown option: %s\n' "$1" >&2; exit 2 ;;
    *)                ROOT="$1"; shift ;;
  esac
done

STRICT="${REQUIRED_CONTEXTS_STRICT:-0}"
CACHE_DIR="${REUSABLE_CACHE_DIR:-}"
APP_NAMED_DEFAULT="github-advanced-security Dependabot Dependabot Updates Code scanning results"
# shellcheck disable=SC2086  # deliberate word splitting: a space-separated list
APP_NAMED="${APP_NAMED_CONTEXTS:-$APP_NAMED_DEFAULT}"
GH_ACTIONS_APP=15368
RAW="https://raw.githubusercontent.com"
MERGE_BRANCH=""

say()  { printf '%s\n' "$*"; }
info() { printf '  %s\n' "$*"; }
note() { printf '::notice::%s\n' "$*"; }
warn() { printf '::warning::%s\n' "$*"; }
err()  { printf '::error::%s\n' "$*"; }

# --------------------------------------------------------------- derivation ---
# Emit "jobId<TAB>nameOrDash<TAB>usesOrDash<TAB>isMatrix" for one workflow file.
# Empty fields are written as "-": `read` with IFS=<tab> collapses runs of IFS
# whitespace, so an empty field would silently shift every later field.
jobs_of() {
  awk '
    /^jobs:[[:space:]]*$/ { injobs=1; next }
    /^[^[:space:]#]/      { injobs=0 }
    injobs && /^  [A-Za-z0-9_.-]+:[[:space:]]*$/ {
      if (job != "") printf "%s\t%s\t%s\t%s\n", job, (name==""?"-":name), (uses==""?"-":uses), (mat?"yes":"no")
      job=$1; sub(/:$/,"",job); name=""; uses=""; mat=0; next
    }
    injobs && /^    name:[[:space:]]*/ {
      v=$0; sub(/^[[:space:]]*name:[[:space:]]*/,"",v); gsub(/^"|"[[:space:]]*$/,"",v); name=v
    }
    injobs && /^    uses:[[:space:]]*/ {
      v=$0; sub(/^[[:space:]]*uses:[[:space:]]*/,"",v); sub(/[[:space:]]*#.*$/,"",v); uses=v
    }
    injobs && /^[[:space:]]+matrix:[[:space:]]*$/ { mat=1 }
    END { if (job != "") printf "%s\t%s\t%s\t%s\n", job, (name==""?"-":name), (uses==""?"-":uses), (mat?"yes":"no") }
  ' "$1"
}

fetch_reusable() { # owner/repo/workflow@ref -> path on stdout, or empty
  local spec="$1" path orr file ref url cache
  path="${spec%@*}"; ref="${spec##*@}"
  orr="${path%%/.github/workflows/*}"
  file=".github/workflows/${path##*/}"
  url="$RAW/$orr/$ref/$file"
  if [ -n "$CACHE_DIR" ]; then
    cache="$CACHE_DIR/$(printf '%s' "$spec" | tr -c 'A-Za-z0-9.' '_')"
    if [ -s "$cache" ]; then printf '%s\n' "$cache"; return 0; fi
  else
    cache="$(mktemp)"; TMP_FILES="${TMP_FILES:-} $cache"
  fi
  [ "$OFFLINE" = 1 ] && return 1
  if curl -fsSL --proto '=https' "$url" -o "$cache" 2>/dev/null; then
    printf '%s\n' "$cache"
  else
    rm -f "$cache"; return 1
  fi
}

# Which events a workflow runs on. A required context is matched per check run,
# so "can anything publish this name" is the wrong question on its own: a merge
# requirement is satisfiable only if something publishes the name on
# `pull_request` or an unrestricted push to a PR head. A name published solely
# by a default-branch push/schedule workflow is
# producible and permanently absent from every pull request — the PR sits
# "Expected — waiting for status to be reported" until someone bypasses, and a
# bypass on a ruleset applies to the whole ruleset, not to the one check.
#
# Measured 2026-10-24 in hyperpolymath/standards: `scan / Hypatia Neurosymbolic
# Analysis` is required, and its publisher (.github/workflows/hypatia-scan.yml)
# deliberately has no pull_request trigger — its header explains why (the
# PR-time scan runs inside fork-governance.yml, and running both uploads the
# same findings twice). Every PR was therefore BLOCKED with every check green,
# and the only way through was the ruleset's `always` admin bypass, which
# discards the other nineteen required checks in the same action.
#
# Emit one event name per line from workflow file $1. Recognises scalar and
# inline-list on: values, and block event keys indented by two spaces.
# No recognised events produces no output; unreadable files return awk's error
# status.
events_of() {
  awk '
    /^"?on"?[[:space:]]*:[[:space:]]*\[/ {
      v=$0; sub(/^[^[]*\[/,"",v); sub(/\].*$/,"",v)
      n=split(v,a,","); for(i=1;i<=n;i++){gsub(/[[:space:]"\047]/,"",a[i]); if(a[i]!="") print a[i]}
      inon=0; next
    }
    /^"?on"?[[:space:]]*:/ {
      v=$0; sub(/^[^:]*:/,"",v); sub(/#.*$/,"",v); gsub(/[[:space:]"\047]/,"",v)
      if (v == "") { inon=1 } else { print v; inon=0 }
      next
    }
    /^[^[:space:]#]/ { inon=0 }
    # Event keys sit at exactly two spaces. Anything deeper (branches:, cron:,
    # inputs:, description:) is configuration FOR that event, and emitting it
    # turns the event list into noise. The first version of this function
    # listed `description type required default` from the input schema of
    # every reusable, and could not answer "does this run on a PR?" at all.
    inon && /^  [A-Za-z_][A-Za-z0-9_-]*:/ {
      v=$0; sub(/^  /,"",v); sub(/:.*$/,"",v); print v
    }
  ' "$1"
}

# PR activity and path restrictions are checked without a PR diff; filtered
# pull_request publishers are conservatively unavailable as merge gates.
# pull_request_target retains its previous handling. Job conditions and PR
# branch filters are not evaluated.
pull_request_fires_while_open() { # FILE
  awk '
    function activities(v, n,a,i) {
      gsub(/[\[\]{},"\047]/," ",v)
      n=split(v,a,/[[:space:]]+/)
      for(i=1;i<=n;i++) if(a[i]!="" && a[i]!="closed") before_merge=1
    }
    {
      sub(/[[:space:]]*#.*$/,"")
      if ($0 ~ /^"?on"?[[:space:]]*:[[:space:]]*$/) { inon=1; next }
      if ($0 ~ /^[^[:space:]#]/) inon=0
      if (!inon) next
      if ($0 ~ /^  pull_request[[:space:]]*:/) {
        inpr=1; v=$0; sub(/^[^:]*:[[:space:]]*/,"",v)
      } else if (inpr && $0 ~ /^[[:space:]]*$/) next
      else if (inpr && $0 ~ /^    /) v=$0
      else { inpr=0; next }

      if (v ~ /(^|[,{[:space:]])paths(-ignore)?[[:space:]]*:/) filtered=1
      if (v ~ /(^|[,{[:space:]])types[[:space:]]*:/) {
        has_types=1; intypes=1
        sub(/^.*types[[:space:]]*:[[:space:]]*/,"",v)
        sub(/\].*$/,"]",v); activities(v)
      } else if (intypes && v ~ /^[[:space:]]*-/) {
        sub(/^[[:space:]]*-[[:space:]]*/,"",v); activities(v)
      } else intypes=0
    }
    END { exit(filtered || (has_types && !before_merge) ? 1 : 0) }
  ' "$1"
}

push_reaches_a_pr_head() { # FILE -> 0 for an unrestricted push publisher
  # Pushes on feature branches publish checks on a same-repository PR head.
  # Branch or path filters can exclude that head, so they are not unrestricted.
  awk '
    {
      sub(/[[:space:]]*#.*$/, "")
      if ($0 ~ /^"?on"?[[:space:]]*:[[:space:]]*$/) { inon=1; next }
      if ($0 ~ /^[^[:space:]#]/) inon=0
      if (!inon) next
      if ($0 ~ /^  push[[:space:]]*:/) {
        inpush=1; v=$0; sub(/^[^:]*:[[:space:]]*/, "", v)
      } else if (inpush && $0 ~ /^[[:space:]]*$/) next
      else if (inpush && $0 ~ /^    /) v=$0
      else { inpush=0; next }
      if (v ~ /(^|[,{[:space:]])(branches|paths)(-ignore)?[[:space:]]*:/) filtered=1
    }
    END { exit(filtered ? 1 : 0) }
  ' "$1"
}

runs_on_pr() { # FILE -> 0 if an available publisher can run before merge
  # Capture events before matching, avoiding SIGPIPE under pipefail.
  local ev
  ev="$(events_of "$1")" || return 1
  grep -qx 'pull_request_target' <<<"$ev" && return 0
  if grep -qx 'pull_request' <<<"$ev" && pull_request_fires_while_open "$1"; then
    return 0
  fi
  if grep -qx 'push' <<<"$ev" && push_reaches_a_pr_head "$1"; then
    return 0
  fi
  return 1
}

# Names ONE workflow file can publish. Split out of publishable_names so the
# PR-UNPRODUCIBLE report can ask the same question about a specific file ("which
# workflow publishes this name, and on what events?") without a second, subtly
# different traversal of the YAML.
# Local reusable paths are relative to ROOT. Remote reusables may be fetched
# and cached unless OFFLINE=1; missing or unfetchable reusables emit caller / *
# candidates. Matrix jobs also emit wildcard candidates. Returns 0 even when
# no names are derived; derivation errors are not propagated.
publishable_names_from() { # FILE -> one name per line
  local f="$1" id name uses mat stub iid iname local_path
  while IFS=$'\t' read -r id name uses mat; do
    [ -n "$id" ] || continue
    [ "$name" = "-" ] && name=""
    [ "$uses" = "-" ] && uses=""
    case "$uses" in
      ./.github/workflows/*|.github/workflows/*|'$/'*)
        local_path="${uses#\$/}"
        local_path="$ROOT/${local_path#./}"
        if [ -f "$local_path" ]; then
          while IFS=$'\t' read -r iid iname _u _m; do
            [ -n "$iid" ] || continue
            [ "$iname" = "-" ] && iname=""
            printf '%s / %s\n' "$id" "${iname:-$iid}"
            [ -n "$name" ] && printf '%s / %s\n' "$name" "${iname:-$iid}"
          done < <(jobs_of "$local_path")
        else
          printf '%s / *\n' "$id"
          [ -n "$name" ] && printf '%s / *\n' "$name"
        fi ;;
      *".github/workflows/"*"@"*)
        if stub="$(fetch_reusable "$uses")" && [ -s "$stub" ]; then
          while IFS=$'\t' read -r iid iname _u _m; do
            [ -n "$iid" ] || continue
            [ "$iname" = "-" ] && iname=""
            printf '%s / %s\n' "$id" "${iname:-$iid}"
            [ -n "$name" ] && printf '%s / %s\n' "$name" "${iname:-$iid}"
          done < <(jobs_of "$stub")
        else
          printf '%s / *\n' "$id"
          [ -n "$name" ] && printf '%s / *\n' "$name"
        fi ;;
      *)
        printf '%s\n' "${name:-$id}"
        if [ "$mat" = yes ]; then
          printf '%s (*)\n' "$id"
          [ -n "$name" ] && printf '%s\n' "$(printf '%s' "$name" | sed 's/\${{ *[^}]* *}}/*/g')"
        fi ;;
    esac
  done < <(jobs_of "$f")
  return 0   # see above: never let a false last-command status hide the output
}

publishable_names() { # [pr] -> one name per line; `pr` restricts to PR-capable workflows
  local f
  for f in "$ROOT"/.github/workflows/*.yml "$ROOT"/.github/workflows/*.yaml; do
    [ -f "$f" ] || continue
    if [ "${1:-}" = pr ] && ! runs_on_pr "$f"; then continue; fi
    publishable_names_from "$f"
  done
  return 0
}

# ------------------------------------------------------------ required set ----
required_json() { # emit {"context":..,"app_id":..,"source":..} objects
  if [ -n "${REQUIRED_CONTEXTS_JSON:-}" ]; then
    [ -f "$REQUIRED_CONTEXTS_JSON" ] || { err "REQUIRED_CONTEXTS_JSON not found: $REQUIRED_CONTEXTS_JSON"; return 1; }
    jq -c '.[] | {context: .context, app_id: (.app_id // 0), source: (.source // "file")}' "$REQUIRED_CONTEXTS_JSON" \
      || { err "REQUIRED_CONTEXTS_JSON is not a JSON array of {\"context\": ...} objects"; return 1; }
    return 0
  fi
  command -v gh >/dev/null 2>&1 || { note "gh not installed — cannot read branch rules"; return 1; }
  command -v jq >/dev/null 2>&1 || { note "jq not installed — cannot read branch rules"; return 1; }
  [ -n "${GITHUB_REPOSITORY:-}" ] || GITHUB_REPOSITORY="$(gh repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null)"
  [ -n "${GITHUB_REPOSITORY:-}" ] || { note "no GITHUB_REPOSITORY and gh cannot resolve the current repo"; return 1; }

  local repo="$GITHUB_REPOSITORY" br id list body
  # An API failure returns an object ({"message": ...}), not an array. Without
  # this check a rate-limited or unauthorised run would emit no contexts and
  # read as "nothing is required" — a false clean bill of health on exactly the
  # gate that exists to catch unsatisfiable requirements.
  list="$(gh api "repos/$repo/rulesets" 2>/dev/null || true)"
  if ! printf '%s' "$list" | jq -e 'type == "array"' >/dev/null 2>&1; then
    note "could not read branch rulesets for $repo (API error, rate limit, or missing permission) — nothing inspected" >&2
    return 1
  fi
  br="$(gh api "repos/$repo" 2>/dev/null | jq -r 'if type=="object" then (.default_branch // "main") else "main" end' 2>/dev/null || echo main)"
  MERGE_BRANCH="$br"
  for id in $(printf '%s' "$list" | jq -r '.[]? | select(.target=="branch") | .id'); do
    body="$(gh api "repos/$repo/rulesets/$id" 2>/dev/null || true)"
    printf '%s' "$body" | jq -c '
      if type == "object" and .enforcement != "disabled" then
        .rules[]? | select(.type=="required_status_checks") | .parameters.required_status_checks[]?
        | {context: (.context|tostring), app_id: (.integration_id // 0), source: "ruleset"}
      else empty end' 2>/dev/null
  done
  body="$(gh api "repos/$repo/branches/$br/protection/required_status_checks" 2>/dev/null || true)"
  if printf '%s' "$body" | jq -e 'type == "object" and has("contexts")' >/dev/null 2>&1; then
    note "legacy branch protection readable: enforcement_level=$(printf '%s' "$body" | jq -r '.enforcement_level // "unknown"')" >&2
    printf '%s' "$body" | jq -c '.contexts[]? | {context: ., app_id: 0, source: "branch-protection"}' 2>/dev/null
    printf '%s' "$body" | jq -c '.checks[]? | {context: (.context|tostring), app_id: (.app_id // 0), source: "branch-protection"}' 2>/dev/null
  else
    note "legacy branch protection unreadable with this credential (needs Administration: read) — rulesets only" >&2
  fi
  return 0
}

# Rule types whose satisfaction depends on DATA being uploaded by something,
# rather than on a workflow existing. GitHub evaluates these itself, so they
# never appear as a status context and the producibility check above cannot see
# them — which is exactly how they go unnoticed while blocking every pull
# request. Measured 2026-10-24: `code_coverage` (minimum_coverage 80) and
# `code_quality` (severity all) appeared on the main gate of a repository whose
# only coverage job reports SKIPPED and which uploads no code-quality results.
# Neither rule is in the repository's own config/rulesets/*.json; they arrived
# through a UI edit.
#
# The repair differs per type and is not a rename: either upload the data, or
# remove the rule. Keeping the rule and hoping is what makes every PR
# undecidable, and an undecidable gate is a gate people bypass.
DATA_RULES='code_coverage code_quality'
# Locate a workflow in the tree that uploads data for a data-dependent rule, and
# echo its path; echo nothing when there is none. This changes the WORDING of the
# finding only, never whether it is a finding: a rule with an uploader in the
# tree is still unsatisfiable until the repository setting it needs is on, so it
# must keep failing rather than becoming a warning. What it stops is the audit
# asserting that a repository "never uploads" while a workflow in that repository
# does the uploading — an error message that is confidently wrong sends the
# reader looking for data that is already being produced.
# `code_quality` deliberately has no producer here: GitHub runs that analysis
# itself once the feature is enabled, so no file can supply it.
data_rule_producer() {
  local match
  case "${1:-}" in
    code_coverage)
      match="$(grep -rl --include='*.yml' --include='*.yaml' \
        'actions/upload-code-coverage' "$ROOT/.github/workflows" 2>/dev/null | head -1)"
      [ -n "$match" ] || return 0
      # Strip the scan root by expansion, not by regex: ROOT is usually a temp
      # path, and its dots and slashes would be live regex metacharacters.
      printf '%s\n' "${match#"$ROOT"/}"
      ;;
    *) return 0 ;;
  esac
}

# Print evidence that a data-dependent rule is demonstrably fed, or nothing when
# there is none. Empty output means the finding stands.
#
# This is the difference between *unverified* and *unsatisfiable*, and it is the
# whole point of the check. A rule whose data pipeline has actually run is not a
# rule that blocks every pull request, and reporting it as one is its own defect:
# a permanent alarm is how people learn to ignore the alarm.
#
# The evidence is deliberately an OUTCOME, never an intention. For coverage that
# means a completed run that SUCCEEDED. A workflow merely existing proves
# nothing — every run on this branch failed with HTTP 404 while Code Quality was
# off, and a check that accepted file presence would have called that satisfied
# and let the block through. Returns 1 when a lookup fails, so an API problem
# becomes an inspection failure instead of a silent pass.
data_rule_evidence() {
  local type="${1:-}" repo="${GITHUB_REPOSITORY:-}" producer body
  [ -n "$repo" ] || return 1
  case "$type" in
    code_coverage)
      producer="$(data_rule_producer code_coverage)"
      [ -n "$producer" ] || return 0
      # Address the workflow by file name; inspect several successful runs so a
      # recent fork run cannot hide an earlier run from this repository.
      body="$(gh api "repos/$repo/actions/workflows/${producer##*/}/runs?status=success&per_page=30" 2>/dev/null)" || return 1
      [ -n "$body" ] || return 1
      printf '%s' "$body" | jq -es '
        length == 1 and (.[0] | type == "object" and
          (.workflow_runs | type == "array"))
      ' >/dev/null 2>&1 || return 1
      printf '%s' "$body" | jq -r --arg repo "$repo" '
        [.workflow_runs[] | select((.head_repository.full_name // "") == $repo)][0] // empty
        | "a successful \(.path // "coverage") run on \(.head_branch // "?") (\((.head_sha // "")[0:12]))"' \
        2>/dev/null
      ;;
    code_quality)
      # GitHub runs this analysis itself once the feature is enabled, so there is
      # no uploader in the tree to inspect; the workflow it registers is the
      # signal that the feature exists for this repository.
      body="$(gh api "repos/$repo/actions/workflows?per_page=100" --paginate --slurp 2>/dev/null)" || return 1
      [ -n "$body" ] || return 1
      # --slurp groups all pages. Reject malformed or incomplete catalogues,
      # including a successful response whose pagination stopped too early.
      printf '%s' "$body" | jq -es '
        length == 1 and (.[0] | type == "array" and length > 0 and
          all(.[]; type == "object" and (.total_count | type == "number") and
            (.workflows | type == "array")) and
          (.[0].total_count as $total |
            all(.[]; .total_count == $total) and
            ([.[].workflows[]] | length == $total)))
      ' >/dev/null 2>&1 || return 1
      printf '%s' "$body" | jq -r '
        [.[].workflows[]? | select(.path == "dynamic/code-quality")]
        | .[0] // empty
        | "the GitHub-generated workflow \(.path) (\(.name))"' 2>/dev/null
      ;;
    *) return 0 ;;
  esac
}

# Emit tab-separated finding name, app id 0 and UNSATISFIED-RULE for each
# DATA_RULES match in an active branch ruleset whose ref filters match MERGE_BRANCH.
# Presence alone is flagged; uploaded coverage or quality data is not checked.
# Uses GITHUB_REPOSITORY, resolving and setting it with gh when unset.
# Reuses MERGE_BRANCH resolved by required_json. Returns 0 with no findings for
# file fixtures; unavailable inspection or a ruleset read/parse failure returns 1.
# Output may be partial on failure; run_audit discards those partial findings.
data_rules_json() {
  [ -f "${REQUIRED_CONTEXTS_JSON:-}" ] && return 0   # offline fixtures: none
  command -v gh >/dev/null 2>&1 && command -v jq >/dev/null 2>&1 || return 1
  # Resolve the repo the same way required_json does. Returning early on an
  # unset GITHUB_REPOSITORY made this check silently report nothing in exactly
  # the local, hand-run case where someone is trying to find out why a green PR
  # will not merge.
  [ -n "${GITHUB_REPOSITORY:-}" ] || GITHUB_REPOSITORY="$(gh repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null)"
  [ -n "${GITHUB_REPOSITORY:-}" ] || return 1
  local repo="$GITHUB_REPOSITORY" id body
  local list ev_cov ev_qual
  [ -n "$MERGE_BRANCH" ] || return 1
  # Resolve the evidence once per rule type, not once per ruleset: the answer
  # cannot differ between rulesets, and these are API calls.
  ev_cov="$(data_rule_evidence code_coverage)" || return 1
  ev_qual="$(data_rule_evidence code_quality)" || return 1
  list="$(gh api "repos/$repo/rulesets" 2>/dev/null)" || return 1
  printf '%s' "$list" | jq -es '
    length == 1 and (.[0] | type == "array" and all(.[]; type == "object" and
      (.target | type == "string") and (.id | type == "number")))
  ' >/dev/null 2>&1 || return 1
  for id in $(printf '%s' "$list" | jq -r '.[]? | select(.target=="branch") | .id'); do
    body="$(gh api "repos/$repo/rulesets/$id" 2>/dev/null)" || return 1
    printf '%s' "$body" | jq -es '
      length == 1 and (.[0] | type == "object" and
      (.enforcement | IN("active", "evaluate", "disabled")) and
      (.rules | type == "array" and all(.[]; type == "object" and (.type | type == "string"))) and
      (.conditions.ref_name | type == "object" and
        (.include | type == "array" and all(.[]; type == "string")) and
        (.exclude | type == "array" and all(.[]; type == "string"))))
    ' >/dev/null 2>&1 || return 1
    printf '%s' "$body" | jq -r --arg types "$DATA_RULES" --arg id "$id" --arg branch "$MERGE_BRANCH" \
      --arg ev_cov "$ev_cov" --arg ev_qual "$ev_qual" '
      # GitHub ref filters use pathname globs: * and ? do not cross /.
      def matches_ref:
        if . == "~ALL" or . == "~DEFAULT_BRANCH" then true
        else
          ([scan("\\*\\*/|\\*\\*|\\*|\\?|\\[[^]]+\\]|.") |
            if . == "**/" then "(.*/)?"
            elif . == "**" then ".*"
            elif . == "*" then "[^/]*"
            elif . == "?" then "[^/]"
            elif startswith("[") then sub("^\\[!"; "[^")
            else gsub("(?<c>[.()+^$|{}\\\\])"; "\\\(.c)") end] | join("")) as $pattern
          | ("refs/heads/" + $branch) | test("^" + $pattern + "$")
        end;
      if .enforcement == "active" then
        select(any(.conditions.ref_name.include[]; matches_ref))
        | select(all(.conditions.ref_name.exclude[]; matches_ref | not))
        | .rules[]? | select(.type as $t | ($types | split(" ") | index($t)))
        # A rule with evidence of real data keeps its row, because a reader of
        # the report deserves to see what the audit found and accepted. It is
        # just no longer a finding: the verdict carries the claim, not the
        # absence of a row.
        | (if .type == "code_coverage" then $ev_cov
           elif .type == "code_quality" then $ev_qual
           else "" end) as $ev
        | "rule:\(.type) in ruleset \($id)\t0\t" +
          (if $ev != "" then "VERIFIED-RULE\t\($ev)" else "UNSATISFIED-RULE" end)
      else empty end' 2>/dev/null || return 1
  done
  return 0
}

# ------------------------------------------------------------------ audit -----
# Print the required-context audit using workflows beneath ROOT; if JSON_OUT is
# set, replace any existing report and attempt to write its JSON summary.
# Returns 1 for UNSATISFIABLE, PR-UNPRODUCIBLE or UNSATISFIED-RULE findings
# when STRICT=1, otherwise 0. A required_json or data_rules_json failure returns
# 1 regardless of STRICT and marks the JSON report inspected:false.
# JSON write failures do not affect the return status. Data-rule findings appear
# in both results and the JSON unsatisfiable count.
run_audit() {
  local pubb pubb_pr required ROWS unsatisfiable=0 line ctx app verdict inspected=1
  pubb="$(mktemp)"; pubb_pr="$(mktemp)"; required="$(mktemp)"; ROWS="$(mktemp)"
  publishable_names | sort -u >"$pubb"
  publishable_names pr | sort -u >"$pubb_pr"
  # Never let a failed inspection leave a stale or empty successful report.
  [ -z "$JSON_OUT" ] || rm -f "$JSON_OUT"
  # Data-dependent rules are findings in their own right; they are appended to
  # ROWS before the context loop so they appear in the same report, the same
  # JSON and the same allow-list reconciliation. A context name that cannot be
  # produced and a rule whose data is never uploaded fail the same way: every
  # pull request becomes undecidable.
  if ! required_json >"$required"; then
    err "required contexts not inspected — could not read branch rules"
    inspected=0
    : >"$required"  # discard any partial results from the failed inspection
  elif ! data_rules_json >>"$ROWS"; then
    err "data-dependent rules not inspected — could not read branch rules"
    inspected=0
    : >"$ROWS"  # discard any partial findings from the failed inspection
  fi

  say ""
  say "== publishable context names ($(wc -l <"$pubb" | tr -d ' '); $(wc -l <"$pubb_pr" | tr -d ' ') on pull_request)"
  sed 's/^/  /' "$pubb"
  say ""
  say "== required contexts vs publishable names"
  if [ ! -s "$required" ]; then
    if [ "$inspected" = 1 ]; then info "no required status contexts found"; fi
  fi
  while IFS= read -r line; do
    [ -n "$line" ] || continue
    ctx="$(printf '%s' "$line" | jq -r '.context')"
    app="$(printf '%s' "$line" | jq -r '.app_id')"
    local src; src="$(printf '%s' "$line" | jq -r '.source')"
    if [ "$app" != 0 ] && [ "$app" != "$GH_ACTIONS_APP" ]; then
      verdict=app-owned
    elif awk -v c="$ctx" '
           { if ($0 == c) { ok=1; exit }        # exact name
             n=$0; gsub(/\*/,".*",n)             # matrix/wrapper wildcard form
             if (n ~ /\*/ && n != ".*" && c ~ ("^" n "$")) ok=1 }
           END { exit(ok?0:1) }' "$pubb_pr"; then
      # Publishable AND publishable on the event that gates a merge.
      verdict=producible
    elif awk -v c="$ctx" '
           { if ($0 == c) { ok=1; exit }
             n=$0; gsub(/\*/,".*",n)
             if (n ~ /\*/ && n != ".*" && c ~ ("^" n "$")) ok=1 }
           END { exit(ok?0:1) }' "$pubb"; then
      # The name exists, but no publisher is reliably available before merge.
      # This is a distinct verdict and a distinct repair: the name is not wrong,
      # the TRIGGER is. Rolling it in with UNSATISFIABLE sends someone to rename
      # a job that was never the problem.
      verdict=PR-UNPRODUCIBLE
    else
      verdict=UNSATISFIABLE
    fi
    printf '%s\t%s\t%s\n' "$ctx" "$app" "$verdict" >>"$ROWS"
    case "$verdict" in
      UNSATISFIABLE)
        unsatisfiable=$((unsatisfiable+1))
        if [ "$STRICT" = 1 ]; then
          err "required context '$ctx' ($src) is not producible by any workflow here, is not bound to an app integration, and no job publishes a matching name (candidate only: GitHub-generated workflows publish checks no file mentions — confirm against the check-run history before changing the rule)"
        else
          warn "required context '$ctx' ($src) is not producible by any workflow here and is not bound to an app integration — unsatisfiable as written (candidate only: GitHub-generated workflows publish checks no file mentions — confirm against the check-run history before changing the rule)"
        fi
        if [ "$PRINT_COMMANDS" = 1 ]; then
          cat <<EOS
    # Remediation — require a name this repository publishes, or publish this one:
    #   a) replace the requirement with a producible name (list above), or
    #   b) implement the check in-repo and publish '$ctx' from a plain job
    #      (a reusable-wrapper caller cannot: it publishes '<caller> / <inner job>').
    # FIRST, though: confirm the name never reports. Read the check-run history of
    # the default branch — GitHub-generated workflows publish checks that no file
    # in this repository mentions, and five such candidates were refuted this way
    # on 2026-09-20 (docs/audits/audit-ci-context-producibility-2026-09-20.adoc).
EOS
        fi ;;
      PR-UNPRODUCIBLE)
        unsatisfiable=$((unsatisfiable+1))
        local owners wf
        owners=""
        for wf in "$ROOT"/.github/workflows/*.yml "$ROOT"/.github/workflows/*.yaml; do
          [ -f "$wf" ] || continue
          runs_on_pr "$wf" && continue
          publishable_names_from "$wf" 2>/dev/null | awk -v c="$ctx" '
            { if ($0 == c) f=1
              n=$0; gsub(/\*/,".*",n)
              if (n ~ /\*/ && n != ".*" && c ~ ("^" n "$")) f=1 }
            END { exit(f?0:1) }' || continue
          owners="${owners}$(basename "$wf") [$(events_of "$wf" | paste -sd, -)] "
        done
        owners="${owners% }"   # the loop separates with a trailing space
        if [ "$STRICT" = 1 ]; then
          err "required context '$ctx' ($src) has no publisher reliably available before merge (${owners:-see the publishable list}) — a pull request can wait indefinitely, and a ruleset bypass discards every other required check too"
        else
          warn "required context '$ctx' ($src) has no publisher reliably available before merge (${owners:-see the publishable list}) — unsatisfiable for a merge rule; pull requests can wait indefinitely"
        fi
        if [ "$PRINT_COMMANDS" = 1 ]; then
          cat <<EOS
    # Remediation — pick ONE:
    #   a) give the publisher an unfiltered pre-merge pull_request trigger, if running it on
    #      PRs is wanted (check first that it does not duplicate an existing
    #      PR-time run: on 2026-10-24 hypatia-scan.yml omits pull_request
    #      precisely because fork-governance.yml already scans on PRs), or
    #   b) require the context the PR-time publisher actually emits
    #      (see the 'on pull_request' list above), or
    #   c) drop the requirement if the check only makes sense post-merge.
EOS
        fi ;;
      app-owned)   info "app-owned ($src): $ctx" ;;
      *)           info "$verdict ($src): $ctx" ;;
    esac
  done <"$required"

  # Count rule findings before serializing the total into the JSON report.
  local n_rule n_verified
  n_rule="$(awk -F'\t' '$3=="UNSATISFIED-RULE"' "$ROWS" 2>/dev/null | wc -l | tr -d ' ')"
  case "$n_rule" in ''|*[!0-9]*) n_rule=0 ;; esac
  # A rule that is demonstrably fed is reported, not hidden. Silence would make
  # the audit unable to answer "was this rule considered at all?" — the same
  # question that goes unanswered when a check quietly never runs.
  n_verified="$(awk -F'\t' '$3=="VERIFIED-RULE"' "$ROWS" 2>/dev/null | wc -l | tr -d ' ')"
  case "$n_verified" in ''|*[!0-9]*) n_verified=0 ;; esac
  unsatisfiable=$((unsatisfiable + n_rule))
  if [ -n "$JSON_OUT" ]; then
    jq -Rs --arg repo "${GITHUB_REPOSITORY:-$ROOT}" --argjson n "$unsatisfiable" \
       --argjson inspected "$inspected" --rawfile _ /dev/null '
      {repository: $repo, inspected: ($inspected == 1), unsatisfiable: $n,
       results: (split("\n") | map(select(length > 0) | split("\t")
                 | {context: .[0], app_id: (.[1]|tonumber), verdict: .[2]}))}' "$ROWS" >"$JSON_OUT" 2>/dev/null || true
    info "json report written to $JSON_OUT"
  fi
  # Data-rule findings are appended to ROWS directly, so they never pass through
  # the per-context case above. Without counting them here, a strict run whose
  # ONLY finding was a data-dependent rule returned 0 — the gate would print the
  # reason a pull request cannot merge and then declare success, which is the
  # defect this script exists to catch, committed by the script itself.
  if [ "$n_verified" -gt 0 ]; then
    while IFS=$'\t' read -r r _ _ ev; do
      [ -n "$r" ] || continue
      info "satisfied $r — $ev"
    done <<EOF
$(awk -F'\t' '$3=="VERIFIED-RULE"' "$ROWS" 2>/dev/null)
EOF
  fi
  if [ "$n_rule" -gt 0 ]; then
    while IFS= read -r r; do
      [ -n "$r" ] || continue
      # `$r` is "rule:<type> in ruleset <id>"; the repair depends on the type,
      # and so does the sentence. All three branches are findings.
      local rtype producer msg
      rtype="$(printf '%s' "$r" | sed 's/^rule:\([^ ]*\) .*/\1/')"
      producer="$(data_rule_producer "$rtype")"
      if [ "$rtype" = code_quality ]; then
        msg="required $r needs Code Quality enabled for this repository — a repository setting, and no file in the tree can supply it; every pull request waits for a result that cannot exist"
      elif [ -n "$producer" ]; then
        msg="required $r has an uploader in the tree ($producer), but that upload cannot succeed until Code Quality is enabled for this repository; every pull request waits for data that is uploaded and rejected"
      else
        msg="required $r needs data this repository never uploads — every pull request waits for a result that cannot exist; upload the data, or remove the rule"
      fi
      if [ "$STRICT" = 1 ]; then
        err "$msg"
      else
        warn "$msg"
      fi
    done <<EOF
$(awk -F'\t' '$3=="UNSATISFIED-RULE" {print $1}' "$ROWS" 2>/dev/null)
EOF
  fi

  rm -f "$pubb" "$pubb_pr" "$required" "$ROWS"
  [ "$inspected" = 1 ] || return 1
  [ "$unsatisfiable" -gt 0 ] && [ "$STRICT" = 1 ] && return 1
  return 0
}

self_test() {
  local work pass=0 fail=0
  work="$(mktemp -d)"
  mkdir -p "$work/repo/.github/workflows" "$work/cache"
  cat >"$work/repo/.github/workflows/hypatia-scan.yml" <<'YML'
name: Hypatia Security Scan
on: [pull_request]
jobs:
  hypatia:
    uses: hyperpolymath/standards/.github/workflows/hypatia-scan-reusable.yml@cafebabe
    secrets: inherit
YML
  cat >"$work/repo/.github/workflows/other.yml" <<'YML'
name: Other
on: [pull_request]
jobs:
  lake-build:
    runs-on: ubuntu-latest
    steps: [{run: "true"}]
  analyze:
    strategy:
      matrix:
        language: [actions, python]
    runs-on: ubuntu-latest
    steps: [{run: "true"}]
YML
  cat >"$work/cache/hyperpolymath_standards_.github_workflows_hypatia-scan-reusable.yml_cafebabe" <<'YML'
name: Hypatia Reusable Scan
on: [workflow_call]
jobs:
  scan:
    name: Hypatia Neurosymbolic Analysis
    runs-on: ubuntu-latest
YML
  check_says() { # label contexts-json expected-text — exit code alone cannot
                 # tell PR-UNPRODUCIBLE from UNSATISFIABLE: both exit 0 advisory
                 # and both exit 1 strict, so a classification regression would
                 # pass every other case in this file.
    local label="$1" file="$2" want="$3" out
    out="$(REUSABLE_CACHE_DIR="$work/cache" REQUIRED_CONTEXTS_JSON="$file" \
             REQUIRED_CONTEXTS_STRICT=0 GITHUB_REPOSITORY=example/repo \
             bash "$0" "$work/repo" --no-network 2>&1)"
    if printf '%s' "$out" | grep -qF "$want"; then pass=$((pass+1)); say "PASS $label"
    else fail=$((fail+1)); say "FAIL $label — output does not mention: $want"; say "$out" | tail -4; fi
  }

  check() { # label expected-exit contexts-json strict [expected-verdict]
    local label="$1" want="$2" file="$3" strict="${4:-0}" verdict="${5:-}" out status
    out="$(REUSABLE_CACHE_DIR="$work/cache" REQUIRED_CONTEXTS_JSON="$file" \
             REQUIRED_CONTEXTS_STRICT="$strict" GITHUB_REPOSITORY=example/repo \
             bash "$0" "$work/repo" --no-network --json "$work/report.json" 2>&1)"; status=$?
    if [ "$status" != "$want" ]; then
      fail=$((fail+1)); say "FAIL $label — expected exit $want, got $status"; say "$out" | head -6
    elif ! jq -e '.inspected == true' "$work/report.json" >/dev/null 2>&1; then
      fail=$((fail+1)); say "FAIL $label — expected inspected:true"
    elif [ -n "$verdict" ] && ! jq -e --arg verdict "$verdict" \
      '(.results | length) > 0 and all(.results[]; .verdict == $verdict)' "$work/report.json" >/dev/null 2>&1; then
      fail=$((fail+1)); say "FAIL $label — expected verdict $verdict"; cat "$work/report.json"
    else pass=$((pass+1)); say "PASS $label"; fi
  }
  printf '[{"context":"hypatia / Hypatia Neurosymbolic Analysis","app_id":15368,"source":"ruleset"},{"context":"lake-build","app_id":15368,"source":"ruleset"},{"context":"analyze (actions, python)","app_id":15368,"source":"ruleset"},{"context":"CodeRabbit","app_id":9999,"source":"ruleset"}]' >"$work/ok.json"
  printf '[{"context":"Hypatia Neurosymbolic Analysis","app_id":15368,"source":"ruleset"}]' >"$work/bare.json"
  printf '[{"context":"Retired Check","app_id":0,"source":"ruleset"}]' >"$work/phantom.json"
  printf '[{"context":"lake","app_id":15368,"source":"ruleset"}]' >"$work/substring.json"
  check "publishes-what-is-required" 0 "$work/ok.json"
  check "bare-reusable-name-is-unsatisfiable-advisory" 0 "$work/bare.json" 0
  check "bare-reusable-name-is-unsatisfiable-strict" 1 "$work/bare.json" 1
  check "unknown-check-is-unsatisfiable-strict" 1 "$work/phantom.json" 1
  check "prefix-of-a-real-name-is-not-a-match" 1 "$work/substring.json" 1
  printf '[]' >"$work/empty.json"
  check "inspected-empty-required-set-is-a-pass" 0 "$work/empty.json" 1

  # --- the name exists, but nothing that publishes it runs on a pull request --
  #
  # This is the 2026-10-24 shape in hyperpolymath/standards: a required context
  # whose only publisher is a push/schedule workflow. It is NOT the same defect
  # as an unproducible name and must not share its verdict or its repair — the
  # name is right, the trigger is missing.
  cat >"$work/repo/.github/workflows/nightly.yml" <<'YML'
name: Nightly
on:
  push:
    branches: [main]
  schedule:
    - cron: "0 3 * * *"
  workflow_dispatch:
jobs:
  audit:
    name: Nightly Audit
    runs-on: ubuntu-latest
    steps: [{run: "true"}]
YML
  printf '[{"context":"Nightly Audit","app_id":15368,"source":"ruleset"}]' >"$work/nightly.json"
  check "publishable-only-off-pull_request-advisory" 0 "$work/nightly.json" 0 PR-UNPRODUCIBLE
  check "publishable-only-off-pull_request-strict" 1 "$work/nightly.json" 1 PR-UNPRODUCIBLE

  check_says "classified PR-UNPRODUCIBLE, not UNSATISFIABLE" "$work/nightly.json" \
    "has no publisher reliably available before merge"

  pr_trigger_check() { # label expected-verdict trigger-yaml
    local want=0
    [ "$2" = producible ] || want=1
    printf '%s\n' "$3" >"$work/repo/.github/workflows/pr-audit.yml"
    cat >>"$work/repo/.github/workflows/pr-audit.yml" <<'YML'
jobs:
  audit:
    name: Nightly Audit
    runs-on: ubuntu-latest
YML
    check "$1" "$want" "$work/nightly.json" 1 "$2"
  }
  pr_trigger_check closed-only-inline PR-UNPRODUCIBLE 'on:
  pull_request:
    types: [closed]'
  pr_trigger_check closed-only-block PR-UNPRODUCIBLE 'on:
  pull_request:
    types:
      - closed'
  pr_trigger_check closed-only-flow-config PR-UNPRODUCIBLE 'on:
  pull_request: {types: [closed]}'
  pr_trigger_check pre-merge-inline producible 'on:
  pull_request:
    types: [opened, synchronize, closed]'
  pr_trigger_check pre-merge-block producible 'on:
  pull_request:
    types:
      - closed
      - opened'
  pr_trigger_check paths-filtered PR-UNPRODUCIBLE 'on:
  pull_request:
    paths: ["src/**"]'
  pr_trigger_check paths-ignore-filtered PR-UNPRODUCIBLE 'on:
  pull_request:
    paths-ignore:
      - "docs/**"'
  pr_trigger_check paths-flow-config PR-UNPRODUCIBLE 'on:
  pull_request: {paths: ["src/**"], types: [opened]}'
  pr_trigger_check push-filter-does-not-restrict-pr producible 'on:
  push:
    paths: ["src/**"]
  pull_request:'
  pr_trigger_check pr-filter-does-not-restrict-target producible 'on:
  pull_request:
    types: [closed]
    paths: ["src/**"]
  pull_request_target:'
  pr_trigger_check commented-on-header producible 'on: # event configuration follows
  pull_request:'
  pr_trigger_check commented-closed-trigger PR-UNPRODUCIBLE 'on: # event configuration follows
  pull_request:
    types: [closed]'
  pr_trigger_check unrestricted-push-scalar producible 'on: push'
  pr_trigger_check unrestricted-push-quoted-scalar producible 'on: "push"'
  pr_trigger_check unrestricted-push-list producible 'on: [push, workflow_dispatch]'
  pr_trigger_check unrestricted-push-block producible 'on:
  push:'
  pr_trigger_check unrestricted-push-fallback producible 'on:
  pull_request:
    types: [closed]
  push:'
  pr_trigger_check restricted-push-inline PR-UNPRODUCIBLE 'on:
  push:
    branches: [main]'
  pr_trigger_check restricted-push-block PR-UNPRODUCIBLE 'on:
  push:
    branches:
      - main'
  pr_trigger_check restricted-push-flow PR-UNPRODUCIBLE 'on:
  push: {branches: [main]}'
  pr_trigger_check restricted-push-ignore PR-UNPRODUCIBLE 'on:
  push:
    branches-ignore: ["feature/**"]'
  pr_trigger_check filtered-push PR-UNPRODUCIBLE 'on:
  push:
    paths: ["src/**"]'

  # The same name, once something publishes it on pull_request — including the
  # inline `on: [...]` spelling and pull_request_target, so a parser that only
  # understands one form cannot pass this by accident.
  cat >"$work/repo/.github/workflows/pr-audit.yml" <<'YML'
name: PR Audit
on: [pull_request_target]
jobs:
  audit:
    name: Nightly Audit
    runs-on: ubuntu-latest
    steps: [{run: "true"}]
YML
  check "same-name-on-a-pr-workflow-is-producible" 0 "$work/nightly.json" 1 producible

  # `types: [closed]` fires AFTER the merge, so it cannot gate one — the event
  # alone is not enough, which is easy to regress.
  cat >"$work/repo/.github/workflows/after-merge.yml" <<'YML'
name: After merge
on:
  pull_request:
    types: [closed]
jobs:
  audit:
    name: Post Merge Audit
    runs-on: ubuntu-latest
    steps: [{run: "true"}]
YML
  printf '[{"context":"Post Merge Audit","app_id":15368,"source":"ruleset"}]' >"$work/postmerge.json"
  check "pull_request types:[closed] does not gate a merge" 1 "$work/postmerge.json" 1

  # Resolve all supported local forms and preserve both caller names. The
  # missing inner name must remain unsatisfiable, rather than caller / *.
  cat >"$work/repo/.github/workflows/local-reusable.yml" <<'YML'
on: workflow_call
jobs:
  inner:
    name: Local Inner
    runs-on: ubuntu-latest
YML
  printf '[{"context":"local / Local Inner"},{"context":"Local Caller / Local Inner"}]' >"$work/local.json"
  printf '[{"context":"local / Missing Inner"}]' >"$work/missing-inner.json"
  local ref
  for ref in './.github/workflows/local-reusable.yml' '.github/workflows/local-reusable.yml' '$/.github/workflows/local-reusable.yml'; do
    cat >"$work/repo/.github/workflows/local-caller.yml" <<YML
on: pull_request
jobs:
  local:
    name: Local Caller
    uses: $ref
YML
    check "local-reference-$ref" 0 "$work/local.json" 1 producible
    check "local-reference-no-wildcard-$ref" 1 "$work/missing-inner.json" 1 UNSATISFIABLE
  done

  # An unavailable rules API must not turn into an empty successful report,
  # even when a report from an earlier run exists at the requested path.
  mkdir -p "$work/bin"
  cat >"$work/bin/gh" <<'SH'
#!/usr/bin/env bash
printf '{"message":"fixture inspection failure"}\n'
exit 1
SH
  chmod +x "$work/bin/gh"
  local inspection_status inspection_out
  inspection_out="$(unset REQUIRED_CONTEXTS_JSON
    PATH="$work/bin:$PATH" GITHUB_REPOSITORY=example/repo \
      bash "$0" "$work/repo" --no-network --json "$work/report.json" 2>&1)"; inspection_status=$?
  if [ "$inspection_status" = 1 ] && jq -e '.inspected == false and .results == []' "$work/report.json" >/dev/null && \
    grep -q 'not inspected' <<<"$inspection_out"; then
    pass=$((pass+1)); say "PASS inspection-failure-is-not-a-clean-report"
  else
    fail=$((fail+1)); say "FAIL inspection-failure-is-not-a-clean-report"
  fi
  rm -rf "$work"
  say ""
  say "self-test: $pass passed, $fail failed"
  [ "$fail" -eq 0 ]
}

if [ "$SELF_TEST" = 1 ]; then self_test; exit $?; fi
run_audit
