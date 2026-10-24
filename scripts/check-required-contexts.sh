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
# a push/schedule workflow is producible and permanently absent from every pull
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
# `pull_request`. A name published solely by a push/schedule workflow is
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
    /^"?on"?[[:space:]]*:[[:space:]]*[^[:space:]]/ {
      v=$0; sub(/^[^:]*:[[:space:]]*/,"",v); sub(/[[:space:]]*#.*$/,"",v)
      gsub(/[[:space:]"\047]/,"",v); if(v!="") print v
      inon=0; next
    }
    /^"?on"?[[:space:]]*:[[:space:]]*$/ { inon=1; next }
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

# Accepts pull_request_target as before. For pull_request, a closed-only
# trigger cannot gate a merge, and path filters can leave a required check
# pending on excluded PRs. Without a PR diff, treat filtered publishers as
# unavailable. Branch filters and job conditions are not evaluated here.
runs_on_pr() { # workflow file -> 0 for an available PR event
  # NOT `events_of "$1" | grep -q`: grep -q exits on the first match, awk takes
  # SIGPIPE, and `pipefail` turns a match into a non-zero pipeline. That reads
  # as "this workflow cannot run on a PR" for exactly the files where the
  # event is listed early — a false negative in the gate, not in the workflows.
  local ev
  ev="$(events_of "$1")" || return 1
  grep -qx 'pull_request_target' <<<"$ev" && return 0
  grep -qx 'pull_request' <<<"$ev" || return 1
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

# ------------------------------------------------------------------ audit -----
# Print the required-context audit using workflows beneath ROOT; if JSON_OUT is
# set, attempt to write its JSON summary. Returns 1 for UNSATISFIABLE or
# PR-UNPRODUCIBLE findings when STRICT=1, otherwise 0.
# Rule-reading failures return 1 without an audit report: nothing was inspected.
# JSON write failures do not affect the return status.
run_audit() {
  local pubb pubb_pr required ROWS unsatisfiable=0 line ctx app verdict
  pubb="$(mktemp)"; pubb_pr="$(mktemp)"; required="$(mktemp)"; ROWS="$(mktemp)"
  publishable_names | sort -u >"$pubb"
  publishable_names pr | sort -u >"$pubb_pr"
  # Never let a failed inspection leave a stale or empty successful report.
  [ -z "$JSON_OUT" ] || rm -f "$JSON_OUT"
  if ! required_json >"$required"; then
    err "required contexts not inspected — could not read branch rules"
    rm -f "$pubb" "$pubb_pr" "$required" "$ROWS"
    return 1
  fi

  say ""
  say "== publishable context names ($(wc -l <"$pubb" | tr -d ' '); $(wc -l <"$pubb_pr" | tr -d ' ') on pull_request)"
  sed 's/^/  /' "$pubb"
  say ""
  say "== required contexts vs publishable names"
  if [ ! -s "$required" ]; then
    info "no required status contexts found (or none could be read)"
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

  if [ -n "$JSON_OUT" ]; then
    jq -Rs --arg repo "${GITHUB_REPOSITORY:-$ROOT}" --argjson n "$unsatisfiable" --rawfile _ /dev/null '
      {repository: $repo, unsatisfiable: $n,
       results: (split("\n") | map(select(length > 0) | split("\t")
                 | {context: .[0], app_id: (.[1]|tonumber), verdict: .[2]}))}' "$ROWS" >"$JSON_OUT" 2>/dev/null || true
    info "json report written to $JSON_OUT"
  fi
  rm -f "$pubb" "$pubb_pr" "$required" "$ROWS"
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
  check() { # label expected-exit contexts-json strict [expected-verdict]
    local label="$1" want="$2" file="$3" strict="${4:-0}" verdict="${5:-}" out status
    out="$(REUSABLE_CACHE_DIR="$work/cache" REQUIRED_CONTEXTS_JSON="$file" \
             REQUIRED_CONTEXTS_STRICT="$strict" GITHUB_REPOSITORY=example/repo \
             bash "$0" "$work/repo" --no-network --json "$work/report.json" 2>&1)"; status=$?
    if [ "$status" != "$want" ]; then
      fail=$((fail+1)); say "FAIL $label — expected exit $want, got $status"; say "$out" | head -6
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
  if [ "$inspection_status" = 1 ] && [ ! -e "$work/report.json" ] && \
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
