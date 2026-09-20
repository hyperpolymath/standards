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
# `scan / Hypatia Neurosymbolic Analysis`; that repository now publishes the bare
# name from an inline job and the prefixed name from its wrapper caller), and
# catalogued for the wrapper case in
# docs/audits/audit-hypatia-pin-orphan-2026-05-27.adoc.
#
# THE RULE THIS ENCODES
# ---------------------
#   * a plain job publishes its `name:` — or `<job id> (<matrix values>)` when it
#     has a matrix and no `name:`, or the job id alone otherwise;
#   * a job that calls a reusable workflow publishes
#         <caller display name or job id> / <inner job display name or job id>
#     and therefore can NEVER publish the reusable's bare inner name;
#   * a context bound to a third-party app integration (CodeQL scanning,
#     CodeRabbit, SonarCloud, …) is that app's to publish, not the repository's.
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
  if curl -fsSL "$url" -o "$cache" 2>/dev/null; then
    printf '%s\n' "$cache"
  else
    rm -f "$cache"; return 1
  fi
}

publishable_names() { # repo root -> one name per line
  local f id name uses mat inner iid iname stub
  for f in "$ROOT"/.github/workflows/*.yml "$ROOT"/.github/workflows/*.yaml; do
    [ -f "$f" ] || continue
    while IFS=$'\t' read -r id name uses mat; do
      [ -n "$id" ] || continue
      [ "$name" = "-" ] && name=""
      [ "$uses" = "-" ] && uses=""
      case "$uses" in
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
  done
}

# ------------------------------------------------------------ required set ----
required_json() { # emit {"context":..,"app_id":..,"source":..} objects
  if [ -n "${REQUIRED_CONTEXTS_JSON:-}" ]; then
    [ -f "$REQUIRED_CONTEXTS_JSON" ] || { err "REQUIRED_CONTEXTS_JSON not found: $REQUIRED_CONTEXTS_JSON"; exit 1; }
    jq -c '.[] | {context: .context, app_id: (.app_id // 0), source: (.source // "file")}' "$REQUIRED_CONTEXTS_JSON" \
      || { err "REQUIRED_CONTEXTS_JSON is not a JSON array of {\"context\": ...} objects"; exit 1; }
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
run_audit() {
  local pubb required ROWS unsatisfiable=0 line ctx app verdict
  pubb="$(mktemp)"; required="$(mktemp)"; ROWS="$(mktemp)"
  publishable_names | sort -u >"$pubb"
  required_json >"$required" || true

  say ""
  say "== publishable context names ($(wc -l <"$pubb" | tr -d ' '))"
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
             if (n ~ /\*/ && c ~ ("^" n "$")) ok=1 }
           END { exit(ok?0:1) }' "$pubb"; then
      verdict=producible
    else
      verdict=UNSATISFIABLE
    fi
    printf '%s\t%s\t%s\n' "$ctx" "$app" "$verdict" >>"$ROWS"
    case "$verdict" in
      UNSATISFIABLE)
        unsatisfiable=$((unsatisfiable+1))
        if [ "$STRICT" = 1 ]; then
          err "required context '$ctx' ($src) is not producible by any workflow here, is not bound to an app integration, and no job publishes a matching name"
        else
          warn "required context '$ctx' ($src) is not producible by any workflow here and is not bound to an app integration — unsatisfiable as written"
        fi
        if [ "$PRINT_COMMANDS" = 1 ]; then
          cat <<EOS
    # Remediation — require a name this repository publishes, or publish this one:
    #   a) replace the requirement with a producible name (list above), or
    #   b) implement the check in-repo and publish '$ctx' from a plain job
    #      (a reusable-wrapper caller cannot: it publishes '<caller> / <inner job>').
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
  rm -f "$pubb" "$required" "$ROWS"
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
  check() { # label expected-exit contexts-json strict
    local label="$1" want="$2" file="$3" strict="${4:-0}" out status
    out="$(REUSABLE_CACHE_DIR="$work/cache" REQUIRED_CONTEXTS_JSON="$file" \
             REQUIRED_CONTEXTS_STRICT="$strict" GITHUB_REPOSITORY=example/repo \
             bash "$0" "$work/repo" --no-network 2>&1)"; status=$?
    if [ "$status" = "$want" ]; then pass=$((pass+1)); say "PASS $label"
    else fail=$((fail+1)); say "FAIL $label — expected exit $want, got $status"; say "$out" | head -6; fi
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
  rm -rf "$work"
  say ""
  say "self-test: $pass passed, $fail failed"
  [ "$fail" -eq 0 ]
}

if [ "$SELF_TEST" = 1 ]; then self_test; exit $?; fi
run_audit
