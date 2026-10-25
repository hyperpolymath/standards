#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# remove-unsatisfiable-data-rules.sh — remove `code_quality` / `code_coverage`
# rules from branch rulesets when the repository cannot feed them.
#
# WHY THIS EXISTS
# ---------------
# A `code_quality` or `code_coverage` ruleset rule is satisfied by DATA, not
# by a status context: GitHub Code Quality must be enabled in the repository
# settings (it then registers the generated workflow
# `dynamic/github-code-quality/codeql`), and coverage must actually be
# uploaded by a workflow using `actions/upload-code-coverage`. When the data
# pipeline does not exist, the rule blocks every pull request forever while
# publishing no check a contributor can see fail — the exact shape
# scripts/check-required-contexts.sh reports as UNSATISFIED-RULE.
#
# Measured 2026-10-25 on this repository: ruleset 23787415 (the main gate)
# carries `code_quality` (severity: all) while Code Quality is not enabled
# (no generated workflow in the catalogue), so the first pull request with no
# ruleset bypass (#1338 and its successor) cannot merge however green it is.
#
# THE PREFERRED REPAIR IS NOT THIS SCRIPT
# ---------------------------------------
# Enabling the data pipeline keeps the protection: turn on Code Quality in
# the repository settings, or land a coverage upload that succeeds. This
# script exists for the deliberate alternative — removing a rule whose data
# the estate has decided not to produce — so that the removal is a reviewed,
# read-back-verified act instead of a UI edit nobody can reconstruct. It
# never touches any other rule, condition, or bypass actor.
#
# WHAT IT REFUSES
# ---------------
# * writing without an explicit `--rule <type>` naming each type to remove;
# * a ruleset that is not `active` (reported, skipped);
# * a payload that would change anything other than deleting the named rule
#   types from `rules` (asserted before send, on payloads built the same way
#   from the same source — never against the raw API response, which carries
#   read-only fields a PUT body must not);
# * a read-back that differs from the intended payload, or cannot be read.
#
# Requires `administration: write`. Installation tokens report repo
# admin=true and still get 403 on ruleset writes; use an admin user token.
#
# Usage:
#   scripts/remove-unsatisfiable-data-rules.sh                 # report only
#   scripts/remove-unsatisfiable-data-rules.sh --apply --rule code_quality
#   ONLY=23787415 scripts/remove-unsatisfiable-data-rules.sh --apply --rule code_quality
#   REPO=owner/name scripts/remove-unsatisfiable-data-rules.sh
#
# Exit: 0 = reported (or applied and verified); 1 = fatal / refused;
#       2 = usage error (including --apply with no --rule).

set -uo pipefail

REPO="${REPO:-${GITHUB_REPOSITORY:-}}"
ONLY="${ONLY:-}"          # space-separated ruleset ids; empty = every repo branch ruleset
GH="${GH:-gh}"            # injectable for scripts/tests/remove-unsatisfiable-data-rules-test.sh
APPLY=0
RULES=""                  # space-separated rule types named for removal

while [ $# -gt 0 ]; do
  case "$1" in
    --apply) APPLY=1; shift ;;
    --rule)
      case "${2:-}" in
        code_quality|code_coverage) RULES="$RULES $2"; shift 2 ;;
        *) echo "--rule needs code_quality or code_coverage, got '${2:-}'" >&2; exit 2 ;;
      esac ;;
    -h|--help) sed -n '2,58p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown argument: $1 (try --help)" >&2; exit 2 ;;
  esac
done
RULES="$(printf '%s' "$RULES" | tr ' ' '\n' | sed '/^$/d' | sort -u | tr '\n' ' ')"

if [ "$APPLY" -eq 1 ] && [ -z "${RULES// /}" ]; then
  echo "--apply refuses to guess: name each rule type to remove with --rule" >&2
  echo "  e.g. $0 --apply --rule code_quality" >&2
  exit 2
fi

command -v "$GH" >/dev/null || { echo "'$GH' is required" >&2; exit 2; }
command -v jq >/dev/null || { echo "jq is required" >&2; exit 2; }

if [ -z "$REPO" ]; then
  REPO="$("$GH" repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null)" \
    || { echo "cannot resolve the repository; set REPO=owner/name" >&2; exit 1; }
fi
[ -n "$REPO" ] || { echo "cannot resolve the repository; set REPO=owner/name" >&2; exit 1; }

tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT

echo "== $REPO"

if ! "$GH" api "repos/$REPO/rulesets" >"$tmp/list.json" 2>"$tmp/err"; then
  echo "FATAL: cannot list rulesets: $(head -1 "$tmp/err")" >&2
  echo "       (403 here means the token cannot see protection at all, not that" >&2
  echo "        none exists)" >&2
  exit 1
fi
if ! jq -e 'type == "array"' "$tmp/list.json" >/dev/null 2>&1; then
  echo "FATAL: ruleset list for $REPO is not a JSON array — nothing inspected" >&2
  exit 1
fi

ids="$(jq -r '.[] | select(.target=="branch" and .source_type=="Repository") | .id' "$tmp/list.json")"
if [ -n "$ONLY" ]; then
  for id in $ONLY; do
    if ! grep -qxF "$id" <<<"$ids"; then
      echo "FATAL: targeted ruleset $id is not a repository-owned branch ruleset" >&2
      exit 1
    fi
  done
  ids="$ONLY"
fi
[ -z "$ids" ] && { echo "no repository-owned branch rulesets to inspect"; exit 0; }

changed=0; skipped=0; target=0; found=0

for id in $ids; do
  if ! "$GH" api "repos/$REPO/rulesets/$id" >"$tmp/rs.json" 2>"$tmp/err"; then
    if [ -n "$ONLY" ]; then
      echo "FATAL: cannot read targeted ruleset $id: $(head -1 "$tmp/err")" >&2
      exit 1
    fi
    echo "SKIP $id — cannot read it: $(head -1 "$tmp/err")" >&2
    skipped=$((skipped+1)); continue
  fi
  if ! jq -e 'type == "object" and (.rules | type == "array")' "$tmp/rs.json" >/dev/null 2>&1; then
    echo "FATAL: ruleset $id did not read back as a ruleset payload" >&2
    exit 1
  fi

  name="$(jq -r '.name' "$tmp/rs.json")"
  enforcement="$(jq -r '.enforcement' "$tmp/rs.json")"
  present="$(jq -r '[.rules[].type | select(. == "code_quality" or . == "code_coverage")] | join(", ")' "$tmp/rs.json")"

  if [ -z "$present" ]; then
    printf '  ok    %-9s %-58s no data-dependent rules\n' "$id" "$name"
    continue
  fi
  found=$((found+1))
  params="$(jq -r '[.rules[] | select(.type == "code_quality" or .type == "code_coverage")
                   | "\(.type)(\(.parameters // {} | to_entries | map("\(.key)=\(.value)") | join(",")))"] | join(" ")' "$tmp/rs.json")"

  if [ "$enforcement" != "active" ]; then
    printf '  SKIP  %-9s %-58s %s but enforcement=%s\n' "$id" "$name" "$params" "$enforcement"
    skipped=$((skipped+1)); continue
  fi

  if [ "$APPLY" -eq 0 ]; then
    printf '  FOUND %-9s %-58s %s\n' "$id" "$name" "$params"
    continue
  fi

  # Which of the named types does this ruleset actually carry?
  hit=""
  for r in $RULES; do
    if jq -e --arg t "$r" 'any(.rules[]; .type == $t)' "$tmp/rs.json" >/dev/null 2>&1; then
      hit="$hit $r"
    fi
  done
  if [ -z "${hit// /}" ]; then
    printf '  ok    %-9s %-58s %s (none named for removal)\n' "$id" "$name" "$params"
    continue
  fi
  target=$((target+1))
  printf '  FIX   %-9s %-58s removing:%s\n' "$id" "$name" "$hit"

  jq '{name, target, enforcement, conditions, rules, bypass_actors}' \
    "$tmp/rs.json" >"$tmp/before-payload.json"
  rules_json="$(printf '%s' "$hit" | tr ' ' '\n' | sed '/^$/d' | jq -R . | jq -s .)"
  jq --argjson drop "$rules_json" \
    '.rules = [.rules[] | select(.type as $t | ($drop | index($t)) | not)]' \
    "$tmp/before-payload.json" >"$tmp/payload.json"

  if ! diff -q \
      <(jq -S 'del(.rules)' "$tmp/before-payload.json") \
      <(jq -S 'del(.rules)' "$tmp/payload.json") >/dev/null; then
    echo "FATAL: payload would change more than the rules array for $id. Refusing." >&2
    exit 1
  fi
  # The surviving rules must be exactly the originals minus the named types:
  # same objects, same order — a filter that also reordered or edited a
  # surviving rule would not be caught by the field-level diff above.
  if ! jq -e --argjson drop "$rules_json" --slurpfile after "$tmp/payload.json" \
      '([.rules[] | select(.type as $t | ($drop | index($t)) | not)]) == $after[0].rules' \
      "$tmp/before-payload.json" >/dev/null 2>&1; then
    echo "FATAL: surviving rules differ from the originals for $id. Refusing." >&2
    exit 1
  fi
  for r in $hit; do
    if jq -e --arg t "$r" 'any(.rules[]; .type == $t)' "$tmp/payload.json" >/dev/null 2>&1; then
      echo "FATAL: payload still carries $r for $id. Refusing." >&2
      exit 1
    fi
  done

  if ! "$GH" api -X PUT "repos/$REPO/rulesets/$id" --input "$tmp/payload.json" >/dev/null 2>"$tmp/err"; then
    echo "FATAL: write refused for $id:" >&2
    sed 's/^/       /' "$tmp/err" >&2
    echo "       'Resource not accessible by integration' means an installation" >&2
    echo "       token: admin=true is not administration:write. Use a user token." >&2
    exit 1
  fi

  if ! "$GH" api "repos/$REPO/rulesets/$id" >"$tmp/verify.json" 2>"$tmp/err"; then
    echo "FATAL: could not read back $id after writing it — protection is UNVERIFIED" >&2
    sed 's/^/       /' "$tmp/err" >&2
    exit 1
  fi
  if ! jq -e --slurpfile intended "$tmp/payload.json" '
    {name, target, enforcement, conditions, rules, bypass_actors} == $intended[0]
  ' "$tmp/verify.json" >/dev/null 2>&1; then
    echo "FATAL: read-back mismatch for $id — writable ruleset differs from intended payload" >&2
    exit 1
  fi
  echo "        verified: removed:$hit; $(jq -r '.rules | length' "$tmp/verify.json") rules remain"
  changed=$((changed+1))
done

echo
if [ "$APPLY" -eq 0 ]; then
  if [ "$found" -eq 0 ]; then
    echo "REPORT-ONLY: no active repository branch ruleset carries a data-dependent rule."
  else
    echo "REPORT-ONLY: $found ruleset(s) carry data-dependent rules. Nothing was written."
    echo "Preferred repair: feed the rule (enable Code Quality / upload coverage)."
    echo "Deliberate removal: re-run with --apply --rule <type> per type."
  fi
else
  echo "done: $changed ruleset(s) updated, $skipped skipped."
fi
