#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# verify-claims.sh — reference verifier for the DYADT claim format.
# See did-you-actually-do-that/spec/{CLAIM-FORMAT,VERIFICATION-PROTOCOL}.adoc.
#
# Re-derives each claim's outcome from PRIMARY EVIDENCE (git tree, real command
# runs) — never from the agent's own statement/evidence text — and emits one
# verdict per claim: confirmed | refuted | unverifiable. `unverifiable` is LOUD:
# by default the run fails unless every claim is `confirmed`.
#
# This reference impl handles the LOCAL verifiers (git-diff, command-transcript,
# claims-compose). Network verifiers (ci-run, issue-state, pr-state) and the
# manual verifier return `unverifiable` with a reason — the production verifier
# in hyperpolymath/did-you-actually-do-that implements those against real APIs.
#
# Usage: verify-claims.sh [path/to/CLAIMS.a2ml]   (default: ./CLAIMS.a2ml)
#   env DYADT_BASE   git ref claims are diffed against (default: origin/main, then HEAD~1)
#   env DYADT_ALLOW_UNVERIFIABLE=1  treat unverifiable as non-fatal (still reported)
# Exit: 0 all confirmed (or unverifiable allowed) · 1 a claim refuted/unverifiable · 2 usage/parse

set -uo pipefail

CLAIMS="${1:-CLAIMS.a2ml}"
[ -f "$CLAIMS" ] || { echo "error: claims file not found: $CLAIMS" >&2; exit 2; }

BASE="${DYADT_BASE:-}"
if [ -z "$BASE" ]; then
  if git rev-parse --verify -q origin/main >/dev/null 2>&1; then BASE="origin/main"
  elif git rev-parse --verify -q HEAD~1 >/dev/null 2>&1; then BASE="HEAD~1"
  else BASE=""; fi
fi

# --- primary-evidence verifiers ---------------------------------------------

# git-diff: file-changed. echoes confirmed|refuted|unverifiable + reason
v_git_diff() { # target expect
  local target="$1" expect="$2" existed_now=0 existed_base=0
  [ -e "$target" ] && existed_now=1
  if [ -n "$BASE" ] && git cat-file -e "$BASE:$target" 2>/dev/null; then existed_base=1; fi
  case "$expect" in
    created)
      { [ "$existed_now" = 1 ] && [ "$existed_base" = 0 ]; } && echo "confirmed created" || echo "refuted not-newly-created" ;;
    modified)
      if [ "$existed_now" = 1 ] && [ "$existed_base" = 1 ]; then
        if [ -n "$BASE" ] && ! git diff --quiet "$BASE" -- "$target" 2>/dev/null; then echo "confirmed modified"; else echo "refuted unchanged"; fi
      else echo "refuted not-modified-pair"; fi ;;
    deleted)
      { [ "$existed_now" = 0 ] && [ "$existed_base" = 1 ]; } && echo "confirmed deleted" || echo "refuted not-deleted" ;;
    contains:*)
      local re="${expect#contains:}"
      if [ "$existed_now" = 1 ] && grep -Eq -- "$re" "$target" 2>/dev/null; then echo "confirmed contains"; else echo "refuted missing-pattern"; fi ;;
    sha256:*)
      local want="${expect#sha256:}" got
      if [ "$existed_now" = 1 ]; then got="$(sha256sum "$target" | cut -d' ' -f1)"; [ "$got" = "$want" ] && echo "confirmed sha256" || echo "refuted sha256-mismatch"; else echo "refuted absent"; fi ;;
    *) echo "unverifiable bad-expect" ;;
  esac
}

# command-transcript: run the command, judge by exit / stdout
v_command() { # target(command) expect
  local cmd="$1" expect="$2" out rc
  out="$(bash -c "$cmd" 2>&1)"; rc=$?
  case "$expect" in
    exit==*)          [ "$rc" = "${expect#exit==}" ] && echo "confirmed exit=$rc" || echo "refuted exit=$rc" ;;
    stdout-contains:*) grep -Fq -- "${expect#stdout-contains:}" <<< "$out" && echo "confirmed stdout-match" || echo "refuted stdout-nomatch" ;;
    *) echo "unverifiable bad-expect" ;;
  esac
}

# claims-compose: referenced CLAIMS.a2ml must be all-confirmed
v_compose() { # target(path) expect
  local path="$1" expect="$2"
  [ "$expect" = "all-confirmed" ] || { echo "unverifiable bad-expect"; return; }
  [ -f "$path" ] || { echo "refuted no-such-claims"; return; }
  if DYADT_ALLOW_UNVERIFIABLE=0 bash "$0" "$path" >/dev/null 2>&1; then echo "confirmed all-confirmed"; else echo "refuted child-not-all-confirmed"; fi
}

# --- dispatch ----------------------------------------------------------------
# Compatibility: verifier -> claim_classes it may discharge.
compatible() { # verifier claim_class
  case "$1:$2" in
    git-diff:file-changed) return 0 ;;
    command-transcript:command-ran|command-transcript:test-passed) return 0 ;;
    ci-run:ci-green) return 0 ;;
    issue-state:issue-closed) return 0 ;;
    pr-state:pr-merged) return 0 ;;
    claims-compose:claim-of-claims) return 0 ;;
    manual:*) return 0 ;;
    *) return 1 ;;
  esac
}

verify_one() { # id class target expect verifier
  local id="$1" class="$2" target="$3" expect="$4" verifier="$5"
  # licence/SPDX claims are always manual-only (estate policy)
  case "$class $target $expect" in
    *[Ll]icence*|*[Ll]icense*|*SPDX*) echo "unverifiable manual-only-licence"; return ;;
  esac
  if ! compatible "$verifier" "$class"; then echo "unverifiable incompatible-verifier"; return; fi
  case "$verifier" in
    git-diff)           v_git_diff "$target" "$expect" ;;
    command-transcript) v_command "$target" "$expect" ;;
    claims-compose)     v_compose "$target" "$expect" ;;
    ci-run|issue-state|pr-state) echo "unverifiable needs-network-verifier" ;;
    manual)             echo "unverifiable manual-only" ;;
    *)                  echo "unverifiable unknown-verifier" ;;
  esac
}

# --- parse + run -------------------------------------------------------------
field() { sed -E "s/^$1 = \"//; s/\"$//"; }

id="" class="" target="" expect="" verifier=""
n=0 confirmed=0 refuted=0 unver=0
declare -a rows=()

emit() {
  [ -z "$id" ] && return
  local res verdict reason
  res="$(verify_one "$id" "$class" "$target" "$expect" "$verifier")"
  verdict="${res%% *}"; reason="${res#* }"
  n=$((n+1))
  case "$verdict" in
    confirmed) confirmed=$((confirmed+1)); rows+=("  ✅ $id  confirmed    [$class] $reason") ;;
    refuted)   refuted=$((refuted+1));     rows+=("  ❌ $id  REFUTED      [$class] $reason — statement: $statement") ;;
    *)         unver=$((unver+1));         rows+=("  ⚠️  $id  unverifiable [$class] $reason") ;;
  esac
}

statement=""
while IFS= read -r raw; do
  line="${raw#"${raw%%[![:space:]]*}"}"
  case "$line" in
    '[[claim]]'*) emit; id=""; class=""; target=""; expect=""; verifier=""; statement="" ;;
    'id = "'*)          id="$(printf '%s' "$line" | field id)" ;;
    'claim_class = "'*) class="$(printf '%s' "$line" | field claim_class)" ;;
    'target = "'*)      target="$(printf '%s' "$line" | field target)" ;;
    'expect = "'*)      expect="$(printf '%s' "$line" | field expect)" ;;
    'verifier = "'*)    verifier="$(printf '%s' "$line" | field verifier)" ;;
    'statement = "'*)   statement="$(printf '%s' "$line" | field statement)" ;;
  esac
done < "$CLAIMS"
emit

echo "DYADT verify: $CLAIMS (base: ${BASE:-<none>})"
printf '%s\n' "${rows[@]}"
echo "  ── $confirmed confirmed · $refuted refuted · $unver unverifiable (of $n claims)"

if [ "$refuted" -gt 0 ]; then exit 1; fi
if [ "$unver" -gt 0 ] && [ "${DYADT_ALLOW_UNVERIFIABLE:-0}" != "1" ]; then exit 1; fi
exit 0
