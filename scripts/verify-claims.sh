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
# Hardened (Wave-4.1) against adversarial review: fail-SAFE everywhere — when
# trustworthy primary evidence cannot be collected, the verdict is
# `unverifiable`, never a confident wrong `confirmed`/`refuted`. Every claim
# appears in the output exactly once (no silent drops), missing required fields
# are `unverifiable`, and empty/always-matching expectations are rejected.
#
# SECURITY: the `command-transcript` verifier EXECUTES the claim's `target`.
# The claims file is trusted input (repo-authored, reviewed in PR). For
# untrusted claims a conforming verifier MUST sandbox execution; this reference
# impl does not sandbox and is for trusted claims only.
#
# This reference impl handles the LOCAL verifiers (git-diff, command-transcript,
# claims-compose). Network verifiers (ci-run, issue-state, pr-state) and the
# manual verifier return `unverifiable` with a reason.
#
# Usage: verify-claims.sh [path/to/CLAIMS.a2ml]   (default: ./CLAIMS.a2ml)
#   env DYADT_BASE   git ref claims are diffed against (default: origin/main, then HEAD~1)
#   env DYADT_ALLOW_UNVERIFIABLE=1  treat unverifiable as non-fatal (still reported)
# Exit: 0 all confirmed (or unverifiable allowed) · 1 a claim refuted/unverifiable · 2 usage/parse

set -uo pipefail

CLAIMS="${1:-CLAIMS.a2ml}"
[ -f "$CLAIMS" ] || { echo "error: claims file not found: $CLAIMS" >&2; exit 2; }

# Recursion depth guard for claims-compose (fork-bomb / cycle protection).
DYADT_DEPTH="${DYADT_DEPTH:-0}"
if [ "$DYADT_DEPTH" -gt 8 ]; then
  echo "error: DYADT compose recursion too deep (>8) — possible claim cycle" >&2; exit 2
fi

BASE="${DYADT_BASE:-}"
if [ -z "$BASE" ]; then
  if git rev-parse --verify -q origin/main >/dev/null 2>&1; then BASE="origin/main"
  elif git rev-parse --verify -q HEAD~1 >/dev/null 2>&1; then BASE="HEAD~1"
  else BASE=""; fi
fi
# A base ref that does not resolve is treated as NO base (fail safe): the
# created/modified/deleted verifiers then return `unverifiable no-base-ref`
# rather than confidently confirming against a phantom ref.
if [ -n "$BASE" ] && ! git rev-parse --verify -q "$BASE" >/dev/null 2>&1; then BASE=""; fi

# --- helpers ----------------------------------------------------------------

# A target used by a file verifier must be a safe, repo-relative path: no
# absolute paths, no traversal, no symlink (symlinks could redirect evidence to
# a known-good file while the real artefact is untouched).
unsafe_path() { # path -> 0 if UNSAFE
  case "$1" in
    /*|../*|*/../*|*/..) return 0 ;;   # absolute or traversal
  esac
  [ -L "$1" ] && return 0             # symlink
  return 1
}

# --- primary-evidence verifiers ---------------------------------------------

# git-diff: file-changed. echoes "<verdict> <reason>"
v_git_diff() { # target expect
  local target="$1" expect="$2" existed_now=0 existed_base=0 tracked_now=0
  if unsafe_path "$target"; then echo "unverifiable unsafe-path"; return; fi
  # created/modified/deleted need a resolvable base; without one we cannot tell
  # "new" from "pre-existing" — fail safe, not confident-wrong.
  case "$expect" in
    created|modified|deleted)
      [ -n "$BASE" ] || { echo "unverifiable no-base-ref"; return; } ;;
  esac
  [ -e "$target" ] && existed_now=1
  [ "$existed_now" = 1 ] && git ls-files --error-unmatch -- "$target" >/dev/null 2>&1 && tracked_now=1
  if [ -n "$BASE" ] && git cat-file -e "$BASE:$target" 2>/dev/null; then existed_base=1; fi
  case "$expect" in
    created)
      if [ "$existed_now" = 1 ] && [ "$existed_base" = 0 ]; then
        # a real "created" is a tracked file new in this change, not stray build output
        [ "$tracked_now" = 1 ] && echo "confirmed created" || echo "refuted exists-but-untracked"
      else echo "refuted not-newly-created"; fi ;;
    modified)
      if [ "$existed_now" = 1 ] && [ "$existed_base" = 1 ]; then
        if ! git diff --quiet "$BASE" -- "$target" 2>/dev/null; then echo "confirmed modified"; else echo "refuted unchanged"; fi
      else echo "refuted not-modified-pair"; fi ;;
    deleted)
      { [ "$existed_now" = 0 ] && [ "$existed_base" = 1 ]; } && echo "confirmed deleted" || echo "refuted not-deleted" ;;
    contains:*)
      local re="${expect#contains:}"
      [ -n "$re" ] || { echo "unverifiable empty-pattern"; return; }
      if [ ! -f "$target" ]; then echo "unverifiable not-a-regular-file"; return; fi
      if [ ! -r "$target" ]; then echo "unverifiable unreadable"; return; fi
      # distinguish "pattern absent" (refuted) from "bad regex" (unverifiable)
      local gout grc
      gout="$(grep -Eq -- "$re" "$target" 2>&1)"; grc=$?
      if [ "$grc" -eq 0 ]; then echo "confirmed contains"
      elif [ "$grc" -eq 1 ]; then echo "refuted missing-pattern"
      else echo "unverifiable bad-regex"; fi ;;
    sha256:*)
      local want="${expect#sha256:}" got
      [ -n "$want" ] || { echo "unverifiable empty-hash"; return; }
      if [ ! -f "$target" ]; then echo "unverifiable not-a-regular-file"; return; fi
      if [ ! -r "$target" ]; then echo "unverifiable unreadable"; return; fi
      got="$(sha256sum "$target" 2>/dev/null | cut -d' ' -f1)"
      [ "$got" = "$want" ] && echo "confirmed sha256" || echo "refuted sha256-mismatch" ;;
    *) echo "unverifiable bad-expect" ;;
  esac
}

# command-transcript: run the command, judge by exit / stdout (stdout ONLY —
# stderr is captured separately so a marker on stderr cannot false-confirm).
v_command() { # target(command) expect
  local cmd="$1" expect="$2" out rc
  [ -n "$cmd" ] || { echo "unverifiable empty-command"; return; }
  local errf; errf="$(mktemp)"
  out="$(bash -c "$cmd" 2>"$errf")"; rc=$?
  rm -f "$errf"
  case "$expect" in
    exit==*)
      local want="${expect#exit==}"
      case "$want" in ''|*[!0-9]*) echo "unverifiable bad-expect"; return ;; esac
      [ "$rc" = "$want" ] && echo "confirmed exit=$rc" || echo "refuted exit=$rc" ;;
    stdout-contains:*)
      local pat="${expect#stdout-contains:}"
      [ -n "$pat" ] || { echo "unverifiable empty-pattern"; return; }
      grep -Fq -- "$pat" <<< "$out" && echo "confirmed stdout-match" || echo "refuted stdout-nomatch" ;;
    *) echo "unverifiable bad-expect" ;;
  esac
}

# claims-compose: referenced CLAIMS.a2ml must be all-confirmed
v_compose() { # target(path) expect
  local path="$1" expect="$2"
  [ "$expect" = "all-confirmed" ] || { echo "unverifiable bad-expect"; return; }
  if unsafe_path "$path"; then echo "unverifiable unsafe-path"; return; fi
  [ -f "$path" ] || { echo "refuted no-such-claims"; return; }
  if DYADT_ALLOW_UNVERIFIABLE=0 DYADT_DEPTH="$((DYADT_DEPTH + 1))" bash "$0" "$path" >/dev/null 2>&1; then
    echo "confirmed all-confirmed"
  else echo "refuted child-not-all-confirmed"; fi
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

verify_one() { # id class target expect verifier statement not_before
  local class="$2" target="$3" expect="$4" verifier="$5" statement="$6" not_before="$7"
  # Required fields — a claim missing any is unverifiable, never guessed.
  if [ -z "$class" ] || [ -z "$target" ] || [ -z "$expect" ] || [ -z "$verifier" ]; then
    echo "unverifiable missing-field"; return
  fi
  # Licence/SPDX claims are ALWAYS manual-only — scan class, target, expect AND
  # statement so a licence claim phrased only in the statement cannot slip past.
  case "$class $target $expect $statement" in
    *[Ll]icence*|*[Ll]icense*|*SPDX*) echo "unverifiable manual-only"; return ;;
  esac
  # not_before (stale-evidence guard): the reference verifier does not collect
  # evidence timestamps, so a claim that pins freshness cannot be trusted here —
  # fail safe rather than verify stale evidence as fresh.
  if [ -n "$not_before" ]; then echo "unverifiable stale-evidence-unsupported"; return; fi
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
# Whitespace-tolerant key/value extraction: accepts `key = "v"`, `key="v"`, and
# trailing inline whitespace. Sets KV_KEY and KV_VAL, or returns 1.
KV_KEY="" KV_VAL=""
kv() { # line
  [[ "$1" =~ ^([A-Za-z_]+)[[:space:]]*=[[:space:]]*\"(.*)\"[[:space:]]*$ ]] || return 1
  KV_KEY="${BASH_REMATCH[1]}"; KV_VAL="${BASH_REMATCH[2]}"; return 0
}

id="" class="" target="" expect="" verifier="" statement="" not_before=""
in_claim=0 block_idx=0
n=0 confirmed=0 refuted=0 unver=0
declare -a rows=()

emit() {
  [ "$in_claim" = 1 ] || return
  block_idx=$((block_idx + 1))
  local cid="${id:-<block-$block_idx-no-id>}"
  local res verdict reason
  res="$(verify_one "$cid" "$class" "$target" "$expect" "$verifier" "$statement" "$not_before")"
  verdict="${res%% *}"; reason="${res#* }"
  # A block with no id is itself a defect: never confirm it, downgrade to unverifiable.
  if [ -z "$id" ] && [ "$verdict" = "confirmed" ]; then verdict="unverifiable"; reason="missing-id"; fi
  n=$((n + 1))
  case "$verdict" in
    confirmed) confirmed=$((confirmed + 1)); rows+=("  ✅ $cid  confirmed    [$class] $reason") ;;
    refuted)   refuted=$((refuted + 1));     rows+=("  ❌ $cid  REFUTED      [${class:-?}] $reason — statement: $statement") ;;
    *)         unver=$((unver + 1));         rows+=("  ⚠️  $cid  unverifiable [${class:-?}] $reason") ;;
  esac
}

while IFS= read -r raw; do
  line="${raw#"${raw%%[![:space:]]*}"}"
  case "$line" in
    '[[claim]]'*) emit; in_claim=1; id=""; class=""; target=""; expect=""; verifier=""; statement=""; not_before=""; continue ;;
  esac
  [ "$in_claim" = 1 ] || continue
  kv "$line" || continue
  case "$KV_KEY" in
    id)          id="$KV_VAL" ;;
    claim_class) class="$KV_VAL" ;;
    target)      target="$KV_VAL" ;;
    expect)      expect="$KV_VAL" ;;
    verifier)    verifier="$KV_VAL" ;;
    statement)   statement="$KV_VAL" ;;
    not_before)  not_before="$KV_VAL" ;;
  esac
done < "$CLAIMS"
emit

echo "DYADT verify: $CLAIMS (base: ${BASE:-<none>})"
printf '%s\n' "${rows[@]}"
echo "  ── $confirmed confirmed · $refuted refuted · $unver unverifiable (of $n claims)"

if [ "$refuted" -gt 0 ]; then exit 1; fi
if [ "$unver" -gt 0 ] && [ "${DYADT_ALLOW_UNVERIFIABLE:-0}" != "1" ]; then exit 1; fi
exit 0
