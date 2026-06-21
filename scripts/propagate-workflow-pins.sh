#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# propagate-workflow-pins.sh — Deliberate, audit-first propagation of the
#   standards reusable-workflow SHA pins to consumer repos.
#
# This is the *deliberate-bump* half of the workflow-staleness story. The gate
# (scripts/check-workflow-staleness.sh) no longer forces a consumer to HEAD on
# every standards commit — it tolerates pins within a recency window (see
# docs/decisions/ADR-003-workflow-pin-staleness-window.adoc). This script is
# how a bump actually lands when one is *wanted* (e.g. to pick up a fix early,
# or on a scheduled cadence): it rewrites a consumer's standards reusable pins
# to a target SHA so a human / gitbot-fleet can open a bump PR.
#
# It is the concrete standards-side action referenced by the Hypatia
# `sha_bump_propagation` rule (hyperpolymath/hypatia): that rule surfaces a
# `:review` finding "consumer pin is behind standards HEAD"; the routed fix is
# to run this script (--fix) in the consumer checkout and open a draft PR. The
# rule decides *whether* to bump; this script performs the edit.
#
# Mode: READ-ONLY (audit) by default.
#   Pass --fix to stage the rewrite on a branch in each affected repo.
#   The script NEVER commits or pushes — that is the human's / bot's job
#   (estate guardrail: no unattended mutations; mirrors
#   scripts/propagate-gitignore-67-68.sh).
#
# Principles (do not violate):
#   * Non-destructive: only rewrites the SHA after a standards reusable `@`.
#   * Idempotent: pins already at the target are left untouched.
#   * No auto-commit / auto-push.
#   * Shell-only: no Python, no Ruby.
#
# Usage:
#   bash scripts/propagate-workflow-pins.sh [--fix] [--to <sha>] [PATH]
#
#   PATH may be a single consumer repo (has .github/workflows) or a parent
#   directory of many repos. Defaults to the current directory.
#   --to <sha>  target standards SHA to pin to. If omitted, resolved from
#               (in order) $STANDARDS_TARGET_SHA, a local standards checkout
#               ($STANDARDS_DIR or ~/standards), then `git ls-remote` HEAD.
#
# Output: tab-separated audit lines suitable for review.

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration / argument parsing
# ---------------------------------------------------------------------------

FIX_MODE=false
TARGET_SHA="${STANDARDS_TARGET_SHA:-}"
BRANCH="chore/bump-standards-pins"
TARGET_PATH="."

while [[ $# -gt 0 ]]; do
  case "$1" in
    --fix) FIX_MODE=true; shift ;;
    --to) TARGET_SHA="$2"; shift 2 ;;
    --to=*) TARGET_SHA="${1#*=}"; shift ;;
    -h|--help) sed -n '2,40p' "$0"; exit 0 ;;
    *) TARGET_PATH="$1"; shift ;;
  esac
done

# Matches `hyperpolymath/standards/.github/workflows/<name>.yml@<sha>` on any
# active line. We rewrite the SHA only; everything else on the line is kept.
PIN_RE='hyperpolymath/standards/\.github/workflows/[A-Za-z0-9._-]+\.yml@[0-9a-fA-F]{7,40}'

log() { printf '%s\n' "$*" >&2; }

# ---------------------------------------------------------------------------
# Resolve the target SHA
# ---------------------------------------------------------------------------

resolve_target() {
  if [ -n "$TARGET_SHA" ]; then printf '%s' "$TARGET_SHA"; return 0; fi

  local sd="${STANDARDS_DIR:-$HOME/standards}"
  if git -C "$sd" rev-parse HEAD >/dev/null 2>&1; then
    git -C "$sd" rev-parse HEAD; return 0
  fi

  # Network fallback — the live standards HEAD.
  local remote
  remote=$(git ls-remote https://github.com/hyperpolymath/standards.git HEAD 2>/dev/null | awk '{print $1}')
  if [ -n "$remote" ]; then printf '%s' "$remote"; return 0; fi

  return 1
}

if ! TARGET_SHA="$(resolve_target)"; then
  log "ERROR: could not resolve a target standards SHA. Pass --to <sha> or set STANDARDS_TARGET_SHA."
  exit 2
fi

# ---------------------------------------------------------------------------
# Per-file rewrite (the unit-testable core)
# ---------------------------------------------------------------------------

# Echoes the count of pins in $1 that are NOT already at $TARGET_SHA.
count_behind() {
  local f="$1"
  grep -hoE "$PIN_RE" "$f" 2>/dev/null \
    | sed -E 's/.*@([0-9a-fA-F]+)/\1/' \
    | grep -vcx "$TARGET_SHA" || true
}

# Rewrite every standards reusable pin in $1 to $TARGET_SHA, in place.
rewrite_file() {
  local f="$1"
  sed -E -i "s#(${PIN_RE%@*}@)[0-9a-fA-F]{7,40}#\1${TARGET_SHA}#g" "$f"
}

# ---------------------------------------------------------------------------
# Per-repo processing
# ---------------------------------------------------------------------------

process_repo() {
  local repo="$1" name
  name="$(basename "$repo")"
  local wfdir="$repo/.github/workflows"
  [ -d "$wfdir" ] || return 0

  local total_behind=0 files_touched=0
  local f base behind
  for f in "$wfdir"/*.yml "$wfdir"/*.yaml; do
    [ -f "$f" ] || continue
    grep -qE "$PIN_RE" "$f" || continue
    base="$(basename "$f")"
    behind="$(count_behind "$f")"
    if [ "$behind" -gt 0 ]; then
      printf '%-30s\t%-28s\tBEHIND(%s)\t-> %s\n' "$name" "$base" "$behind" "${TARGET_SHA:0:12}"
      total_behind=$((total_behind + behind))
      files_touched=$((files_touched + 1))
      if $FIX_MODE; then
        rewrite_file "$f"
      fi
    else
      printf '%-30s\t%-28s\tFRESH\t-> %s\n' "$name" "$base" "${TARGET_SHA:0:12}"
    fi
  done

  if $FIX_MODE && [ "$files_touched" -gt 0 ] && git -C "$repo" rev-parse --git-dir >/dev/null 2>&1; then
    local current_branch
    current_branch=$(git -C "$repo" branch --show-current 2>/dev/null || echo "")
    if [ "$current_branch" != "$BRANCH" ]; then
      if git -C "$repo" rev-parse --verify "refs/heads/$BRANCH" >/dev/null 2>&1; then
        git -C "$repo" checkout -q "$BRANCH" 2>/dev/null || true
      else
        git -C "$repo" checkout -q -b "$BRANCH" 2>/dev/null || true
      fi
    fi
    git -C "$repo" add "$wfdir" 2>/dev/null || true
    log "  [staged] $name — $files_touched file(s) on branch '$BRANCH' (commit/push left to human/gitbot-fleet)"
  fi

  return 0
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

echo "# propagate-workflow-pins.sh — $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "# mode:   $([ "$FIX_MODE" = true ] && echo FIX || echo AUDIT)"
echo "# target: $TARGET_SHA"
echo "# path:   $TARGET_PATH"
echo "#"
printf '%-30s\t%-28s\t%s\t%s\n' "REPO" "WORKFLOW" "STATUS" "TARGET"

if [ -d "$TARGET_PATH/.github/workflows" ]; then
  process_repo "$TARGET_PATH"
else
  for repo in "$TARGET_PATH"/*/; do
    [ -d "${repo}.github/workflows" ] || continue
    process_repo "${repo%/}"
  done
fi

echo "#"
echo "# Audit complete."
if $FIX_MODE; then
  echo "# --fix: review staged branches ('$BRANCH') and open DRAFT PRs."
else
  echo "# Re-run with --fix to stage the bump on a branch (no commit/push)."
fi
