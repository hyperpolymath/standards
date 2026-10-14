#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# check-copied-lock-step.sh — find workflows that run their own gh-actions-lock.
#
# The fault: a sweep copied a "Verify immutable action lockfile" step into each
# repo's workflow-linter.yml. It downloads a pinned gh-actions-lock binary
# (v0.1.6) and runs it raw. That gives a second verifier beside governance's
# lockfile gate (scripts/update-actions-lock.sh), with a second version pin that
# drifts from the estate's. v0.1.6 cannot read lockfile v0.0.3, which every
# current `gh actions-lock` writes, so the step failed whenever a current tool
# refreshed the lock (proof-burrower#111, #112). Owner decision 2026-10-14:
# drop the step from copied linters and let governance own lock verification.
#
# This lists what is left. It reports; it never edits.
#
# Usage:  check-copied-lock-step.sh REPO_DIR [REPO_SLUG]
#           REPO_SLUG default: basename(REPO_DIR); pass it for a throwaway
#           clone so column 1 names the repository.
# Output: TSV on stdout, one row per workflow:
#           repo <TAB> workflow <TAB> version(s) it pins ("unpinned" if none)
# Exit:   0 = none found (or no workflows), 1 = found, 2 = usage error.
#
# standards itself is skipped: its installs ARE the estate pin, and
# scripts/tests/gh-actions-lock-contract-test.sh holds them to one version.
set -uo pipefail

REPO_DIR="${1:-}"
[ -n "$REPO_DIR" ] && [ -d "$REPO_DIR" ] || {
  echo "usage: check-copied-lock-step.sh REPO_DIR [REPO_SLUG]" >&2; exit 2; }
REPO_SLUG="${2:-$(basename "$REPO_DIR")}"
WFDIR="$REPO_DIR/.github/workflows"

case "$REPO_SLUG" in standards|*/standards) exit 0 ;; esac
[ -d "$WFDIR" ] || exit 0

found=0
for wf in "$WFDIR"/*.yml "$WFDIR"/*.yaml; do
  [ -f "$wf" ] || continue
  # Comment lines are prose, not a step. A KYAML `run:` is one quoted scalar,
  # so the substring test still sees the command inside it.
  versions="$(awk '
    /^[[:space:]]*#/ { next }
    /gh-actions-lock\/releases\/download\// {
      if (match($0, /gh-actions-lock\/releases\/download\/v[0-9]+\.[0-9]+\.[0-9]+/))
        { v = substr($0, RSTART, RLENGTH); sub(/.*\//, "", v); print v }
      else print "unpinned"
      next
    }
    /extension[[:space:]]+install[[:space:]]+github\/gh-actions-lock/ {
      if (match($0, /--pin[[:space:]]+v[0-9]+\.[0-9]+\.[0-9]+/)) {
        v = substr($0, RSTART, RLENGTH); sub(/--pin[[:space:]]+/, "", v); print v
      } else print "unpinned"
    }
  ' "$wf" | sort -u | paste -sd, -)"
  if [ -n "$versions" ]; then
    printf '%s\t%s\t%s\n' "$REPO_SLUG" "${wf#"$REPO_DIR"/}" "$versions"
    found=1
  fi
done
exit "$found"
