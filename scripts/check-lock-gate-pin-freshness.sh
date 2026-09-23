#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# check-lock-gate-pin-freshness.sh — the lock gate's own pin must not be stale.
#
# WHY THIS EXISTS
# ---------------
# `governance-reusable.yml` stages the lock-gate tooling from a THIRD pin: not
# the caller's `uses:` ref and not the lockfile's record of it, but a SHA
# hardcoded inside the callee for its own `actions/checkout`. A called reusable
# workflow has no context exposing its own commit, so the hardcode is forced.
#
# That third pin is invisible to every other control. When standards#946 fixed
# `scripts/update-actions-lock.sh`, this pin still pointed at the commit BEFORE
# the fix, so every caller kept being judged by the broken verifier — including
# callers that had just bumped specifically to pick the fix up. The file already
# carried a comment saying "BUMP THIS", but a comment is not a gate.
#
# THE PREDICATE, AND WHY IT IS THIS ONE
# -------------------------------------
# The obvious assertion — "the pin contains the working tree's helpers" —
# DEADLOCKS: a PR that edits a helper would have to pin to its own merge commit,
# which does not exist yet. Unsatisfiable in-PR is the same failure class as a
# required check that can never report.
#
# So the predicate is:
#
#     the pinned commit must already contain everything that is on the
#     COMPARE ref (main), over exactly the paths the step stages.
#
#   * on a pull request, COMPARE is the PR's base SHA. A PR that edits a helper
#     PASSES — its edit is not on base yet. A PR opened while main is already
#     stale is FORCED to bump, and can, because the needed commit exists.
#   * on a push to main, COMPARE is HEAD. Red exactly when a helper change has
#     just landed and the bump is owed; healed by the next PR, which the
#     pull_request run will not let through unbumped.
#
# The comparison is path-scoped, so a rebase or any unrelated commit cannot fail
# it — only a real divergence in the staged tooling can.
#
# SCOPE IS TAKEN FROM THE STEP, NOT HARDCODED
# -------------------------------------------
# The paths compared are read out of the step's own `sparse-checkout:` list, so
# adding a file to what the gate stages automatically extends what this guard
# protects. A hardcoded list here would be a guard asking a different question
# than its consumer, which is the exact trap this file belongs to.
#
# Usage:  check-lock-gate-pin-freshness.sh [COMPARE_REF]   (default: origin/main)

set -uo pipefail

STEP_NAME='Checkout standards for the lock gate'

fail() {
  echo "::error::lock-gate pin freshness: $*" >&2
  exit 1
}

# Resolve the workflow path INSIDE the function, never at script load: a fixture
# override exported by a test after the top-level assignment would otherwise be
# ignored and every mutant would silently read the real tree and "pass".
workflow_path() {
  local root
  root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
  printf '%s\n' "${LOCK_GATE_WORKFLOW:-$root/.github/workflows/governance-reusable.yml}"
}

# Print the step's YAML block: from its `- name:` line to the next `- name:` at
# the same indent, exclusive. `grep -A N` cannot do this — the block is 19 lines
# today and any fixed N is either short of the `ref:` or long enough to capture
# the NEXT step's `ref:` and assert against the wrong pin.
step_block() {
  awk -v want="- name: $STEP_NAME" '
    index($0, want) { inblock = 1; indent = match($0, /-/); next }
    inblock && /^[[:space:]]*- name:/ && match($0, /-/) == indent { exit }
    inblock { print }
  ' "$(workflow_path)"
}

main() {
  local compare="${1:-origin/main}"
  local wf block pin paths diverged

  wf="$(workflow_path)"
  [ -f "$wf" ] || fail "workflow not found: $wf"

  block="$(step_block)"
  [ -n "$block" ] ||
    fail "no step named '$STEP_NAME' in $wf — the guard has lost its subject; \
rename it here too rather than deleting the assertion"

  pin="$(printf '%s\n' "$block" | sed -n 's/^[[:space:]]*ref:[[:space:]]*\([^[:space:]#]*\).*/\1/p' | head -1)"
  [ -n "$pin" ] || fail "step '$STEP_NAME' has no 'ref:' — it would follow the default branch"
  printf '%s' "$pin" | grep -Eq '^[0-9a-f]{40}$' ||
    fail "step '$STEP_NAME' is pinned to '$pin', not an immutable 40-hex commit"

  # The staged scope IS the guarded scope.
  paths="$(printf '%s\n' "$block" | awk '
    /^[[:space:]]*sparse-checkout:[[:space:]]*\|/ { inlist = 1; next }
    inlist && /^[[:space:]]*[a-z-]+:/ { inlist = 0 }
    inlist && NF { gsub(/^[[:space:]]+|[[:space:]]+$/, ""); print }
  ')"
  [ -n "$paths" ] || fail "step '$STEP_NAME' stages no sparse-checkout paths — nothing to compare"

  git rev-parse --verify --quiet "$compare^{commit}" >/dev/null ||
    fail "compare ref '$compare' is not resolvable in this clone"

  # A missing pin object must FAIL, never skip: a skip is indistinguishable from
  # a pass and this guard exists because an unasserted pin rotted unnoticed.
  if ! git cat-file -e "$pin^{commit}" 2>/dev/null; then
    git fetch --quiet --depth=1 origin "$pin" 2>/dev/null || true
    git cat-file -e "$pin^{commit}" 2>/dev/null ||
      fail "pinned commit $pin is not present and could not be fetched — \
give the checkout 'fetch-depth: 0' or grant the fetch network access; \
this guard does not pass on an unverifiable pin"
  fi

  # shellcheck disable=SC2086
  diverged="$(git diff --name-only "$pin" "$compare" -- $paths)"

  if [ -n "$diverged" ]; then
    echo "::error::The lock gate is staged from $pin, which does NOT contain what is already on $compare." >&2
    echo "Stale in the pinned tree:" >&2
    printf '  %s\n' $diverged >&2
    echo >&2
    echo "Every caller of governance-reusable.yml is being judged by that older tooling," >&2
    echo "including callers that bumped their own pin specifically to pick up the fix." >&2
    echo "Cure: set 'ref:' under '$STEP_NAME' to a commit containing the above" >&2
    echo "(usually the current tip of main), in this PR." >&2
    exit 1
  fi

  echo "PASS: lock-gate pin $pin contains $compare over: $(printf '%s ' $paths)"
}

main "$@"
