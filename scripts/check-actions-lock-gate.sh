#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-actions-lock-gate.sh — the `actions-lock-verify` GATE (R2: SHA pins +
# actions.lock everywhere; spec 2026-09-02-cicd-regularisation-design §6.4).
#
# Three outcomes, never a silent pass:
#   lockfile present   → run the authoritative verifier (`gh actions-lock
#                        --verify-local` via scripts/update-actions-lock.sh) and
#                        propagate its exit status. A corrupted lock goes RED.
#   lockfile absent,   → RED: an unpinned `uses:` is a violation today, lock or
#   unpinned refs        no lock.
#   lockfile absent,   → grace window: `::warning` + "NOT YET ENFORCED" and exit
#   all SHA-pinned       0 until ENFORCE_ACTIONS_LOCK_FROM; `::error` + exit 1
#                        from that date. The sweep (spec §10 step 5) lands the
#                        lockfiles before the date; the date makes the gate
#                        real without red-flooding 300 repos on day one.
#
# Test seams (used by scripts/tests/check-actions-lock-gate-test.sh):
#   LOCK_TODAY                 override today's date (YYYY-MM-DD)
#   ENFORCE_ACTIONS_LOCK_FROM  override the cutoff (default 2026-10-01)
#   ACTIONS_LOCK_VERIFIER      path to update-actions-lock.sh (default: sibling)
#
# Usage: check-actions-lock-gate.sh [WORKFLOWS_DIR]   (default .github/workflows)
set -uo pipefail

WF_DIR="${1:-.github/workflows}"
TODAY="${LOCK_TODAY:-$(date -u +%F)}"
ENFORCE_FROM="${ENFORCE_ACTIONS_LOCK_FROM:-2026-10-01}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VERIFIER="${ACTIONS_LOCK_VERIFIER:-$SCRIPT_DIR/update-actions-lock.sh}"

if [ ! -d "$WF_DIR" ]; then
  echo "::error::actions-lock gate: workflows directory not found: $WF_DIR"
  exit 2
fi

if [ -f "$WF_DIR/actions.lock" ]; then
  if [ ! -f "$VERIFIER" ]; then
    echo "::error::actions-lock gate: lockfile present but verifier not found at $VERIFIER"
    exit 2
  fi
  echo "Lockfile present: running the authoritative verifier ($VERIFIER --verify-local)."
  bash "$VERIFIER" --verify-local
  rc=$?
  if [ "$rc" -ne 0 ]; then
    echo "::error::actions-lock gate: lockfile verification FAILED (exit $rc). Regenerate with scripts/update-actions-lock.sh in the same PR as the uses: change."
    exit "$rc"
  fi
  echo "Immutable direct and transitive lockfile coverage verified."
  exit 0
fi

# No lockfile. Unpinned refs are a violation regardless of the grace window.
unpinned=$(grep -rnE --include='*.yml' --include='*.yaml' "^[[:space:]]+-?[[:space:]]*uses:" "$WF_DIR" \
  | grep -vE "@[a-f0-9]{40}([[:space:]]|$)" \
  | grep -vE "uses:[[:space:]]+(\./|docker://|actions/github-script|hyperpolymath/standards/)" || true)
if [ -n "$unpinned" ]; then
  echo "::error::actions-lock gate: no $WF_DIR/actions.lock AND these refs are not SHA-pinned:"
  echo "$unpinned"
  echo "  Prefer \`gh actions-lock\` (scripts/update-actions-lock.sh): it also locks the"
  echo "  transitive dependencies of composite actions, which an inline SHA cannot express."
  exit 1
fi

if [[ "$TODAY" < "$ENFORCE_FROM" ]]; then
  echo "::warning::actions-lock gate: no $WF_DIR/actions.lock. All refs are SHA-pinned, but the lockfile becomes REQUIRED on $ENFORCE_FROM (today is $TODAY). Run scripts/update-actions-lock.sh."
  echo "NOT YET ENFORCED: lockfile missing but inside the grace window."
  exit 0
fi

echo "::error::actions-lock gate: no $WF_DIR/actions.lock and the grace window closed on $ENFORCE_FROM (today is $TODAY). Run scripts/update-actions-lock.sh and commit the lockfile."
exit 1
