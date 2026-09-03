#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Static regression contract for the reusable governance boundaries.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
GOVERNANCE="$ROOT/.github/workflows/governance-reusable.yml"
FOCUSED="$ROOT/.github/workflows/allowlist-preflight-reusable.yml"
RSR_SEED="$ROOT/rhodium-standard-repositories/.github/workflows/allowlist-preflight.yml"
LOCK_HELPER="$ROOT/scripts/update-actions-lock.sh"
LOCK_GATE="$ROOT/scripts/check-actions-lock-gate.sh"

fail() {
  echo "FAIL: $*" >&2
  exit 1
}

helper_checkout="$(grep -F -A 18 -- '- name: Checkout the pinned Standards policy helpers' "$GOVERNANCE")"
# GitHub expression is an asserted literal.
# shellcheck disable=SC2016
printf '%s\n' "$helper_checkout" | grep -Fq 'ref: ${{ job.workflow_sha }}' ||
  fail "governance helpers are not fetched from job.workflow_sha"
if printf '%s\n' "$helper_checkout" | grep -Eq '^[[:space:]]*ref:[[:space:]]*main[[:space:]]*$'; then
  fail "governance helper execution still follows moving main"
fi

if grep -Fq 'bash scripts/update-actions-lock.sh --verify-local' "$GOVERNANCE"; then
  fail "reusable governance still assumes a consumer-local Standards helper"
fi
# The workflow no longer names `--verify-local` directly: it stages both
# helpers into RUNNER_TEMP and runs the gate, which delegates to the
# authoritative verifier. Assert that composition, and follow the
# `--verify-local` literal to where it now lives.
# RUNNER_TEMP is an asserted workflow literal.
# shellcheck disable=SC2016
grep -Fq 'ACTIONS_LOCK_VERIFIER="$RUNNER_TEMP/update-actions-lock.sh"' "$GOVERNANCE" ||
  fail "reusable governance does not point the lock gate at the staged pinned verifier"
# shellcheck disable=SC2016
grep -Fq 'bash "$RUNNER_TEMP/check-actions-lock-gate.sh"' "$GOVERNANCE" ||
  fail "reusable governance does not execute the staged pinned lock gate"
[ -f "$LOCK_GATE" ] || fail "lock gate script is missing from the pinned helper set"
# VERIFIER is an asserted script literal.
# shellcheck disable=SC2016
grep -Fq 'bash "$VERIFIER" --verify-local' "$LOCK_GATE" ||
  fail "lock gate does not delegate to the authoritative verifier with --verify-local"

for workflow in "$GOVERNANCE" "$FOCUSED"; do
  grep -Fq 'Live Actions policy (credentialed advisory)' "$workflow" ||
    fail "$workflow lacks the distinct live-policy advisory job"
  grep -Fq "if: \${{ env.GH_TOKEN == '' }}" "$workflow" ||
    fail "$workflow does not report a missing optional credential"
  grep -Fq "if: \${{ env.GH_TOKEN != '' }}" "$workflow" ||
    fail "$workflow can run the live API without a credential guard"
  # rc is an asserted workflow-script literal.
  # shellcheck disable=SC2016
  grep -Fq 'if [ "$rc" -eq 3 ]; then' "$workflow" ||
    fail "$workflow conflates API unavailability with a policy verdict"
done

grep -Eq 'uses: hyperpolymath/standards/.github/workflows/allowlist-preflight-reusable.yml@[0-9a-f]{40}$' \
  "$RSR_SEED" || fail "RSR allowlist seed is not a thin immutable reusable-workflow caller"
if grep -Eq 'raw\.githubusercontent\.com/.*/main|curl[[:space:]]' "$RSR_SEED"; then
  fail "RSR allowlist seed still downloads executable policy from moving main"
fi

grep -Fq 'when actions.lock is present, gh actions-lock is authoritative' "$LOCK_HELPER" ||
  fail "lock helper does not state the authoritative-lock contract"
if grep -Fq 'relock-sha-keys.sh' "$LOCK_HELPER"; then
  fail "lock helper still rewrites generated lock keys around legacy inline SHAs"
fi

echo "PASS: reusable governance uses one pinned implementation with focused and suite entry points"
