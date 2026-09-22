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

# `set -e` + a command substitution is a silence trap: when the grep matches
# nothing it exits 1 and the assignment terminates the script BEFORE `fail()`
# can name the missing assertion — a contract test that cannot say why it
# failed is the same vacuous class it exists to catch. Capture, then assert.
helper_checkout="$(grep -F -A 18 -- '- name: Checkout the pinned Standards policy helpers' "$GOVERNANCE" || true)"
[ -n "$helper_checkout" ] ||
  fail "governance workflow has no step named 'Checkout the pinned Standards policy helpers'"
# GitHub expression is an asserted literal.
# shellcheck disable=SC2016
printf '%s\n' "$helper_checkout" | grep -Eq 'ref: [0-9a-f]{40}$' ||
  fail "governance helpers are not fetched from an immutable commit"
if printf '%s\n' "$helper_checkout" | grep -Eq '^[[:space:]]*ref:[[:space:]]*main[[:space:]]*$'; then
  fail "governance helper execution still follows moving main"
fi

# The assertions above cover the step named `Checkout the pinned Standards
# policy helpers` — the DUPKEY helpers. The lock gate is staged by a DIFFERENT
# step, `Checkout standards for the lock gate`, and until now nothing in this
# file named it: the two steps share the nouns "checkout", "pinned" and
# "standards", so a name-match guard written for one proves nothing about the
# other. That gap is how the lock-gate pin sat at a pre-standards#946 commit
# while this test stayed green. Bind an assertion to the step itself.
#
# `grep -A N` cannot delimit the block: it is 19 lines today, so a fixed N is
# either short of the `ref:` or long enough to reach the NEXT step's `ref:` and
# assert against the wrong pin. Take the range from `- name:` to `- name:`.
lock_gate_block="$(awk '
  index($0, "- name: Checkout standards for the lock gate") { inblock = 1; indent = match($0, /-/); next }
  inblock && /^[[:space:]]*- name:/ && match($0, /-/) == indent { exit }
  inblock { print }
' "$GOVERNANCE")"
[ -n "$lock_gate_block" ] ||
  fail "governance workflow has no step named 'Checkout standards for the lock gate'"
printf '%s\n' "$lock_gate_block" | grep -Eq '^[[:space:]]*ref:[[:space:]]*[0-9a-f]{40}[[:space:]]*$' ||
  fail "the lock gate is not staged from an immutable 40-hex commit"
if printf '%s\n' "$lock_gate_block" | grep -Eq '^[[:space:]]*ref:[[:space:]]*main[[:space:]]*$'; then
  fail "the lock gate follows moving main"
fi
# Shape is not currency: a well-formed SHA can still point at stale tooling, and
# did. The freshness predicate needs git history, so it runs as its own Self
# Test step; assert here only that it still exists and is still wired in.
[ -f "$ROOT/scripts/check-lock-gate-pin-freshness.sh" ] ||
  fail "the lock-gate pin freshness guard is missing — shape alone cannot detect a stale pin"
grep -Fq 'check-lock-gate-pin-freshness.sh' "$ROOT/.github/workflows/self-test.yml" ||
  fail "the lock-gate pin freshness guard is not executed by Self Test"

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
