#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# cicd-squabbler-contract-test.sh — the CI wiring of cicd-squabbler must
# preserve the tool's one invariant (squabble != bypass) at the workflow
# layer, where a well-meaning edit could quietly break it:
#
#   * CI must never pass --apply (propose-only; enactment is a human commit);
#   * the binary must come from the pinned composite action (40-hex commit),
#     never a tag, a branch, or an ad-hoc download;
#   * chains exit 4 (cycle / dead upstream) must fail the job — demoting it
#     to a warning would make the gate advisory in exactly the case it
#     exists for;
#   * tool failures (exit 2) must fail too, or a broken advisor reads as an
#     all-clear;
#   * no job may hold a write grant, and no secrets may be inherited;
#   * the caller must carry no paths: filter (a filtered gate never
#     reports on PRs that miss the filter);
#   * both workflows must be present in actions.lock, or GitHub refuses
#     the run at startup and the gate vanishes silently.
#
# Structural (grep) assertions, no network: this is a contract about the
# files, exercised wherever bash runs.

set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"
reusable="$root/.github/workflows/cicd-squabbler-reusable.yml"
caller="$root/.github/workflows/cicd-squabbler.yml"
lock="$root/.github/workflows/actions.lock"
pass=0; fail=0

t() { # label condition...
  local label="$1"; shift
  if "$@" >/dev/null 2>&1; then pass=$((pass+1)); echo "PASS $label"
  else fail=$((fail+1)); echo "FAIL $label"; fi
}
# -e is load-bearing: patterns like `--apply` would otherwise be parsed as
# grep flags, making the `lacks` assertions pass without checking anything.
has()   { grep -qF -e "$2" "$1"; }
hasre() { grep -qE -e "$2" "$1"; }
lacks() { ! grep -qF -e "$2" "$1"; }

for f in "$reusable" "$caller" "$lock"; do
  t "exists: ${f##*/}" test -f "$f"
done

# Pinned install, and only pinned installs.
t "squabble action is commit-pinned" hasre "$reusable" \
  'uses: hyperpolymath/cicd-squabbler/\.github/actions/squabble@[0-9a-f]{40}'
t "no tag/branch squabble ref" lacks "$reusable" 'squabble@main'
t "no tag-pinned squabble ref" lacks "$reusable" 'squabble@v'
t "no ad-hoc binary download" lacks "$reusable" 'releases/download'

# Propose-only, forever, from CI. Comment lines are stripped first: the
# workflows explain the rule using the very flag they forbid, and the
# explanation must not read as the offence.
noflag() { ! grep -v '^[[:space:]]*#' "$1" | grep -qF -e "$2"; }
# Firing fixture first: the matcher must SEE a planted --apply, or the
# assertions below could pass blind (they once did — grep parsed the
# pattern as a flag). Silence fixture: a comment mentioning it is fine.
planted="$(mktemp)"; trap 'rm -f "$planted"' EXIT
printf '      - run: squabble fight o/r 1 --apply\n' > "$planted"
if noflag "$planted" '--apply'; then
  fail=$((fail+1)); echo "FAIL matcher is blind to a planted --apply"
else
  pass=$((pass+1)); echo "PASS matcher sees a planted --apply"
fi
printf '      # never passes --apply from CI\n' > "$planted"
t "matcher ignores --apply in comments" noflag "$planted" '--apply'
t "reusable never passes --apply" noflag "$reusable" '--apply'
t "caller never passes --apply" noflag "$caller" '--apply'
t "reusable never summons boj experts" noflag "$reusable" '--summon'

# The exit-code contract (squabble-cli: 2 tool failure, 3 no gate,
# 4 blocking chain finding) is honoured, not flattened.
t "chains exit 4 fails the job" has "$reusable" 'if [ "$rc" -eq 4 ]'
t "chains other non-zero fails the job" has "$reusable" 'squabble chains failed to run'
t "fight exit 2 fails the job" has "$reusable" 'if [ "$rc" -eq 2 ]'
t "fight exit 3 is a warning, not a failure" has "$reusable" 'if [ "$rc" -eq 3 ]'
t "fight refuses a non-numeric PR number" has "$reusable" '*[!0-9]*'

# Least privilege: no write scope anywhere, no secret inheritance.
t "reusable holds no write grant" lacks "$reusable" ': write'
t "caller holds no write grant" lacks "$caller" ': write'
t "caller does not inherit secrets" lacks "$caller" 'secrets: inherit'
t "reusable references no secrets" lacks "$reusable" 'secrets.'
t "checkouts do not persist credentials" has "$reusable" 'persist-credentials: false'

# Gate shape: unfiltered, cancellable, time-boxed.
# Anchored to an indented YAML key: the prose "no `paths:` filter" in the
# header is the explanation of this rule, not a violation of it.
t "caller has no paths filter" bash -c '! grep -qE "^[[:space:]]+paths(-ignore)?:" "$1"' _ "$caller"
t "caller has concurrency" has "$caller" 'cancel-in-progress: true'
t "reusable jobs are time-boxed" has "$reusable" 'timeout-minutes: 10'
t "caller calls the local reusable" has "$caller" \
  'uses: ./.github/workflows/cicd-squabbler-reusable.yml'

# The lock knows both files, with the subpath normalised to repo@sha.
t "lock lists the reusable" has "$lock" "'.github/workflows/cicd-squabbler-reusable.yml':"
t "lock lists the caller as pin-free" has "$lock" "'.github/workflows/cicd-squabbler.yml': []"
t "lock records the squabbler pin" hasre "$lock" \
  "'hyperpolymath/cicd-squabbler@[0-9a-f]{40}'"

# The pin in the workflow and the pin in the lock are the same commit, or
# one of them is stale and the run dies at startup.
wf_pin="$(grep -oE 'squabble@[0-9a-f]{40}' "$reusable" | head -1 | cut -d@ -f2)"
lock_pin="$(grep -oE "cicd-squabbler@[0-9a-f]{40}" "$lock" | head -1 | cut -d@ -f2)"
t "workflow pin equals lock pin" test "$wf_pin" = "$lock_pin"

# A full-SHA lock key must record a containing branch/tag in `ref:`; when
# `ref:` repeats the SHA, GitHub rejects the action at job setup as an
# "invalid lockfile pin" (standards#1271, measured on this workflow).
lock_ref="$(grep -A1 -F "'hyperpolymath/cicd-squabbler@$wf_pin':" "$lock" \
  | sed -n "s/.*ref: '\([^']*\)'.*/\1/p" | head -1)"
t "lock recorded ref is not the pin SHA" \
  bash -c '[ -n "$1" ] && [ "$1" != "$2" ] && ! printf "%s" "$1" | grep -qE "^[0-9a-f]{40}$"' \
  _ "$lock_ref" "$wf_pin"

printf '\ncicd-squabbler contract tests: %s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
