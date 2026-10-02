#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# complete-job-refs-test.sh — fixture suite for scripts/complete-job-refs.sh.
# The planted positive is a job-level reusable ref that `gh actions-lock`
# v0.1.6 does not write; the negatives are refs that must NOT be added (a
# comment, a local `./` reusable, a step-level action).
#
# Run: bash scripts/tests/complete-job-refs-test.sh
# TARGET=<path> runs the suite against another copy (used for mutant checks).
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="${TARGET:-$SCRIPT_DIR/../complete-job-refs.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
pass=0; fail=0
A=1111111111111111111111111111111111111111
S=5555555555555555555555555555555555555555

# ok: record and print a passing assertion.
ok()  { echo "PASS: $1"; pass=$((pass + 1)); }
# bad: record and print a failing assertion.
bad() { echo "FAIL: $1"; fail=$((fail + 1)); }
# run: execute the target inside fixture repo $1, capturing all output.
run() { (cd "$WORK/$1" && bash "$TARGET") 2>&1; }

# mkrepo: create fixture repo $1 with job-level, local, step-level and commented-out refs.
mkrepo() {
  local d="$WORK/$1/.github/workflows"; mkdir -p "$d"
  cat > "$d/ci.yml" <<EOF
jobs:
  call:
    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@$S
  local:
    uses: ./.github/workflows/local.yml
  steps:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@$A
      # uses: other/repo/.github/workflows/x.yml@$A
EOF
  printf 'jobs:\n  b:\n    uses: "o/r/.github/workflows/y.yml@v2"\n' > "$d/other.yml"
}

echo "=== adds the job-level refs, and nothing else ==="
mkrepo t1
cat > "$WORK/t1/.github/workflows/actions.lock" <<EOF
workflows:
    '.github/workflows/ci.yml':
        - 'actions/checkout@$A'
    '.github/workflows/other.yml': []
dependencies:
    'actions/checkout@$A':
        ref: '$A'
EOF
out=$(run t1); st=$?
L="$WORK/t1/.github/workflows/actions.lock"
[ "$st" = 0 ] && grep -qxF "        - 'hyperpolymath/standards@$S'" "$L" && ok "job-level ref added (exit 0)" || bad "job-level ref added — st=$st $out"
grep -qxF "        - 'o/r@v2'" "$L" && ! grep -qF "    '.github/workflows/other.yml': []" "$L" \
  && ok "quoted ref fills an empty [] entry" || bad "quoted ref fills an empty [] entry"
! grep -qF 'other/repo' "$L" && ok "commented-out ref not added" || bad "commented-out ref not added"
! grep -qF 'local.yml' "$L" && ok "local ./ reusable not added" || bad "local ./ reusable not added"
[ "$(grep -cxF "        - 'actions/checkout@$A'" "$L")" = 1 ] && ok "existing step ref kept, not duplicated" || bad "existing step ref kept, not duplicated"
grep -qxF "        ref: '$A'" "$L" && ok "dependencies: section untouched" || bad "dependencies: section untouched"
cp "$L" "$WORK/t1.before"; out=$(run t1)
cmp -s "$L" "$WORK/t1.before" && grep -qF 'newly present in workflows section: 0' <<<"$out" \
  && ok "idempotent: second run changes nothing" || bad "idempotent: second run changes nothing — $out"

echo "=== a workflow with no lock entry gets one ==="
mkrepo t2
cat > "$WORK/t2/.github/workflows/actions.lock" <<EOF
workflows:
    '.github/workflows/ci.yml':
        - 'actions/checkout@$A'
dependencies:
EOF
run t2 >/dev/null
L="$WORK/t2/.github/workflows/actions.lock"
grep -qxF "    '.github/workflows/other.yml':" "$L" && grep -qxF "        - 'o/r@v2'" "$L" \
  && ok "missing workflow entry is created" || bad "missing workflow entry is created"
[ "$(sed -n '/^dependencies:/=' "$L")" -gt "$(sed -n "/other.yml/=" "$L")" ] \
  && ok "created entry stays inside workflows:" || bad "created entry stays inside workflows:"

echo "=== structural ==="
mkrepo t3; printf 'dependencies:\n' > "$WORK/t3/.github/workflows/actions.lock"
run t3 >/dev/null; st=$?
[ "$st" != 0 ] && grep -qxF 'dependencies:' "$WORK/t3/.github/workflows/actions.lock" \
  && ok "no workflows: section → non-zero, lock unchanged" || bad "no workflows: section → non-zero, lock unchanged (st=$st)"
mkdir -p "$WORK/t4/.github/workflows"; run t4 >/dev/null; st=$?
[ "$st" = 1 ] && ok "no lockfile → exit 1" || bad "no lockfile → exit 1 (got $st)"

echo "complete-job-refs-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
