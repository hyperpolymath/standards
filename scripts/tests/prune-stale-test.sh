#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# prune-stale-test.sh — fixture suite for scripts/prune-stale.sh. The planted
# positive is a stale per-workflow ref that must be dropped; the planted parser
# blind spot is a ref whose exact string survives only in a comment, which the
# parser cannot see and the literal-grep oracle must veto.
#
# Run: bash scripts/tests/prune-stale-test.sh
# TARGET=<path> runs the suite against another copy (used for mutant checks).
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="${TARGET:-$SCRIPT_DIR/../prune-stale.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
pass=0; fail=0
A=1111111111111111111111111111111111111111
B=2222222222222222222222222222222222222222
S=5555555555555555555555555555555555555555

# ok: record and print a passing assertion.
ok()  { echo "PASS: $1"; pass=$((pass + 1)); }
# bad: record and print a failing assertion.
bad() { echo "FAIL: $1"; fail=$((fail + 1)); }

d="$WORK/r/.github/workflows"; mkdir -p "$d"
cat > "$d/ci.yml" <<EOF
jobs:
  call:
    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@$S
  a:
    steps:
      - uses: actions/checkout@$A # v6
      - uses: github/codeql-action/init@$B
EOF
cat > "$d/old.yml" <<'EOF'
jobs:
  a:
    steps:
      - run: true   # previously o/kept@v1
EOF
cat > "$d/actions.lock" <<EOF
workflows:
    '.github/workflows/ci.yml':
        - 'actions/checkout@$A'
        - 'actions/setup-node@$A'
        - 'github/codeql-action@$B'
        - 'hyperpolymath/standards@$S'
    '.github/workflows/old.yml':
        - 'o/gone@v1'
        - 'o/kept@v1'
    '.github/workflows/empty.yml':
        - 'o/gone@v1'
    '.github/workflows/retired.yml':
        - 'o/gone@v1'
    '.github/workflows/retired-empty.yml': []
dependencies:
    'actions/setup-node@$A':
        ref: '$A'
        commit: 'sha1-$A'
EOF
printf 'jobs:\n  a:\n    steps:\n      - run: true\n' > "$d/empty.yml"
L="$d/actions.lock"

out=$(cd "$WORK/r" && bash "$TARGET" 2>&1); st=$?
[ "$st" = 0 ] && ok "exit 0" || bad "exit 0 (got $st) — $out"
! grep -qF "        - 'actions/setup-node@$A'" "$L" && ok "stale step ref pruned (planted positive)" || bad "stale step ref pruned (planted positive)"
grep -qxF "        - 'actions/checkout@$A'" "$L" && ok "used step ref kept" || bad "used step ref kept"
grep -qxF "        - 'github/codeql-action@$B'" "$L" && ok "subpath ref (codeql-action/init) kept" || bad "subpath ref (codeql-action/init) kept"
grep -qxF "        - 'hyperpolymath/standards@$S'" "$L" && ok "job-level reusable ref kept" || bad "job-level reusable ref kept"
grep -qxF "        - 'o/kept@v1'" "$L" && grep -qF 'oracle disagrees, KEEPING o/kept@v1' <<<"$out" \
  && ok "comment-only ref vetoed by the literal-grep oracle" || bad "comment-only ref vetoed by the literal-grep oracle"
! grep -qF "o/gone@v1" "$L" && ok "stale refs dropped from both workflows" || bad "stale refs dropped from both workflows"
grep -qxF "    '.github/workflows/empty.yml': []" "$L" && ok "emptied entry becomes []" || bad "emptied entry becomes []"
! grep -qF "retired.yml" "$L" && ok "key for a deleted workflow with refs dropped" || bad "key for a deleted workflow with refs dropped"
! grep -qF "retired-empty.yml" "$L" && ok "key for a deleted workflow already [] dropped" || bad "key for a deleted workflow already [] dropped"
grep -qxF "    'actions/setup-node@$A':" "$L" && grep -qxF "        commit: 'sha1-$A'" "$L" \
  && ok "dependencies: records untouched" || bad "dependencies: records untouched"
cp "$L" "$WORK/before"; out=$(cd "$WORK/r" && bash "$TARGET" 2>&1)
cmp -s "$L" "$WORK/before" && ok "idempotent: second run changes nothing" || bad "idempotent: second run changes nothing — $out"

g="$WORK/g/.github/workflows"; mkdir -p "$g"
printf 'jobs:\n  a:\n    steps:\n      - uses: actions/checkout@%s\n' "$A" > "$g/ci.yml"
printf 'workflows:\n    '\''.github/workflows/ci.yml'\'':\n        - '\''actions/checkout@%s'\''\n    '\''.github/workflows/gone.yml'\'': []\ndependencies:\n' "$A" > "$g/actions.lock"
(cd "$WORK/g" && bash "$TARGET") >/dev/null 2>&1
! grep -qF "gone.yml" "$g/actions.lock" && grep -qxF "        - 'actions/checkout@$A'" "$g/actions.lock" \
  && ok "only a deleted workflow [] key stale: key dropped, live ref kept" || bad "only a deleted workflow [] key stale: key dropped, live ref kept — $(cat "$g/actions.lock")"

mkdir -p "$WORK/n/.github/workflows"; (cd "$WORK/n" && bash "$TARGET") >/dev/null 2>&1; st=$?
[ "$st" = 1 ] && ok "no lockfile → exit 1" || bad "no lockfile → exit 1 (got $st)"

echo "prune-stale-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
