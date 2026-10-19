#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# relock-sha-keys-test.sh — fixture suite for scripts/relock-sha-keys.sh.
#
# The planted positive (standards#1122): the lock already holds a SHA-keyed
# record for a commit, and `gh actions-lock` has added a tag-keyed record for
# the same commit. Re-keying the tag record to the SHA used to leave two
# records under one key, which the lockfile reader rejects ("already
# defined"). The two must merge into one. The plain re-key (no SHA record yet)
# must still happen, and a tag still written in a workflow must keep its key.
#
# Run: bash scripts/tests/relock-sha-keys-test.sh
# TARGET=<path> runs the suite against another copy (used for mutant checks).
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="${TARGET:-$SCRIPT_DIR/../relock-sha-keys.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
pass=0; fail=0
A=1111111111111111111111111111111111111111
B=2222222222222222222222222222222222222222
C=3333333333333333333333333333333333333333
T=4444444444444444444444444444444444444444

# ok: record and print a passing assertion.
ok()  { echo "PASS: $1"; pass=$((pass + 1)); }
# bad: record and print a failing assertion.
bad() { echo "FAIL: $1"; fail=$((fail + 1)); }

d="$WORK/r/.github/workflows"; mkdir -p "$d"
cat > "$d/ci.yml" <<EOF
jobs:
  a:
    steps:
      - uses: actions/checkout@$A # v7
      - uses: actions/cache@$C # v4
      - uses: o/tagged@v2
EOF
cat > "$d/actions.lock" <<EOF
version: 'v0.0.2'
workflows:
    '.github/workflows/ci.yml':
        - 'actions/cache@v4'
        - 'actions/checkout@$A'
        - 'actions/checkout@v7'
        - 'o/tagged@v2'
dependencies:
    'actions/cache@v4':
        ref: 'v4'
        commit: 'sha1-$C'
        owner_id: 1
        repo_id: 3
    'actions/checkout@$A':
        ref: '$A'
        commit: 'sha1-$A'
        owner_id: 1
        repo_id: 2
    'actions/checkout@v7':
        ref: 'v7'
        commit: 'sha1-$A'
        owner_id: 1
        repo_id: 2
    'o/composite@$B':
        ref: '$B'
        commit: 'sha1-$B'
        owner_id: 1
        repo_id: 4
        uses:
            - 'actions/checkout@$A'
            - 'actions/checkout@v7'
    'o/tagged@v2':
        ref: 'v2'
        commit: 'sha1-$T'
        owner_id: 1
        repo_id: 5
EOF
L="$d/actions.lock"

out=$(cd "$WORK/r" && bash "$TARGET" 2>&1); st=$?
[ "$st" = 0 ] && ok "exit 0" || bad "exit 0 (got $st) — $out"
[ "$(grep -cxF "    'actions/checkout@$A':" "$L")" = 1 ] \
  && ok "SHA record and re-keyed tag record merge into one key (planted positive)" \
  || bad "SHA record and re-keyed tag record merge into one key — $(grep -cxF "    'actions/checkout@$A':" "$L") records"
! grep -qF "actions/checkout@v7" "$L" && ok "no tag-keyed checkout left anywhere" || bad "no tag-keyed checkout left anywhere"
[ "$(grep -cxF "        - 'actions/checkout@$A'" "$L")" = 1 ] \
  && ok "the workflow entry lists the merged ref once" || bad "the workflow entry lists the merged ref once"
[ "$(grep -cxF "            - 'actions/checkout@$A'" "$L")" = 1 ] \
  && ok "a record's uses: list holds the merged ref once" || bad "a record's uses: list holds the merged ref once"
[ "$(grep -c "^        commit: 'sha1-$A'" "$L")" = 1 ] \
  && ok "the merged record's body appears once" || bad "the merged record's body appears once"
grep -qxF "        ref: 'v7'" "$L" && ! grep -qxF "        ref: '$A'" "$L" \
  && ok "merged record keeps the symbolic tag ref ('v7'), not the self-SHA ref" \
  || bad "merged record keeps the symbolic tag ref ('v7'), not the self-SHA ref"
grep -qxF "    'actions/cache@$C':" "$L" && ! grep -qF "actions/cache@v4" "$L" \
  && ok "plain re-key (no SHA record yet) still happens" || bad "plain re-key (no SHA record yet) still happens"
grep -qxF "    'o/tagged@v2':" "$L" && grep -qxF "        - 'o/tagged@v2'" "$L" \
  && ok "a tag still written in a workflow keeps its key" || bad "a tag still written in a workflow keeps its key"
[ -z "$(awk '/^    '\''/ { if (seen[$0]++) print }' "$L")" ] \
  && ok "no duplicate keys anywhere" || bad "no duplicate keys anywhere — $(awk '/^    '\''/ { if (seen[$0]++) print }' "$L")"
cp "$L" "$WORK/before"; (cd "$WORK/r" && bash "$TARGET") >/dev/null 2>&1
cmp -s "$L" "$WORK/before" && ok "idempotent: second run changes nothing" || bad "idempotent: second run changes nothing"

mkdir -p "$WORK/n/.github/workflows"; (cd "$WORK/n" && bash "$TARGET") >/dev/null 2>&1; st=$?
[ "$st" = 1 ] && ok "no lockfile → exit 1" || bad "no lockfile → exit 1 (got $st)"

echo "relock-sha-keys-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
