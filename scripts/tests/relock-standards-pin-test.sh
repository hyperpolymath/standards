#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
set -euo pipefail

# Regression test for relock-standards-pin.sh. Offline: a synthetic standards
# repository and a synthetic consumer lock, with known answers.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RELOCK="$SCRIPT_DIR/../relock-standards-pin.sh"
SYNC="$SCRIPT_DIR/../check-lock-sync.sh"
T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
# fail MESSAGE — report a failed assertion and stop the test.
fail() { echo "FAIL: $*" >&2; exit 1; }

# Synthetic standards: OLD's reusable uses checkout + old/dep; NEW's uses
# checkout + acme/tool, and NEW's own lock records acme/tool.
S="$T/standards"; git init -q -b main "$S"; git -C "$S" config user.email t@t; git -C "$S" config user.name t
mkdir -p "$S/.github/workflows"
cat > "$S/.github/workflows/x-reusable.yml" <<'Y'
on: { workflow_call: {} }
jobs:
  a:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@1111111111111111111111111111111111111111 # v1
      - uses: old/dep/sub@2222222222222222222222222222222222222222
Y
git -C "$S" add -A; git -C "$S" commit -q -m old; OLD=$(git -C "$S" rev-parse HEAD)
cat > "$S/.github/workflows/x-reusable.yml" <<'Y'
on: { workflow_call: {} }
jobs:
  a:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@1111111111111111111111111111111111111111 # v1
      - uses: acme/tool/sub@3333333333333333333333333333333333333333
Y
cat > "$S/.github/workflows/actions.lock" <<'Y'
version: 'v0.0.3'
workflows:
    '.github/workflows/x-reusable.yml':
        - 'acme/tool@3333333333333333333333333333333333333333'
        - 'actions/checkout@1111111111111111111111111111111111111111'
dependencies:
    'acme/tool@3333333333333333333333333333333333333333':
        ref: 'v3'
        commit: 'sha1-3333333333333333333333333333333333333333'
        owner_id: 30
        repo_id: 31
    'actions/checkout@1111111111111111111111111111111111111111':
        ref: 'v1'
        commit: 'sha1-1111111111111111111111111111111111111111'
        owner_id: 10
        repo_id: 11
Y
git -C "$S" add -A; git -C "$S" commit -q -m new; NEW=$(git -C "$S" rev-parse HEAD)

# A consumer whose caller was re-pointed to NEW but whose lock is still on OLD.
consumer() {
  local d="$T/$1/.github/workflows"; rm -rf "$T/$1"; mkdir -p "$d"
  printf 'on: push\njobs:\n  call:\n    uses: hyperpolymath/standards/.github/workflows/x-reusable.yml@%s\n' "$NEW" > "$d/caller.yml"
  printf 'on: push\njobs:\n  b:\n    runs-on: ubuntu-latest\n    steps:\n      - uses: actions/checkout@1111111111111111111111111111111111111111\n' > "$d/own.yml"
  cat > "$d/actions.lock" <<Y
version: 'v0.0.3'
workflows:
    '.github/workflows/caller.yml':
        - 'hyperpolymath/standards@$OLD'
    '.github/workflows/own.yml':
        - 'actions/checkout@1111111111111111111111111111111111111111'
dependencies:
    'actions/checkout@1111111111111111111111111111111111111111':
        ref: 'v1'
        commit: 'sha1-1111111111111111111111111111111111111111'
        owner_id: 10
        repo_id: 11
    'hyperpolymath/standards@$OLD':
        ref: '$OLD'
        commit: 'sha1-$OLD'
        owner_id: 6759885
        repo_id: 1116521501
        uses:
            - 'actions/checkout@1111111111111111111111111111111111111111'
            - 'old/dep@2222222222222222222222222222222222222222'
    'old/dep@2222222222222222222222222222222222222222':
        ref: 'v2'
        commit: 'sha1-2222222222222222222222222222222222222222'
        owner_id: 20
        repo_id: 21
        uses:
            - 'old/sub@6666666666666666666666666666666666666666'
    'old/sub@6666666666666666666666666666666666666666':
        ref: 'v6'
        commit: 'sha1-6666666666666666666666666666666666666666'
        owner_id: 60
        repo_id: 61
    'unrelated/orphan@4444444444444444444444444444444444444444':
        ref: 'v4'
        commit: 'sha1-4444444444444444444444444444444444444444'
        owner_id: 40
        repo_id: 41
Y
  echo "$d"
}

# 1. Known answer.
D=$(consumer ok)
bash "$RELOCK" --to "$NEW" --standards "$S" "$D" >/dev/null
L="$D/actions.lock"
grep -q "^        - 'hyperpolymath/standards@$NEW'\$" "$L" || fail "workflow entry not re-keyed"
grep -q "$OLD" "$L" && fail "OLD standards commit still in the lock"
grep -q "^    'hyperpolymath/standards@$NEW':\$" "$L" || fail "no record for the new pin"
grep -q "^    'acme/tool@3333333333333333333333333333333333333333':\$" "$L" || fail "needed record not copied from standards' lock"
grep -q "old/dep@" "$L" && fail "record only the OLD reusable used was not pruned"
grep -q "old/sub@" "$L" && fail "record kept alive only by a pruned record was not pruned"
grep -q "unrelated/orphan@" "$L" || fail "an orphan the OLD record never named was pruned"
grep -q "owner_id: 6759885" "$L" || fail "owner_id not carried over"
[ "$(awk '/^dependencies:$/{f=1;next} f && /^    \x27/' "$L" | LC_ALL=C sort -c 2>&1; echo $?)" = 0 ] || fail "dependency records not in C-locale key order"
if command -v gawk >/dev/null 2>&1; then
  sed -i '/unrelated\/orphan/,/repo_id: 41/d' "$L"   # the gate checks closure, not unrelated orphans
  bash "$SYNC" "$D" >/dev/null 2>&1 || fail "check-lock-sync.sh rejects the relocked lock"
fi
echo "  PASS known answer (re-key, record rebuilt, needed record copied, OLD-only records pruned transitively, unrelated orphan kept)"

# 2. Idempotent.
D=$(consumer idem); bash "$RELOCK" --to "$NEW" --standards "$S" "$D" >/dev/null
cp "$D/actions.lock" "$T/once"; bash "$RELOCK" --to "$NEW" --standards "$S" "$D" >/dev/null
cmp -s "$T/once" "$D/actions.lock" || fail "second run changed the lock"
echo "  PASS idempotent"

# 3. Refusals leave the lock byte-identical.
refuse_case() { # name mutate-command
  local d; d=$(consumer "$1"); eval "$2"; cp "$d/actions.lock" "$T/before"
  if bash "$RELOCK" --to "$NEW" --standards "$S" "$d" >/dev/null 2>&1; then fail "$1: relock succeeded"; fi
  cmp -s "$T/before" "$d/actions.lock" || fail "$1: lock modified on refusal"
  echo "  PASS refused: $1"
}
refuse_case still-on-old 'sed -i "s/@$NEW/@$OLD/" "$d/caller.yml"'
# A pin whose reusable needs a record that neither lock has: a commit NEW2
# whose reusable uses acme/tool at a ref standards' own lock does not record.
sed -i "s#acme/tool/sub@3333333333333333333333333333333333333333#acme/tool/sub@5555555555555555555555555555555555555555#" "$S/.github/workflows/x-reusable.yml"
git -C "$S" commit -qam new2; NEW2=$(git -C "$S" rev-parse HEAD)
refuse_case record-nowhere 'sed -i "s/@$NEW/@$NEW2/" "$d/caller.yml"; NEW_SAVE=$NEW; NEW=$NEW2'
NEW=$NEW_SAVE
refuse_case no-old-record 'sed -i "/^    .hyperpolymath\/standards@$OLD.:/,/^            - .old\/dep/d" "$d/actions.lock"'

# 4. No lock: nothing to do.
D=$(consumer nolock); rm "$D/actions.lock"
bash "$RELOCK" --to "$NEW" --standards "$S" "$D" >/dev/null || fail "no-lock repo was refused"
[ ! -e "$D/actions.lock" ] || fail "a lock was created"
echo "  PASS no actions.lock: nothing to do"

echo "PASS: relock-standards-pin.sh"
