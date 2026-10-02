#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# validate-actions-lock-test.sh — fixture suite for .githooks/validate-actions-lock.sh,
# covering its YAML-syntax independence (YAML-POLICY Y-1 / Y-3).
#
# The validator used to grep `uses:` lines. On a KYAML workflow
# (`uses: "owner/repo@sha", # v1`) that captured the quote and comma into the
# ref and reported a pinned, locked action as missing. Each case below runs
# against a fixture repo via INPUT_PATH; the KYAML cases fail on that old grep.
#
# Run: bash scripts/tests/validate-actions-lock-test.sh
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VALIDATOR="${VALIDATOR:-$SCRIPT_DIR/../../.githooks/validate-actions-lock.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

LOCKED=3d3c42e5aac5ba805825da76410c181273ba90b1
UNLOCKED=1111111111111111111111111111111111111111

pass=0 fail=0

# expect <label> <want-rc> <needle|-> — runs the validator on $WORK/repo and
# checks its exit code and, unless the needle is "-", a substring of its output.
expect() {
  local label=$1 want=$2 needle=$3 out rc
  out="$(INPUT_PATH="$WORK/repo" bash "$VALIDATOR" 2>&1)"; rc=$?
  if [ "$rc" -ne "$want" ]; then
    echo "FAIL: $label — expected exit $want, got $rc"
    printf '%s\n' "$out" | sed 's/^/      | /'
    fail=$((fail + 1)); return
  fi
  if [ "$needle" != "-" ] && ! printf '%s' "$out" | /usr/bin/grep -qF -- "$needle"; then
    echo "FAIL: $label — exit $rc correct, but output lacked '$needle'"
    printf '%s\n' "$out" | sed 's/^/      | /'
    fail=$((fail + 1)); return
  fi
  echo "PASS: $label"; pass=$((pass + 1))
}

# reset_repo — a fixture repo whose lock keys exactly actions/checkout@$LOCKED.
reset_repo() {
  rm -rf "$WORK/repo"
  mkdir -p "$WORK/repo/.github/workflows"
  cat > "$WORK/repo/.github/workflows/actions.lock" <<EOF
version: 'v0.0.2'
workflows:
    '.github/workflows/ci.yml':
        - 'actions/checkout@$LOCKED'
EOF
}

reset_repo
cat > "$WORK/repo/.github/workflows/ci.yml" <<EOF
name: ci
on: push
permissions: {}
jobs:
  a:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@$LOCKED # v7.0.1
EOF
expect "block control: a locked ref passes" 0 "1 SHA-pinned ref(s) found"

reset_repo
cat > "$WORK/repo/.github/workflows/ci.yml" <<EOF
# SPDX-License-Identifier: MPL-2.0
{
  name: "ci",
  on: "push",
  permissions: {},
  jobs: {
    a: {
      runs-on: "ubuntu-latest",
      steps: [
        { uses: "actions/checkout@$LOCKED", # v7.0.1
        },
      ],
    },
  },
}
EOF
expect "KYAML: a locked, quoted ref passes" 0 "1 SHA-pinned ref(s) found"

reset_repo
cat > "$WORK/repo/.github/workflows/ci.yml" <<EOF
{
  jobs: {
    a: {
      steps: [
        { uses: "actions/checkout@$LOCKED" },
        { uses: "actions/setup-node@$UNLOCKED" },
      ],
    },
  },
}
EOF
expect "KYAML mutant: an unlocked ref is caught" 1 "actions/setup-node@$UNLOCKED"

reset_repo
cat > "$WORK/repo/.github/workflows/ci.yml" <<EOF
jobs:
  a:
    steps:
      - uses: actions/checkout@$LOCKED
      - run: |
          echo "uses: actions/setup-node@$UNLOCKED"
EOF
expect "a uses: inside a run: body is not a ref" 0 "1 SHA-pinned ref(s) found"

reset_repo
printf '{\n  jobs: {\n' > "$WORK/repo/.github/workflows/ci.yml"
expect "an unparseable workflow fails closed" 1 "ci.yml"

echo
echo "validate-actions-lock: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
