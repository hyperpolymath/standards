#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# lock-selfcheck-test.sh — fixture suite for scripts/lock-selfcheck.sh, covering
# its YAML-syntax independence (YAML-POLICY Y-1 / Y-3).
#
# lock-selfcheck reads workflows at a commit and asks whether every action ref
# is keyed in that commit's own actions.lock. Its old line grep captured
# `…@sha",` from a KYAML workflow and called a self-consistent commit POISON.
# Each case is one commit in a throwaway repo, checked via STANDARDS_DIR.
#
# Run: bash scripts/tests/lock-selfcheck-test.sh
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SELFCHECK="${SELFCHECK:-$SCRIPT_DIR/../lock-selfcheck.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

LOCKED=3d3c42e5aac5ba805825da76410c181273ba90b1
UNLOCKED=1111111111111111111111111111111111111111
REPO="$WORK/repo"
WF="$REPO/.github/workflows"

mkdir -p "$WF"
git -C "$REPO" init -q -b main .
git -C "$REPO" config user.email t@example.com
git -C "$REPO" config user.name T
git -C "$REPO" config commit.gpgsign false
cat > "$WF/actions.lock" <<EOF
version: 'v0.0.2'
workflows:
    '.github/workflows/ci.yml':
        - 'actions/checkout@$LOCKED'
EOF

pass=0 fail=0

# commit_ci — commits stdin as .github/workflows/ci.yml and prints the new SHA.
commit_ci() {
  cat > "$WF/ci.yml"
  git -C "$REPO" add -A
  git -C "$REPO" commit -q -m fixture
  git -C "$REPO" rev-parse HEAD
}

# expect <label> <sha> <want-rc> <needle> — runs lock-selfcheck on one commit
# and checks its exit code and a substring of its output.
expect() {
  local label=$1 sha=$2 want=$3 needle=$4 out rc
  out="$(cd "$WORK" && STANDARDS_DIR="$REPO" bash "$SELFCHECK" "$sha" 2>&1)"; rc=$?
  if [ "$rc" -ne "$want" ] || ! printf '%s' "$out" | /usr/bin/grep -qF -- "$needle"; then
    echo "FAIL: $label — wanted exit $want and '$needle', got exit $rc"
    printf '%s\n' "$out" | sed 's/^/      | /'
    fail=$((fail + 1)); return
  fi
  echo "PASS: $label"; pass=$((pass + 1))
}

sha=$(commit_ci <<EOF
jobs:
  a:
    steps:
      - uses: actions/checkout@$LOCKED # v7.0.1
EOF
)
expect "block control: keyed ref is self-consistent" "$sha" 0 "VERDICT: SELF-CONSISTENT"

sha=$(commit_ci <<EOF
# SPDX-License-Identifier: MPL-2.0
{
  jobs: {
    a: {
      steps: [
        { uses: "actions/checkout@$LOCKED", # v7.0.1
        },
      ],
    },
  },
}
EOF
)
expect "KYAML: keyed, quoted ref is self-consistent" "$sha" 0 "VERDICT: SELF-CONSISTENT"

sha=$(commit_ci <<EOF
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
)
expect "KYAML mutant: unkeyed ref is POISON" "$sha" 1 "uses  actions/setup-node@$UNLOCKED"

sha=$(commit_ci <<EOF
jobs:
  a:
    steps:
      - uses: actions/checkout@$LOCKED
      - run: |
          echo "uses: actions/setup-node@$UNLOCKED"
EOF
)
expect "a uses: inside a run: body is not a ref" "$sha" 0 "VERDICT: SELF-CONSISTENT"

sha=$(printf '{\n  jobs: {\n' | commit_ci)
expect "an unparseable workflow is UNEXAMINED, not consistent" "$sha" 1 "VERDICT: UNEXAMINED"

echo
echo "lock-selfcheck: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
