#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# validate-permissions-test.sh — fixture suite for .githooks/validate-permissions.sh,
# covering its YAML-syntax independence (YAML-POLICY Y-1 / Y-3).
#
# The hook used to grep `^permissions:`, which never matches a KYAML workflow
# (every key sits inside `{ … }`), so a KYAML file with permissions was rejected.
# Each case runs the hook on one staged fixture via INPUT_STAGED_FILES.
#
# Run: bash scripts/tests/validate-permissions-test.sh
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOOK="${HOOK:-$SCRIPT_DIR/../../.githooks/validate-permissions.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/.github/workflows"
WF=".github/workflows/ci.yml"

pass=0 fail=0

# expect <label> <want-rc> — runs the hook on $WORK/$WF (stdin is the fixture)
# and checks its exit code.
expect() {
  local label=$1 want=$2 out rc
  cat > "$WORK/$WF"
  out="$(cd "$WORK" && INPUT_STAGED_FILES="$WF" bash "$HOOK" 2>&1)"; rc=$?
  if [ "$rc" -ne "$want" ]; then
    echo "FAIL: $label — expected exit $want, got $rc"
    printf '%s\n' "$out" | sed 's/^/      | /'
    fail=$((fail + 1)); return
  fi
  echo "PASS: $label"; pass=$((pass + 1))
}

expect "block control: top-level permissions passes" 0 <<'EOF'
on: push
permissions:
  contents: read
jobs: {}
EOF

expect "block mutant: no permissions fails" 1 <<'EOF'
on: push
jobs: {}
EOF

expect "KYAML: top-level permissions passes" 0 <<'EOF'
# SPDX-License-Identifier: MPL-2.0
{
  on: "push",
  permissions: {
    contents: "read",
  },
  jobs: {},
}
EOF

expect "KYAML mutant: no permissions fails" 1 <<'EOF'
{
  on: "push",
  jobs: {},
}
EOF

expect "KYAML mutant: job-level permissions only fails" 1 <<'EOF'
{
  on: "push",
  jobs: { a: { permissions: { contents: "read" }, runs-on: "ubuntu-latest" } },
}
EOF

expect "an unparseable workflow fails closed" 1 <<'EOF'
{
  permissions: {},
  jobs: {
EOF

echo
echo "validate-permissions: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
