#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Check repository-name matching without changing the canonical allowlist.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/workflows"
cat > "$WORK/allowed-actions.json" <<'JSON'
{"patterns_allowed": ["Swatinem/rust-cache@*", "ExampleOrg/*", "Tools/build-*@*"]}
JSON
pass=0
fail=0

# Check one workflow reference against the same synthetic policy.
expect() {
  local want="$1" ref="$2" label="$3" rc=0 out
  printf 'jobs:\n  test:\n    steps:\n      - uses: %s\n' "$ref" > "$WORK/workflows/test.yml"
  out="$(bash "$ROOT/scripts/check-allowed-actions.sh" "$WORK/allowed-actions.json" "$WORK/workflows" 2>&1)" || rc=$?
  if [ "$rc" -eq "$want" ]; then
    echo "PASS: $label"
    pass=$((pass + 1))
  else
    echo "FAIL: $label (expected $want, got $rc)"
    printf '%s\n' "$out"
    fail=$((fail + 1))
  fi
}

expect 0 'Swatinem/rust-cache@v2' 'exact spelling remains accepted'
expect 0 'swatinem/rust-cache@6323deb102c322ba6fcbdcafc7e3dddab59af2b6' 'lowercase owner matches mixed-case policy'
expect 0 'SWATINEM/RUST-CACHE@v2' 'owner and repository case are ignored'
expect 0 'exampleorg/repo/.github/workflows/build.yml@Release' 'owner wildcard covers mixed-case reusable workflow'
expect 0 'TOOLS/BUILD-helper@v1' 'repository glob ignores case'
expect 0 'Actions/Checkout@v4' 'GitHub-owned action ignores case'
expect 1 'unlisted/rust-cache@v2' 'unlisted owner is rejected'
expect 1 'swatinem/other@v2' 'unlisted repository is rejected'
expect 1 'exampleorg-extra/repo@v1' 'owner wildcard cannot match a different owner'
expect 1 'tools/test-helper@v1' 'repository glob still rejects nonmatching names'

printf '\nallowlist regression: %s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
