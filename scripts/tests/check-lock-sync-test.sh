#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Test suite for scripts/check-lock-sync.sh
# Part of hyperpolymath/standards#968 campaign

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
CHECK_SCRIPT="$REPO_ROOT/check-lock-sync.sh"

# Load test helpers if available
if [ -f "$SCRIPT_DIR/test-helpers.sh" ]; then
  # shellcheck source=scripts/tests/test-helpers.sh
  source "$SCRIPT_DIR/test-helpers.sh"
fi

PASS=0
FAIL=0
TOTAL=0

fail() {
  echo "FAIL: $*"
  FAIL=$((FAIL + 1))
  TOTAL=$((TOTAL + 1))
}

pass() {
  echo "PASS: $*"
  PASS=$((PASS + 1))
  TOTAL=$((TOTAL + 1))
}

echo "=== Test suite for check-lock-sync.sh ==="

# Test 1: Script exists and is executable
if [ -x "$CHECK_SCRIPT" ]; then
  pass "Script exists and is executable"
else
  fail "Script missing or not executable"
fi

# Test 2: Script has SPDX header
grep -q "SPDX-License-Identifier: MPL-2.0" "$CHECK_SCRIPT" && \
  pass "Script has SPDX license header" || \
  fail "Script missing SPDX license header"

# Test 3: Script exits 0 when lockfile is in sync (test with current repo if it has a lockfile)
if [ -f "$REPO_ROOT/.github/workflows/actions.lock" ]; then
  if "$CHECK_SCRIPT" "$REPO_ROOT/.github/workflows" >/dev/null 2>&1; then
    pass "Script exits 0 when lockfile is in sync (self-test)"
  else
    fail "Script failed on current repo (may be out of sync, or script error)"
  fi
else
  echo "SKIP: No actions.lock in current repo, cannot test sync case"
fi

# Test 4: Script exits 1 when no lockfile exists
mkdir -p /tmp/test-lock-sync-empty
cd /tmp/test-lock-sync-empty
mkdir -p .github/workflows
touch .github/workflows/test.yml
if "$CHECK_SCRIPT" .github/workflows >/dev/null 2>&1; then
  fail "Script should exit 1 when no lockfile exists"
else
  pass "Script exits 1 when no lockfile exists"
fi
rm -rf /tmp/test-lock-sync-empty

# Test 5: Script handles empty workflows directory gracefully
# Note: An empty workflows directory with just actions.lock is an edge case.
# The script exits 0 because there are no workflows to validate.
mkdir -p /tmp/test-lock-sync-no-wf
cd /tmp/test-lock-sync-no-wf
mkdir -p .github/workflows
touch .github/workflows/actions.lock
if "$CHECK_SCRIPT" .github/workflows >/dev/null 2>&1; then
  pass "Script handles empty workflows directory (exits 0 - no workflows to check)"
else
  fail "Script failed unexpectedly on empty workflows directory"
fi
rm -rf /tmp/test-lock-sync-no-wf

# Test 6: Script has proper documentation
if grep -q "standards#968\|issue #968" "$CHECK_SCRIPT"; then
  pass "Script references issue #968"
else
  fail "Script missing reference to issue #968"
fi

if grep -q "burble#224" "$CHECK_SCRIPT"; then
  pass "Script references burble#224"
else
  fail "Script missing reference to burble#224"
fi

echo ""
echo "=== Results ==="
echo "PASS: $PASS"
echo "FAIL: $FAIL"
echo "TOTAL: $TOTAL"

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi

exit 0
