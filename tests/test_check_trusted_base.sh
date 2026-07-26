#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECK_SCRIPT="${SCRIPT_DIR}/../scripts/check-trusted-base.sh"

TEST_DIR=$(mktemp -d)
trap 'rm -rf "$TEST_DIR"' EXIT

echo "Running check-trusted-base.sh tests..."

# Test 1: Should ignore 'sorry' in string literals
cat << 'EOF' > "${TEST_DIR}/string_literal.lean"
def my_keywords : List String := ["sorry", "axiom"]
EOF

# Provide an empty proof-debt file to satisfy the script's basic requirements
mkdir -p "${TEST_DIR}/docs"
touch "${TEST_DIR}/docs/proof-debt.md"

if "${CHECK_SCRIPT}" "${TEST_DIR}" > /dev/null; then
  echo "PASS: Ignored 'sorry' in string literals."
else
  echo "FAIL: Matched 'sorry' in string literals incorrectly."
  exit 1
fi

# Test 2: Should flag actual 'sorry'
cat << 'EOF' > "${TEST_DIR}/actual_sorry.lean"
theorem fermat : a ^ n + b ^ n = c ^ n := by
  sorry
EOF

if "${CHECK_SCRIPT}" "${TEST_DIR}" > /dev/null; then
  echo "FAIL: Failed to flag actual 'sorry'."
  exit 1
else
  echo "PASS: Flagged actual 'sorry'."
fi

echo "All tests passed!"
