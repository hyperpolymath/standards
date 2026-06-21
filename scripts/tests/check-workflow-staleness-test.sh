#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
set -euo pipefail

# Regression test script for check-workflow-staleness.sh

TEST_DIR=$(mktemp -d)
trap 'rm -rf "$TEST_DIR"' EXIT

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECK_SCRIPT="$SCRIPT_DIR/../check-workflow-staleness.sh"

export STALENESS_EXPECTED_SHA="currentsha123"

create_mock_repo() {
  local repo_path="$1"
  mkdir -p "$repo_path/.github/workflows"
}

run_test_case() {
  local desc="$1"
  local expected_exit="$2"
  local mock_repo="$3"
  
  echo "Running test: $desc"
  set +e
  GITHUB_REPOSITORY="hyperpolymath/test-repo" bash "$CHECK_SCRIPT" "$mock_repo" >/dev/null 2>&1
  local exit_code=$?
  set -e
  
  if [ "$exit_code" -ne "$expected_exit" ]; then
    echo "FAIL: $desc (expected exit $expected_exit, got $exit_code)"
    exit 1
  else
    echo "PASS: $desc"
  fi
}

# --- TEST 1: Stale Scorecard reusable pin must fail ---
REPO1="$TEST_DIR/repo1"
create_mock_repo "$REPO1"
cat << EOF > "$REPO1/.github/workflows/scorecard.yml"
name: Scorecard
on: push
jobs:
  scorecard:
    uses: hyperpolymath/standards/.github/workflows/scorecard-reusable.yml@stalesha123
EOF
run_test_case "Stale Scorecard reusable pin must fail" 1 "$REPO1"

# --- TEST 2: Direct Scorecard SARIF upload must fail ---
REPO2="$TEST_DIR/repo2"
create_mock_repo "$REPO2"
cat << EOF > "$REPO2/.github/workflows/scorecard.yml"
name: Scorecard
on: push
jobs:
  scorecard:
    steps:
      - uses: ossf/scorecard-action@abc
      - uses: github/codeql-action/upload-sarif@xyz
EOF
run_test_case "Direct Scorecard SARIF upload must fail" 1 "$REPO2"

# --- TEST 3: Stale Hypatia reusable pin must fail ---
REPO3="$TEST_DIR/repo3"
create_mock_repo "$REPO3"
cat << EOF > "$REPO3/.github/workflows/hypatia.yml"
name: Hypatia
on: push
jobs:
  hypatia:
    uses: hyperpolymath/standards/.github/workflows/hypatia-scan-reusable.yml@stalehypatia123
EOF
run_test_case "Stale Hypatia reusable pin must fail" 1 "$REPO3"

# --- TEST 4: Clean current reusable pin must pass ---
REPO4="$TEST_DIR/repo4"
create_mock_repo "$REPO4"
cat << EOF > "$REPO4/.github/workflows/scorecard.yml"
name: Scorecard
on: push
jobs:
  scorecard:
    uses: hyperpolymath/standards/.github/workflows/scorecard-reusable.yml@currentsha123
EOF
cat << EOF > "$REPO4/.github/workflows/hypatia.yml"
name: Hypatia
on: push
jobs:
  hypatia:
    uses: hyperpolymath/standards/.github/workflows/hypatia-scan-reusable.yml@currentsha123
EOF
run_test_case "Clean current reusable pin must pass" 0 "$REPO4"

# --- TEST 5: scorecard-enforcer.yml in non-standards repo must fail ---
REPO5="$TEST_DIR/repo5"
create_mock_repo "$REPO5"
touch "$REPO5/.github/workflows/scorecard-enforcer.yml"
run_test_case "scorecard-enforcer.yml in non-standards repo must fail" 1 "$REPO5"

echo "All regression tests passed!"
exit 0
