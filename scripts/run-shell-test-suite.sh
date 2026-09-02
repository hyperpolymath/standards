#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Canonical fail-closed shell test discovery used by CI and `just test`.
set -uo pipefail

mapfile -t TESTS < <(
  {
    find tests -maxdepth 1 -name '*.sh' -type f
    find scripts/tests -maxdepth 1 -name '*.sh' -type f
  } | sort
)

if [ "${#TESTS[@]}" -eq 0 ]; then
  echo "ERROR: no tests found under tests/ or scripts/tests/ — discovery is broken." >&2
  exit 1
fi

echo "Discovered ${#TESTS[@]} test file(s)."
failed=0

for test_file in "${TESTS[@]}"; do
  echo "::group::$test_file"

  if bash "$test_file"; then
    echo "PASS $test_file"
  else
    status=$?
    echo "::error file=$test_file::$test_file failed (exit $status)"
    failed=$((failed + 1))
  fi

  echo "::endgroup::"
done

echo
if [ "$failed" -gt 0 ]; then
  echo "ERROR: $failed of ${#TESTS[@]} test file(s) failed." >&2
  exit 1
fi

echo "All ${#TESTS[@]} test file(s) passed."
