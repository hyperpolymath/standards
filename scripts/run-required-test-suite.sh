#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Run one required test suite without conflating a missing prerequisite with a
# passing (or skipped) suite.  The aggregate `just test` recipe is an
# attestation about these suites, so each prerequisite and test failure must
# remain observable to its caller.

set -uo pipefail

if [ "$#" -lt 4 ]; then
  echo "usage: $0 <suite-label> <working-directory> <tool> <tool-arg> [tool-arg ...]" >&2
  exit 64
fi

suite_label="$1"
working_directory="$2"
tool="$3"
shift 3

if [ ! -d "$working_directory" ]; then
  echo "MISSING TARGET: $suite_label: $working_directory is not a directory" >&2
  exit 66
fi

if ! command -v "$tool" >/dev/null 2>&1; then
  echo "UNAVAILABLE: $suite_label: required executable '$tool' is not on PATH" >&2
  exit 127
fi

echo "=== $suite_label ==="
(
  cd "$working_directory" || exit 66
  "$tool" "$@"
)
status=$?

if [ "$status" -eq 0 ]; then
  echo "PASS: $suite_label"
  exit 0
fi

echo "FAILED (exit $status): $suite_label" >&2
exit "$status"
