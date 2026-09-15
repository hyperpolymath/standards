#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Tests for .githooks/validate-spdx-workflows.sh, the pre-commit gate.
#
# ⚠ TEST 1 IS THE REASON THIS EXISTS. The validator ended validate_file() with
#
#     [ "$HAS_SPDX" = false ] && { echo ERROR; ERRORS=$((ERRORS+1)); }
#
# under `set -euo pipefail`. When the header IS present that test is false, the
# && short-circuits, the function returns 1, and `set -e` killed the script —
# silently, with no output. The gate therefore exited non-zero on BOTH valid and
# invalid input: it could never pass a workflow file, and it blocked every
# workflow commit in this repo while printing nothing to say why.
#
# A gate is only proven by a PASSING case. Test 1 is that planted positive;
# without it the bug is invisible, because the failing case looked correct.
set -uo pipefail
HOOK="$(cd "$(dirname "$0")/../.." && pwd)/.githooks/validate-spdx-workflows.sh"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
mkdir -p "$T/.github/workflows"
pass=0; fail=0

ck() { # name expected_exit staged_files
  local out rc
  out="$(cd "$T" && INPUT_STAGED_FILES="$3" bash "$HOOK" 2>&1)"; rc=$?
  if [ "$rc" = "$2" ]; then printf '  ok    %s (exit %s)\n' "$1" "$rc"; pass=$((pass+1))
  else printf '  FAIL  %s (expected exit %s, got %s) output=%s\n' "$1" "$2" "$rc" "${out:-<none>}"; fail=$((fail+1)); fi
}

printf '# SPDX-License-Identifier: MPL-2.0\nname: good\non: push\n' > "$T/.github/workflows/good.yml"
printf 'name: bad\non: push\n'                                      > "$T/.github/workflows/bad.yml"
# gh actions-lock displaces line 1; the header still sits in the leading block.
printf '# This workflow is managed by gh actions-lock.\n# SPDX-License-Identifier: MPL-2.0\nname: locked\n' \
  > "$T/.github/workflows/locked.yml"

echo "validate-spdx-workflows.sh"
ck "PLANTED POSITIVE: valid header must PASS" 0 ".github/workflows/good.yml"
ck "missing header must FAIL"                 1 ".github/workflows/bad.yml"
ck "header below an actions-lock line passes" 0 ".github/workflows/locked.yml"
ck "two valid files pass together"            0 ".github/workflows/good.yml
.github/workflows/locked.yml"
ck "one bad among good still fails"           1 ".github/workflows/good.yml
.github/workflows/bad.yml"
ck "non-workflow staged file is ignored"      0 "README.adoc"

printf '\n%s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
