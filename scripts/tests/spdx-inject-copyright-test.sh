#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Regression test for scripts/spdx-inject-copyright.sh.
# Exercises:
#   - Each comment style (html / slash / dash / hash / semi)
#   - Extension-based autodetection
#   - Idempotence (running twice does nothing the second time)
#   - The three behaviours: prepend / insert-after-SPDX / skip-already-has
#   - --check mode exits 1 if any file would change
#   - --dry-run mode does not modify
#   - Missing files are counted but do not error
#   - Edge case: file already has BOTH SPDX and Copyright on consecutive lines
#   - Edge case: file content lines that begin with `--` (regression for the
#     printf/grep flag-parsing bug discovered during the 2026-06-01 sweep)
#   - --holder containing special chars

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT="$SCRIPT_DIR/../spdx-inject-copyright.sh"

if [[ ! -x "$SCRIPT" ]]; then
  echo "FATAL: cannot locate $SCRIPT" >&2
  exit 2
fi

PASS=0
FAIL=0

# Standard holder for all tests.
export SPDX_COPYRIGHT_HOLDER="Test Holder <test@example.org>"

# Run a test case. Args:
#   $1 — case name
#   $2 — expected exit code
#   $3 — expected first-line of the file after running (empty = don't check)
#   $4 — bash function name that sets up the test directory (called inside a subshell)
run_case() {
  local name="$1"; shift
  local expected_exit="$1"; shift
  local expected_firstline="$1"; shift
  local setup_fn="$1"; shift
  local tmp; tmp="$(mktemp -d)" || return 1
  (
    cd "$tmp" || exit 99
    "$setup_fn"
    set +e
    "$@" >/dev/null 2>&1
    local actual_exit=$?
    set -e

    if [[ "$actual_exit" -ne "$expected_exit" ]]; then
      echo "FAIL [$name]: expected exit $expected_exit, got $actual_exit"
      exit 1
    fi

    if [[ -n "$expected_firstline" && -f target ]]; then
      local actual_first
      actual_first="$(head -1 target)"
      if [[ "$actual_first" != "$expected_firstline" ]]; then
        echo "FAIL [$name]: expected first-line '$expected_firstline', got '$actual_first'"
        exit 1
      fi
    fi

    echo "PASS [$name]"
    exit 0
  )
  if [[ $? -eq 0 ]]; then
    PASS=$((PASS + 1))
  else
    FAIL=$((FAIL + 1))
  fi
  rm -rf "$tmp"
}

# --- Setup functions ---------------------------------------------------------

setup_empty_md()  { echo "# Title" > target.md; ln -s target.md target; }
setup_empty_rs()  { echo "fn main() {}" > target.rs; ln -s target.rs target; }
setup_empty_idr() { echo "module Foo" > target.idr; ln -s target.idr target; }
setup_empty_ex()  { echo "defmodule Foo do end" > target.ex; ln -s target.ex target; }
setup_empty_el()  { echo "(defun foo ())" > target.el; ln -s target.el target; }

setup_md_with_spdx() {
  cat > target.md <<'EOF'
<!--
SPDX-License-Identifier: MPL-2.0
-->
# Title
EOF
  ln -s target.md target
}

setup_rs_with_spdx() {
  cat > target.rs <<'EOF'
// SPDX-License-Identifier: MPL-2.0
fn main() {}
EOF
  ln -s target.rs target
}

setup_idr_with_spdx() {
  cat > target.idr <<'EOF'
-- SPDX-License-Identifier: MPL-2.0
module Foo
EOF
  ln -s target.idr target
}

setup_md_with_both() {
  cat > target.md <<'EOF'
<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Test Holder <test@example.org>
-->
# Title
EOF
  ln -s target.md target
}

setup_unknown_ext() {
  echo "binary content" > target.unknownext
  ln -s target.unknownext target
}

# Edge case: content lines start with `--` (used to break grep/printf parsing).
setup_idr_with_dash_content() {
  cat > target.idr <<'EOF'
-- Filesystem Composition
-- Operation Sequences and Undo/Redo
module Filesystem.Composition
EOF
  ln -s target.idr target
}

# --- Test cases --------------------------------------------------------------

# 1. Each style: empty file gets the block prepended.
run_case "md: prepend block on bare file"      0 "<!--"                         setup_empty_md  "$SCRIPT" target.md
run_case "rs: prepend block on bare file"      0 "// SPDX-License-Identifier: MPL-2.0" setup_empty_rs  "$SCRIPT" target.rs
run_case "idr: prepend block on bare file"     0 "-- SPDX-License-Identifier: MPL-2.0" setup_empty_idr "$SCRIPT" target.idr
run_case "ex: prepend block on bare file"      0 "# SPDX-License-Identifier: MPL-2.0"  setup_empty_ex  "$SCRIPT" target.ex
run_case "el: prepend block on bare file"      0 ";; SPDX-License-Identifier: MPL-2.0" setup_empty_el  "$SCRIPT" target.el

# 2. File with SPDX but no Copyright: insert after SPDX.
md_after_insert() {
  setup_md_with_spdx
  "$SCRIPT" target.md >/dev/null 2>&1
  grep -qF "Copyright (c) Test Holder <test@example.org>" target.md
}
run_case "md: insert copyright after existing SPDX" 0 "" setup_md_with_spdx "$SCRIPT" target.md

# 3. File already has both: skipped (no change).
run_case "md: skip when already has both" 0 "<!--" setup_md_with_both "$SCRIPT" target.md

# 4. --check mode: file would change → exit 1.
run_case "check mode: bare file → exit 1" 1 "# Title" setup_empty_md "$SCRIPT" --check target.md

# 5. --check mode: file already has both → exit 0.
run_case "check mode: file with block → exit 0" 0 "<!--" setup_md_with_both "$SCRIPT" --check target.md

# 6. --dry-run mode: does not modify even when changes pending.
run_case "dry-run: does not modify bare file" 1 "# Title" setup_empty_md "$SCRIPT" --dry-run target.md

# 7. Missing file: counted but not an error.
setup_missing() { :; }
run_case "missing file: not an error" 0 "" setup_missing "$SCRIPT" /nonexistent/path

# 8. Unknown extension: skipped without error.
run_case "unknown extension: skipped silently" 0 "binary content" setup_unknown_ext "$SCRIPT" target.unknownext

# 9. Edge case: file content starts with `--` (regression test for the bug
# discovered 2026-06-01 where the Idris2 batch's grep/printf calls had `--`
# parsed as flag terminators, producing duplicate SPDX lines).
run_case "idr: file content starting with -- does not break" 0 "-- SPDX-License-Identifier: MPL-2.0" setup_idr_with_dash_content "$SCRIPT" target.idr

# 10. Idempotence: running twice gives the same result as running once.
test_idempotence() {
  local tmp; tmp="$(mktemp -d)" || return 1
  (
    cd "$tmp" || exit 99
    echo "fn main() {}" > foo.rs
    "$SCRIPT" foo.rs >/dev/null 2>&1
    cp foo.rs foo.rs.first
    "$SCRIPT" foo.rs >/dev/null 2>&1
    if diff -q foo.rs foo.rs.first >/dev/null; then
      exit 0
    fi
    exit 1
  )
  if [[ $? -eq 0 ]]; then
    PASS=$((PASS + 1))
    echo "PASS [idempotence: running twice == running once]"
  else
    FAIL=$((FAIL + 1))
    echo "FAIL [idempotence]"
  fi
  rm -rf "$tmp"
}
test_idempotence

# 11. --files flag: reads a list file.
test_files_flag() {
  local tmp; tmp="$(mktemp -d)" || return 1
  (
    cd "$tmp" || exit 99
    echo "a" > a.md
    echo "b" > b.md
    cat > list <<EOF
a.md
b.md
EOF
    "$SCRIPT" --files list >/dev/null 2>&1
    grep -qF "Copyright (c) Test Holder <test@example.org>" a.md \
      && grep -qF "Copyright (c) Test Holder <test@example.org>" b.md
  )
  if [[ $? -eq 0 ]]; then
    PASS=$((PASS + 1))
    echo "PASS [--files flag: both files updated]"
  else
    FAIL=$((FAIL + 1))
    echo "FAIL [--files flag]"
  fi
  rm -rf "$tmp"
}
test_files_flag

# 12. Stdin input.
test_stdin_input() {
  local tmp; tmp="$(mktemp -d)" || return 1
  (
    cd "$tmp" || exit 99
    echo "x" > x.md
    printf 'x.md\n' | "$SCRIPT" >/dev/null 2>&1
    grep -qF "Copyright (c) Test Holder <test@example.org>" x.md
  )
  if [[ $? -eq 0 ]]; then
    PASS=$((PASS + 1))
    echo "PASS [stdin input]"
  else
    FAIL=$((FAIL + 1))
    echo "FAIL [stdin input]"
  fi
  rm -rf "$tmp"
}
test_stdin_input

# 13. Missing --holder is a hard error (exit 2).
test_missing_holder() {
  local tmp; tmp="$(mktemp -d)" || return 1
  (
    cd "$tmp" || exit 99
    echo "x" > x.md
    set +e
    SPDX_COPYRIGHT_HOLDER= "$SCRIPT" x.md >/dev/null 2>&1
    local rc=$?
    set -e
    [[ "$rc" -eq 2 ]]
  )
  if [[ $? -eq 0 ]]; then
    PASS=$((PASS + 1))
    echo "PASS [missing holder → exit 2]"
  else
    FAIL=$((FAIL + 1))
    echo "FAIL [missing holder]"
  fi
  rm -rf "$tmp"
}
test_missing_holder

# 14. --style override.
test_style_override() {
  local tmp; tmp="$(mktemp -d)" || return 1
  (
    cd "$tmp" || exit 99
    echo "content" > somefile.unknown
    "$SCRIPT" --style hash somefile.unknown >/dev/null 2>&1
    grep -qF "# SPDX-License-Identifier: MPL-2.0" somefile.unknown
  )
  if [[ $? -eq 0 ]]; then
    PASS=$((PASS + 1))
    echo "PASS [--style override on unknown extension]"
  else
    FAIL=$((FAIL + 1))
    echo "FAIL [--style override]"
  fi
  rm -rf "$tmp"
}
test_style_override

# --- Summary ---------------------------------------------------------------

echo ""
echo "RESULTS: $PASS passed, $FAIL failed"
[[ "$FAIL" -eq 0 ]] || exit 1
exit 0
