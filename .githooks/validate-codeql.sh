#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# CodeQL Configuration Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
CODEQL_FILE="$SCAN_PATH/.github/workflows/codeql.yml"
[ -f "$CODEQL_FILE" ] || exit 0

# Detect languages.
#
# `find ... | head -1` is NOT safe here. This script runs under
# `set -euo pipefail`; when `head` exits after the first match, `find` is killed
# by SIGPIPE, the pipeline reports 141, and `set -e` terminates this script with
# NO OUTPUT. The hook passes an ABSOLUTE INPUT_PATH, under which a match is
# found early with tree left to walk, so the gate failed 6 times out of 6 —
# silently, on a perfectly valid repository. `-print -quit` stops find itself
# after the first hit and needs no pipe.
#
# The -name alternations are also parenthesised: without the group, `-o` binds
# loosely and the implicit -print does not apply as intended.
first_match() {
  find "$SCAN_PATH" \( "$@" \) -print -quit 2>/dev/null
}
HAS_JS=$(first_match -name '*.js' -o -name '*.ts' -o -name '*.jsx' -o -name '*.tsx')
HAS_PY=$(first_match -name '*.py')
HAS_GO=$(first_match -name '*.go')
HAS_RS=$(first_match -name '*.rs')

# Check for unsupported languages.
#
# These must be `if`, not `[[ ... ]] && ... && echo`. Under `set -e` an AND-OR
# list whose guard is false returns 1, which terminates the script — so the
# "no Python present" case would abort the gate instead of passing it.
if [ -n "$HAS_PY" ] && ! grep -q "language:.*'python'" "$CODEQL_FILE"; then
  echo "[validate-codeql] WARNING: Python files but no Python in CodeQL" >&2
fi
if [ -n "$HAS_GO" ] && ! grep -q "language:.*'go'" "$CODEQL_FILE"; then
  echo "[validate-codeql] WARNING: Go files but no Go in CodeQL" >&2
fi
if [ -n "$HAS_JS" ] && ! grep -q "language:.*'javascript'" "$CODEQL_FILE"; then
  echo "[validate-codeql] WARNING: JS files but no JavaScript in CodeQL" >&2
fi

# Rust/OCaml not supported
if [ -n "$HAS_RS" ] && grep -q "language:.*'rust'" "$CODEQL_FILE"; then
  echo "[validate-codeql] ERROR: CodeQL does not support Rust - use ['actions']" >&2
  exit 1
fi

echo "[validate-codeql] ✅ CodeQL configuration valid"
exit 0
