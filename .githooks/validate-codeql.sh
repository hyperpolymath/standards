#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# CodeQL Configuration Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
CODEQL_FILE="$SCAN_PATH/.github/workflows/codeql.yml"
[ -f "$CODEQL_FILE" ] || exit 0

# Detect languages
HAS_JS=$(find "$SCAN_PATH" -name "*.js" -o -name "*.ts" -o -name "*.jsx" -o -name "*.tsx" 2>/dev/null | head -1)
HAS_PY=$(find "$SCAN_PATH" -name "*.py" 2>/dev/null | head -1)
HAS_GO=$(find "$SCAN_PATH" -name "*.go" 2>/dev/null | head -1)
HAS_RS=$(find "$SCAN_PATH" -name "*.rs" 2>/dev/null | head -1)

# Check for unsupported languages
[[ "$HAS_PY" ]] && ! grep -q "language:.*'python'" "$CODEQL_FILE" && echo "[validate-codeql] WARNING: Python files but no Python in CodeQL" >&2
[[ "$HAS_GO" ]] && ! grep -q "language:.*'go'" "$CODEQL_FILE" && echo "[validate-codeql] WARNING: Go files but no Go in CodeQL" >&2
[[ "$HAS_JS" ]] && ! grep -q "language:.*'javascript'" "$CODEQL_FILE" && echo "[validate-codeql] WARNING: JS files but no JavaScript in CodeQL" >&2

# Rust/OCaml not supported
[[ "$HAS_RS" ]] && grep -q "language:.*'rust'" "$CODEQL_FILE" && {
  echo "[validate-codeql] ERROR: CodeQL does not support Rust - use ['actions']" >&2
  exit 1
}

echo "[validate-codeql] ✅ CodeQL configuration valid"
exit 0
