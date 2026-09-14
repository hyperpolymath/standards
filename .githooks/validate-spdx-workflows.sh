#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX Header Validation for Workflows

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
ERRORS=0

validate_file() {
  local file="$1"
  
  # Check for SPDX header in first non-comment line
  HAS_SPDX=false
  while IFS= read -r line; do
    [[ "$line" =~ ^[[:space:]]*$ ]] && continue
    [[ "$line" =~ ^[[:space:]]*# ]] && { echo "$line" | grep -qE 'SPDX-License-Identifier' && HAS_SPDX=true; continue; }
    break
  done < "$file"
  
  # NOTE: this must be an `if`, not `[ ... ] && { ... }`. Under `set -e` the
  # && form makes the function return 1 whenever the header IS present (the
  # test is false and short-circuits), killing the script silently on VALID
  # input. See scripts/tests/validate-spdx-workflows-test.sh.
  if [ "$HAS_SPDX" = false ]; then
    echo "[validate-spdx-workflows] ERROR: $file missing SPDX header" >&2
    ERRORS=$((ERRORS + 1))
  fi
}

# If staged files provided, only check those
if [ -n "$STAGED_FILES" ]; then
  while IFS=$'\n' read -r file; do
    [ -z "$file" ] && continue
    # Only check workflow files
    [[ "$file" == *.yml || "$file" == *.yaml ]] || continue
    [[ "$file" == *".github/workflows/"* ]] || continue
    [ -f "$file" ] || continue
    validate_file "$file"
  done <<< "$STAGED_FILES"
else
  while IFS= read -r file; do
    validate_file "$file"
  done < <(find "$SCAN_PATH" -path '*/.git/*' -prune -o \
    -type f \( -name '*.yml' -o -name '*.yaml' \) \
    -path '*/.github/workflows/*' \
    -print 2>/dev/null)
fi

if [ "$ERRORS" -gt 0 ]; then
  exit 1
fi
echo "[validate-spdx-workflows] All workflow files have SPDX headers"
exit 0
