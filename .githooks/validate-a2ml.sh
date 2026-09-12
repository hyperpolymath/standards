#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# A2ML Manifest Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STRICT="${INPUT_STRICT:-false}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
ERRORS=0

validate_file() {
  local file="$1"
  
  # Check required fields
  if ! grep -qE '^(agent-id|pedigree):' "$file"; then
    echo "[validate-a2ml] ERROR: $file missing agent-id or pedigree" >&2
    ERRORS=$((ERRORS + 1))
  fi
  
  # Check SPDX header
  if ! head -5 "$file" | grep -qE '^# SPDX-License-Identifier:'; then
    echo "[validate-a2ml] ERROR: $file missing SPDX header" >&2
    ERRORS=$((ERRORS + 1))
  fi
  
  # Check version
  if ! grep -qE '^version:' "$file"; then
    echo "[validate-a2ml] ERROR: $file missing version" >&2
    ERRORS=$((ERRORS + 1))
  fi
}

# If STAGED_FILES is provided, only validate those files
if [ -n "$STAGED_FILES" ]; then
  echo "$STAGED_FILES" | tr ' ' '\n' | while read -r file; do
    [ -z "$file" ] && continue
    # Only check .a2ml files
    [[ "$file" == *.a2ml ]] || continue
    # Check if file exists
    [ -f "$file" ] || continue
    validate_file "$file"
  done
else
  # Scan entire path for .a2ml files
  find "$SCAN_PATH" -path '*/.git/*' -prune -o -name '*.a2ml' -type f -print 2>/dev/null | while read -r file; do
    validate_file "$file"
  done
fi

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-a2ml] All A2ML files valid"
exit 0
