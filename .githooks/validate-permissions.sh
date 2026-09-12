#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Workflow Permissions Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
ERRORS=0

validate_file() {
  local file="$1"
  
  # Check for permissions block
  if ! grep -qE '^permissions:' "$file"; then
    echo "[validate-permissions] ERROR: $file missing permissions block" >&2
    ERRORS=$((ERRORS + 1))
  fi
}

# If staged files provided, only check those
if [ -n "$STAGED_FILES" ]; then
  echo "$STAGED_FILES" | tr ' ' '\n' | while read -r file; do
    [ -z "$file" ] && continue
    # Only check workflow files
    [[ "$file" == *.yml || "$file" == *.yaml ]] || continue
    [[ "$file" == *".github/workflows/"* ]] || continue
    [ -f "$file" ] || continue
    validate_file "$file"
  done
else
  for workflow in $(find "$SCAN_PATH" -path '*/.git/*' -prune -o \
    -path '*/.github/workflows/*.yml' -o -path '*/.github/workflows/*.yaml' \
    -print 2>/dev/null || true); do
    [ -f "$workflow" ] || continue
    validate_file "$workflow"
  done
fi

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-permissions] All workflows have permissions blocks"
exit 0
