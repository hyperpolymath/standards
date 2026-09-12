#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SHA-Pinning Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
ERRORS=0

validate_file() {
  local file="$1"
  
  # Check for unpinned actions (uses: without SHA)
  if grep -qE 'uses:[[:space:]]+[a-zA-Z]' "$file" && ! grep -qE 'uses:[[:space:]]+[a-zA-Z].*@[a-f0-9]' "$file"; then
    echo "[validate-sha-pins] ERROR: $file has unpinned actions" >&2
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
echo "[validate-sha-pins] All workflow actions are SHA-pinned"
exit 0
