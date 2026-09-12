#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# K9 Contract Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
ERRORS=0

validate_file() {
  local file="$1"
  
  # Basic structure check
  if ! grep -qE '^contract' "$file"; then
    echo "[validate-k9] ERROR: $file missing contract declaration" >&2
    ERRORS=$((ERRORS + 1))
  fi
  
  # SPDX header check
  if ! head -5 "$file" | grep -qE '^# SPDX-License-Identifier:'; then
    echo "[validate-k9] WARNING: $file missing SPDX header" >&2
  fi
}

# If staged files provided, only check those
if [ -n "$STAGED_FILES" ]; then
  echo "$STAGED_FILES" | tr ' ' '\n' | while read -r file; do
    [ -z "$file" ] && continue
    # Only check .k9 files
    [[ "$file" == *.k9 || "$file" == *.k9.ncl ]] || continue
    [ -f "$file" ] || continue
    validate_file "$file"
  done
else
  find "$SCAN_PATH" -path '*/.git/*' -prune -o \( -name '*.k9' -o -name '*.k9.ncl' \) -type f -print 2>/dev/null | while read -r file; do
    validate_file "$file"
  done
fi

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-k9] All K9 contracts valid"
exit 0
