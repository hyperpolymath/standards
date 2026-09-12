#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX Header Validation for Workflows

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
ERRORS=0

find "$SCAN_PATH" -path '*/.git/*' -prune -o \
  -type f \( -name '*.yml' -o -name '*.yaml' \) \
  -path '*/.github/workflows/*' \
  -print 2>/dev/null | while read -r file; do
  
  # Check for SPDX header in first non-comment line
  HAS_SPDX=false
  while IFS= read -r line; do
    [[ "$line" =~ ^[[:space:]]*$ ]] && continue
    [[ "$line" =~ ^[[:space:]]*# ]] && { echo "$line" | grep -qE 'SPDX-License-Identifier' && HAS_SPDX=true; continue; }
    break
  done < "$file"
  
  [ "$HAS_SPDX" = false ] && {
    echo "[validate-spdx-workflows] ERROR: $file missing SPDX header" >&2
    ERRORS=$((ERRORS + 1))
  }
done

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-spdx-workflows] ✅ All workflow files have SPDX headers"
exit 0
