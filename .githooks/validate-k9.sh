#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# K9 Contract Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
ERRORS=0

find "$SCAN_PATH" -path '*/.git/*' -prune -o \( -name '*.k9' -o -name '*.k9.ncl' \) -type f -print 2>/dev/null | while read -r file; do
  # Basic structure check
  if ! grep -qE '^contract' "$file"; then
    echo "[validate-k9] ERROR: $file missing contract declaration" >&2
    ERRORS=$((ERRORS + 1))
  fi
  
  # SPDX header check
  if ! head -5 "$file" | grep -qE '^# SPDX-License-Identifier:'; then
    echo "[validate-k9] WARNING: $file missing SPDX header" >&2
  fi
done

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-k9] ✅ All K9 contracts valid"
exit 0
