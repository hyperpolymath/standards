#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# A2ML Manifest Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STRICT="${INPUT_STRICT:-false}"
ERRORS=0

find "$SCAN_PATH" -path '*/.git/*' -prune -o -name '*.a2ml' -type f -print 2>/dev/null | while read -r file; do
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
done

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-a2ml] ✅ All A2ML files valid"
exit 0
