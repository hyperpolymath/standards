#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Workflow Permissions Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
ERRORS=0

for workflow in $(find "$SCAN_PATH" -path '*/.git/*' -prune -o \
  -path '*/.github/workflows/*.yml' -o -path '*/.github/workflows/*.yaml' \
  -print 2>/dev/null); do
  
  [ -f "$workflow" ] || continue
  
  if ! grep -qE '^permissions:' "$workflow"; then
    echo "[validate-permissions] ERROR: $workflow missing permissions" >&2
    ERRORS=$((ERRORS + 1))
  fi
done

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-permissions] ✅ All workflows have permissions"
exit 0
