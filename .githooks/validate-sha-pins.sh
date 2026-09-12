#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SHA-Pinning Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
ERRORS=0

for workflow in $(find "$SCAN_PATH" -path '*/.git/*' -prune -o \
  -path '*/.github/workflows/*.yml' -o -path '*/.github/workflows/*.yaml' \
  -print 2>/dev/null); do
  
  [ -f "$workflow" ] || continue
  
  # Find uses: lines
  while IFS= read -r line; do
    [[ "$line" =~ uses:.*@ ]] || continue
    
    # Check if it has a SHA (40 hex chars)
    if ! echo "$line" | grep -qE '@[a-f0-9]{40}'; then
      echo "[validate-sha-pins] ERROR: Unpinned action in $workflow" >&2
      echo "  $line" >&2
      ERRORS=$((ERRORS + 1))
    fi
  done < "$workflow"
done

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-sha-pins] ✅ All actions are SHA-pinned"
exit 0
