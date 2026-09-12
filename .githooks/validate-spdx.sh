#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX Header Validation for Source Files

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
ERRORS=0

# If staged files provided, only check those
if [ -n "$STAGED_FILES" ]; then
  FILES_TO_CHECK=$STAGED_FILES
else
  # Check all source files
  FILES_TO_CHECK=$(find "$SCAN_PATH" -path '*/.git/*' -prune -o -path '*/node_modules/*' -prune -o \
    -type f \( -name '*.rs' -o -name '*.res' -o -name '*.js' -o -name '*.ts' -o -name '*.sh' \
      -o -name '*.bash' -o -name '*.zig' -o -name '*.ex' -o -name '*.exs' -o -name '*.gleam' \
      -o -name '*.ml' -o -name '*.mli' -o -name '*.adb' -o -name '*.ads' -o -name '*.ncl' \
      -o -name '*.toml' -o -name '*.json' -o -name '*.yaml' -o -name '*.yml' \
    \) -print 2>/dev/null || true)
fi

[ -z "$FILES_TO_CHECK" ] && exit 0

for file in $FILES_TO_CHECK; do
  [ -f "$file" ] || continue
  
  # Check for SPDX header in first 10 lines
  if ! head -10 "$file" | grep -qE '^# SPDX-License-Identifier:'; then
    echo "[validate-spdx] ERROR: $file missing SPDX header" >&2
    ERRORS=$((ERRORS + 1))
  fi
done

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-spdx] ✅ All source files have SPDX headers"
exit 0
