#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX Header Validation for Source Files

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
ERRORS=0

# The extension allowlist is the SINGLE source of truth for "is this a source
# file we require an SPDX header on". It MUST be applied in both modes: staged
# mode previously passed $STAGED_FILES through unfiltered, so any commit that
# touched a non-source file was judged by a rule written for source files. The
# machine-generated .github/workflows/actions.lock ("Do not edit by hand")
# carries no SPDX header and has any added header stripped on the next
# regeneration, so that omission blocked EVERY commit touching the lockfile --
# which is why a Dependabot-caused lockfile desync could sit unrepaired.
is_source_file() {
  case "$1" in
    *.rs|*.res|*.js|*.ts|*.sh|*.bash|*.zig|*.ex|*.exs|*.gleam) return 0 ;;
    *.ml|*.mli|*.adb|*.ads|*.ncl|*.toml|*.json|*.yaml|*.yml)   return 0 ;;
    *) return 1 ;;
  esac
}

if [ -n "$STAGED_FILES" ]; then
  FILES_TO_CHECK=$STAGED_FILES
else
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
  is_source_file "$file" || continue
  
  # Check for SPDX header in first 10 lines
  if ! head -10 "$file" | grep -qE '^# SPDX-License-Identifier:'; then
    echo "[validate-spdx] ERROR: $file missing SPDX header" >&2
    ERRORS=$((ERRORS + 1))
  fi
done

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-spdx] ✅ All source files have SPDX headers"
exit 0
