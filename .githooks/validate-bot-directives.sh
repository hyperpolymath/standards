#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Bot Directives Migration Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
ERRORS=0

validate_file() {
  local file="$1"
  
  # Check for deprecated bot directives
  if grep -qiE '(codex|gci|other-bot)' "$file" 2>/dev/null; then
    echo "[validate-bot-directives] ERROR: $file contains deprecated bot directives" >&2
    ERRORS=$((ERRORS + 1))
  fi
}

# If staged files provided, only check those
if [ -n "$STAGED_FILES" ]; then
  while IFS=$'\n' read -r file; do
    [ -z "$file" ] && continue
    # Check all text files
    case "$file" in
      *.md|*.txt|*.adoc|*.yml|*.yaml|*.json|*.toml|*.sh|*.bash|*.js|*.ts|*.rs|*.ex|*.exs) ;;
      *) continue ;;
    esac
    [ -f "$file" ] || continue
    validate_file "$file"
  done <<< "$STAGED_FILES"
else
  # Check machine readable directory
  MACHINE_READABLE="$SCAN_PATH/.machine_readable"
  if [ -d "$MACHINE_READABLE" ]; then
    while IFS= read -r file; do
      validate_file "$file"
    done < <(find "$MACHINE_READABLE" -type f \( -name '*.a2ml' -o -name '*.md' -o -name '*.txt' \) 2>/dev/null || true)
  fi
fi

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-bot-directives] All files validated"
exit 0
