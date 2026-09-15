#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# A2ML Manifest Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STRICT="${INPUT_STRICT:-false}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
ERRORS=0

validate_file() {
  local file="$1"

  # A machine-generated manifest is the generator's responsibility, not the
  # committer's. Skipping it breaks a hard deadlock in .githooks/pre-commit:
  # the registry-drift gate FAILS every commit until you regenerate and stage
  # .machine_readable/REGISTRY.a2ml, and this validator then REJECTED that very
  # file -- so no ordering satisfied both gates and --no-verify was the only
  # exit. Narrow by construction: 2 of 222 tracked .a2ml files are generated.
  if head -20 "$file" | grep -qE '^#[[:space:]]*GENERATED FILE.*DO NOT EDIT BY HAND'; then
    return 0
  fi

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
}

# If STAGED_FILES is provided, only validate those files
if [ -n "$STAGED_FILES" ]; then
  while IFS=$'\n' read -r file; do
    [ -z "$file" ] && continue
    # Only check .a2ml files
    [[ "$file" == *.a2ml ]] || continue
    # Check if file exists
    [ -f "$file" ] || continue
    validate_file "$file"
  done <<< "$STAGED_FILES"
else
  # Scan entire path for .a2ml files
  while IFS= read -r file; do
    validate_file "$file"
  done < <(find "$SCAN_PATH" -path '*/.git/*' -prune -o -name '*.a2ml' -type f -print 2>/dev/null)
fi

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-a2ml] All A2ML files valid"
exit 0
