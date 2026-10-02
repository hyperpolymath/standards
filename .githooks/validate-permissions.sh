#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Workflow Permissions Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
ERRORS=0

# Ask the YAML parser, not a line grep (YAML-POLICY Y-1): `^permissions:` never
# matches a KYAML workflow, where every key sits inside `{ ... }`. Without yq the
# grep is kept -- it can only false-FAIL a KYAML file, never false-pass one.
HAVE_YQ=1
command -v yq >/dev/null 2>&1 || {
  HAVE_YQ=0
  echo "[validate-permissions] WARNING: yq not found -- line grep used; a KYAML workflow will be misreported" >&2
}

# Records an error unless <file> declares a top-level `permissions:` key; a
# file that does not parse is an error too, never a pass.
validate_file() {
  local file="$1" verdict

  if [ "$HAVE_YQ" -eq 1 ]; then
    if ! verdict="$(yq 'has("permissions")' "$file" 2>&1)"; then
      echo "[validate-permissions] ERROR: $file is not parseable as YAML: $verdict" >&2
      ERRORS=$((ERRORS + 1))
    elif [ "$verdict" != "true" ]; then
      echo "[validate-permissions] ERROR: $file missing permissions block" >&2
      ERRORS=$((ERRORS + 1))
    fi
  elif ! grep -qE '^permissions:' "$file"; then
    echo "[validate-permissions] ERROR: $file missing permissions block" >&2
    ERRORS=$((ERRORS + 1))
  fi
}

# If staged files provided, only check those
if [ -n "$STAGED_FILES" ]; then
  while IFS=$'\n' read -r file; do
    [ -z "$file" ] && continue
    # Only check workflow files
    [[ "$file" == *.yml || "$file" == *.yaml ]] || continue
    [[ "$file" == *".github/workflows/"* ]] || continue
    [ -f "$file" ] || continue
    validate_file "$file"
  done <<< "$STAGED_FILES"
else
  while IFS= read -r workflow; do
    [ -f "$workflow" ] || continue
    validate_file "$workflow"
  done < <(find "$SCAN_PATH" -path '*/.git/*' -prune -o \
    -path '*/.github/workflows/*.yml' -o -path '*/.github/workflows/*.yaml' \
    -print 2>/dev/null || true)
fi

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-permissions] All workflows have permissions blocks"
exit 0
