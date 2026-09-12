#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Bot Directives Migration Validation

set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
ERRORS=0

MACHINE_READABLE="$SCAN_PATH/.machine_readable"

# Check for legacy directory
if [ -d "$MACHINE_READABLE/agent_instructions" ]; then
  echo "[validate-bot-directives] ERROR: Legacy agent_instructions/ found" >&2
  ERRORS=$((ERRORS + 1))
fi

# Check for canonical directory
if [ ! -d "$MACHINE_READABLE/bot_directives" ]; then
  echo "[validate-bot-directives] ERROR: Missing bot_directives/" >&2
  ERRORS=$((ERRORS + 1))
fi

# Check for references
if command -v rg &>/dev/null; then
  REFS=$(rg --hidden --glob '!**/.git/**' --no-line-number 'agent_instructions' "$SCAN_PATH" 2>/dev/null || true)
  [ -n "$REFS" ] && echo "[validate-bot-directives] ERROR: agent_instructions references found" >&2 && ERRORS=$((ERRORS + 1))
elif command -v grep &>/dev/null; then
  REFS=$(find "$SCAN_PATH" -type f -not -path '*/.git/*' -exec grep -l 'agent_instructions' {} \; 2>/dev/null || true)
  [ -n "$REFS" ] && echo "[validate-bot-directives] ERROR: agent_instructions references found" >&2 && ERRORS=$((ERRORS + 1))
fi

[ $ERRORS -gt 0 ] && exit 1
echo "[validate-bot-directives] ✅ Bot directives validation passed"
exit 0
