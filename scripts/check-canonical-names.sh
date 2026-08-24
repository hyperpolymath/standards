#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# check-canonical-names.sh — block REINTRODUCTION of deprecated names.
#
# CANONICAL-NAMES.adoc (owner mandate 2026-06-30) deprecates:
#   * 6a2                -> descriptiles
#   * agent_instructions -> bot_directives
# The bulk migration of existing occurrences is chartered separately; this guard
# stops NEW occurrences from landing in the meantime. It inspects only the
# ADDED lines of a diff (grandfathered existing text is untouched), so it can be
# wired into pre-commit and CI without tripping on the not-yet-migrated files.
#
# Usage: check-canonical-names.sh [base-ref]
#   base-ref default: origin/main (CI) then HEAD (pre-commit staged diff).
# Exit: 0 no new deprecated tokens · 1 a deprecated token was added · 2 usage

set -uo pipefail
cd "$(git rev-parse --show-toplevel)" || exit

BASE="${1:-}"
if [ -z "$BASE" ]; then
  if git rev-parse --verify -q origin/main >/dev/null 2>&1; then BASE="origin/main"; else BASE="HEAD"; fi
fi

# Deprecated token -> canonical replacement (for the error message).
declare -A REPL=( ["6a2"]="descriptiles" ["agent_instructions"]="bot_directives" )

# Files that legitimately NAME the deprecated tokens (the mandate itself, this
# guard, migration/charter docs). Excluded from the check.
is_excluded() {
  case "$1" in
    CANONICAL-NAMES.adoc|scripts/check-canonical-names.sh|scripts/tests/*|\
    *MIGRATION*|*migration*|*CHANGELOG*|*/6a2/*) return 0 ;;
  esac
  return 1
}

# Added lines in the working diff vs BASE, per file.
added_diff() { git diff "$BASE" -- . 2>/dev/null; }

rc=0
current_file=""
while IFS= read -r line; do
  case "$line" in
    "+++ b/"*) current_file="${line#+++ b/}" ;;
    "+"*)
      is_excluded "$current_file" && continue
      body="${line#+}"
      # Hypatia baselines identify findings by their literal on-disk path. This
      # narrow matcher records an existing legacy path; it does not name a new
      # product, interface, or source location.
      if [ "$current_file" = ".hypatia-baseline.json" ] &&
         printf '%s' "$body" | grep -Fq '"file_pattern": ".machine_readable/6a2/*.a2ml"'; then
        continue
      fi
      # Skip a line that is DESCRIBING the deprecation rather than using the old
      # name — it also mentions the canonical replacement or the mandate itself
      # (e.g. tooling comments, this guard's own wiring, docs about the rename).
      if printf '%s' "$body" | grep -Eqi 'deprecat|canonical|reintroduc|descriptiles|bot_directives'; then
        continue
      fi
      for tok in "${!REPL[@]}"; do
        # word-ish boundary so e.g. 'v6a2ml' style false hits are limited
        if printf '%s' "$body" | grep -Eq "(^|[^A-Za-z0-9])$tok([^A-Za-z0-9]|$)"; then
          echo "❌ $current_file: reintroduces deprecated '$tok' — use '${REPL[$tok]}' (CANONICAL-NAMES.adoc)"
          echo "   + $body"
          rc=1
        fi
      done ;;
  esac
done < <(added_diff)

if [ "$rc" -eq 0 ]; then
  echo "✅ no deprecated names reintroduced (vs $BASE)"
fi
exit $rc
