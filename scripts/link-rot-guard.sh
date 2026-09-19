#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# link-rot-guard.sh — internal-pointer integrity check for the canon and
# the spine (and any repo).
#
# Rationale: the estate has repeatedly shipped docs that point at paths a
# reorg moved (ADR-0003 and archetypes/README.adoc cite
# rhodium-standard-repositories/spec/SCAFFOLD-LIFECYCLE.adoc; the spec now
# lives at 0-canon/rsr/). The deed-pointer sweep (standards f5ba975) proved
# the pattern works when done by hand; this makes it a standing check.
#
# Checks real link syntax only (not code-quoted paths — too noisy):
#   1. AsciiDoc:  link:target[label]   and   image:target[alt]
#   2. Markdown:  [text](target)
# Relative targets only; http(s)/mailto/absolute/fragment-stripped.
#
# Usage: link-rot-guard.sh [REPO_ROOT]

set -uo pipefail
ROOT="${1:-.}"
FAIL=0

# Archive exclusion: rhodium-standard-repositories/ is the LAST COPY of a
# dead upstream (8 of 10 satellites have no external home; standards-map
# note: "ARCHIVE, do not delete"). Its internal links are historical and
# deliberately unmaintained; the guard polices live content.
EXCLUDES=()
case "$(basename "$(cd "$ROOT" && pwd)")" in
  standards) EXCLUDES+=("-path" "$ROOT/rhodium-standard-repositories" -prune -o) ;;
esac

check_target() {
  local src="$1" target="$2"
  target="${target%%#*}"
  target="${target%%\?*}"
  [ -z "$target" ] && return 0
  case "$target" in
    http*|https*|mailto:*|/*) return 0 ;;
  esac
  local dir
  dir="$(dirname "$src")"
  if [ -e "$dir/$target" ] || [ -e "$ROOT/$target" ]; then
    return 0
  fi
  echo "BROKEN  $src -> $target"
  FAIL=1
  return 0
}

# 1. AsciiDoc links and local images
while IFS= read -r -d '' f; do
  while IFS= read -r target; do
    check_target "$f" "$target"
  done < <(grep -oE '(link|image):[^][]+\[' "$f" 2>/dev/null | sed -E 's/^(link|image)://; s/\[$//')
done < <(find "$ROOT" "${EXCLUDES[@]:-}" -path '*/.git' -prune -o -type f -name '*.adoc' -print0)

# 2. Markdown relative links
while IFS= read -r -d '' f; do
  while IFS= read -r target; do
    check_target "$f" "$target"
  done < <(grep -oE '\]\([^)]+\)' "$f" 2>/dev/null | sed -E 's/^\]\(//; s/\)$//' | grep -vE '^(http|mailto|#|/)')
done < <(find "$ROOT" "${EXCLUDES[@]:-}" -path '*/.git' -prune -o -type f -name '*.md' -print0)

echo "----"
if [ "$FAIL" -ne 0 ]; then
  echo "link rot: findings above (exit 1)"
  exit 1
else
  echo "all internal links resolve"
fi
