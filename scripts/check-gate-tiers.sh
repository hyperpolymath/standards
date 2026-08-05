#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Gate-tier invariant lint (docs/CICD-SIGNAL-DISCIPLINE.adoc).
#
#   🔴 GATE:     MUST be a required status check
#   🟡 CHECK:    MUST NOT be required
#   ⚪ ADVISORY: MUST NOT be required
#   📅 PERIODIC: MUST NOT be required
#
# A required status check matches on the CONTEXT, which is the JOB name — not
# the workflow name. The first version of this lint compared workflow names to
# required contexts and reported every correctly-wired gate as a violation.
# It now reads each tiered workflow and resolves its actual job names.
#
# Reusable-workflow callers emit `<caller job> / <reusable job>`, which cannot
# be known without a run, so a caller job is treated as satisfied when any
# required context starts with "<job> / ".
#
# Report-only by default; --strict exits non-zero.
# Usage: check-gate-tiers.sh [--strict] OWNER/REPO [OWNER/REPO ...]
set -uo pipefail
STRICT=0
[ "${1:-}" = "--strict" ] && { STRICT=1; shift; }
[ $# -eq 0 ] && { echo "usage: $0 [--strict] OWNER/REPO..." >&2; exit 2; }

TOTAL=0
for R in "$@"; do
  REQ=$(mktemp); DEF=$(mktemp)
  DB=$(gh api "repos/$R" -q .default_branch 2>/dev/null)
  gh api "repos/$R/branches/$DB/protection" -q '.required_status_checks.checks[]?.context' 2>/dev/null >> "$REQ"
  for ID in $(gh api "repos/$R/rulesets" -q '.[].id' 2>/dev/null); do
    gh api "repos/$R/rulesets/$ID" \
      -q '.rules[]?|select(.type=="required_status_checks")|.parameters.required_status_checks[].context' \
      2>/dev/null >> "$REQ"
  done
  sort -u -o "$REQ" "$REQ"

  gh api "repos/$R/actions/workflows?per_page=100" -q '.workflows[]|[.name,.path]|@tsv' 2>/dev/null > "$DEF"
  while IFS=$'\t' read -r NAME PATHW; do
    case "$NAME" in
      "🔴"*) TIER=GATE ;; "🟡"*) TIER=CHECK ;;
      "⚪"*) TIER=ADVISORY ;; "📅"*) TIER=PERIODIC ;; *) continue ;;
    esac
    JOBS=$(gh api "repos/$R/contents/$PATHW" -q .content 2>/dev/null | base64 -d 2>/dev/null | python3 -c "
import sys,yaml
try: d=yaml.safe_load(sys.stdin)
except Exception: raise SystemExit
if isinstance(d,dict):
    for j in (d.get('jobs') or {}): print(j)
" 2>/dev/null)
    [ -z "$JOBS" ] && continue
    WIRED=no
    while read -r J; do
      [ -z "$J" ] && continue
      grep -Fxq "$J" "$REQ" && { WIRED=yes; break; }
      grep -q "^$J / " "$REQ"  && { WIRED=yes; break; }
    done <<< "$JOBS"
    case "$TIER:$WIRED" in
      GATE:no)  echo -e "$R\tGATE_NOT_REQUIRED\t$NAME\t$PATHW"; TOTAL=$((TOTAL+1)) ;;
      CHECK:yes|ADVISORY:yes|PERIODIC:yes)
                echo -e "$R\t${TIER}_IS_REQUIRED\t$NAME\t$PATHW"; TOTAL=$((TOTAL+1)) ;;
    esac
  done < "$DEF"
  rm -f "$REQ" "$DEF"
done

echo "gate-tier invariant: $TOTAL discrepanc$([ "$TOTAL" = 1 ] && echo y || echo ies)" >&2
[ "$STRICT" = 1 ] && [ "$TOTAL" -gt 0 ] && exit 1
exit 0
