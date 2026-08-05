#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Gate-tier invariant lint (standards docs/CICD-SIGNAL-DISCIPLINE.adoc).
#
# The four-tier taxonomy is only a naming convention until something enforces
# the correspondence between the tier a check CLAIMS and the way it is WIRED:
#
#   🔴 GATE:     MUST be a required status check
#   🟡 CHECK:    MUST NOT be required
#   ⚪ ADVISORY: MUST NOT be required
#   📅 PERIODIC: MUST NOT be required, and MUST NOT trigger on pull_request
#
# Report-only by default: the first run's output IS the gap list, so it is not
# useful as a gate until the gaps are closed. --strict exits non-zero.
#
# Usage: check-gate-tiers.sh [--strict] OWNER/REPO [OWNER/REPO ...]
set -uo pipefail
STRICT=0
[ "${1:-}" = "--strict" ] && { STRICT=1; shift; }
[ $# -eq 0 ] && { echo "usage: $0 [--strict] OWNER/REPO..." >&2; exit 2; }

for R in "$@"; do
  # ── what the repo REQUIRES (branch protection + every ruleset) ──────────
  REQ=$(mktemp)
  gh api "repos/$R/branches/$(gh api "repos/$R" -q .default_branch 2>/dev/null)/protection" \
     -q '.required_status_checks.checks[]?.context' 2>/dev/null >> "$REQ"
  for ID in $(gh api "repos/$R/rulesets" -q '.[].id' 2>/dev/null); do
    gh api "repos/$R/rulesets/$ID" \
      -q '.rules[]|select(.type=="required_status_checks")|.parameters.required_status_checks[].context' \
      2>/dev/null >> "$REQ"
  done
  sort -u -o "$REQ" "$REQ"

  # ── what the repo DECLARES (workflow names carry the tier) ──────────────
  gh api "repos/$R/actions/workflows?per_page=100" -q '.workflows[]|[.name,.path]|@tsv' 2>/dev/null \
  | while IFS=$'\t' read -r NAME PATHW; do
      case "$NAME" in
        "🔴"*) TIER=GATE ;;
        "🟡"*) TIER=CHECK ;;
        "⚪"*) TIER=ADVISORY ;;
        "📅"*) TIER=PERIODIC ;;
        *)     continue ;;
      esac
      # A workflow's CHECK contexts are its job names, not its own name, so an
      # exact match is only meaningful when the two coincide. Substring match
      # keeps this advisory rather than confidently wrong.
      if grep -qiF "$(echo "$NAME" | sed 's/^[^ ]* //; s/^[A-Za-z]*: //')" "$REQ"; then
        WIRED=required
      else
        WIRED=not-required
      fi
      case "$TIER:$WIRED" in
        GATE:not-required)     echo -e "$R\tGATE_NOT_REQUIRED\t$NAME\t$PATHW" ;;
        CHECK:required|ADVISORY:required|PERIODIC:required)
                               echo -e "$R\t${TIER}_IS_REQUIRED\t$NAME\t$PATHW" ;;
      esac
    done
  rm -f "$REQ"
done | tee /dev/stderr | grep -c . > /tmp/.gt_count 2>/dev/null || true

N=$(cat /tmp/.gt_count 2>/dev/null || echo 0)
echo "gate-tier invariant: $N discrepanc$([ "$N" = 1 ] && echo y || echo ies)" >&2
[ "$STRICT" = 1 ] && [ "$N" -gt 0 ] && exit 1
exit 0
