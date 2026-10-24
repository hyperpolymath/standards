#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# triage-2026-10-24-apply.sh — apply the "close now" list from
# docs/triage/2026-10-24-issue-classification.adoc.
#
# The classification puts every open issue in one of seven tiers. Only tier C
# ("close now") is safe to apply mechanically: those issues are answered by
# evidence already recorded in the document (probes, duplicates, parked
# campaigns, report-only notes, and defects verified fixed on main).
#
# Tiers Q/S/E/D/R/U are deliberately NOT touched: they are work, and closing
# work because it is small is how a backlog becomes a lie.
#
# Usage:
#   bash scripts/triage-2026-10-24-apply.sh            # dry-run (default)
#   bash scripts/triage-2026-10-24-apply.sh --apply    # comment + close tier C
#   bash scripts/triage-2026-10-24-apply.sh --report   # print tier counts only
#
# Idempotent: an issue that is already closed is skipped. Every close carries
# the reason from the CSV as its comment, so the closure is auditable from the
# issue itself.
#
# Who can run it: closing issues needs issues:write. The estate's bot token
# (arena-ai-coding-agent) has that today; a session token that does not will
# fail at the first close and stop, having changed nothing else.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIB="$SCRIPT_DIR/lib"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO="${REPO:-hyperpolymath/standards}"
CSV="$ROOT/docs/triage/2026-10-24-issue-classification.csv"
DOC="docs/triage/2026-10-24-issue-classification.adoc"

MODE="dry-run"
case "${1:-}" in
  ""|--dry-run) MODE="dry-run" ;;
  --apply)      MODE="apply" ;;
  --report)     MODE="report" ;;
  -h|--help)    sed -n '6,30p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
  *) printf 'unknown argument: %s (try --help)\n' "$1" >&2; exit 2 ;;
esac

[ -f "$CSV" ] || { printf 'missing classification CSV: %s\n' "$CSV" >&2; exit 2; }

# rows_for TIER — print "number<TAB>note" for every row in a tier, in numeric order.
#
# Parsed by csv-to-tsv.awk, not by splitting on ",": the CSV has quoted fields
# containing commas ("bug,security"), and -F, moves every later column one to
# the left. Column order is the document's: 1 number, 3 tier, 12 note.
rows_for() {
  awk -f "$LIB/csv-to-tsv.awk" "$CSV" \
    | awk -F'\t' -v tier="$1" 'NR > 1 && $3 == tier { print $1 "\t" $12 }' \
    | sort -t"$(printf '\t')" -k1,1n
}

if [ "$MODE" = "report" ]; then
  awk -f "$LIB/csv-to-tsv.awk" "$CSV" | awk -F'\t' '
    NR == 1 { next }
    { count[$3]++; total++ }
    END {
      split("C Q S E D R U", order, " ")
      label["C"] = "close now";  label["Q"] = "quick fix"
      label["S"] = "small task"; label["E"] = "estate campaign"
      label["D"] = "decision";   label["R"] = "research"
      label["U"] = "umbrella/parked"
      for (i = 1; i <= 7; i++) {
        t = order[i]
        printf "%s  %-17s %3d  %4.0f%%\n", t, label[t], count[t] + 0,
               100 * (count[t] + 0) / total
      }
      printf "   %-17s %3d\n", "total", total
    }'
  exit 0
fi

printf 'classification: %s\n' "$DOC"
printf 'mode:           %s\n' "$MODE"
printf 'repo:           %s\n\n' "$REPO"

printf '== tier C — close now ==\n'
closed=0
skipped=0
while IFS=$'\t' read -r number note; do
  [ -n "$number" ] || continue
  state="$(gh issue view "$number" --repo "$REPO" --json state --jq .state 2>/dev/null || echo UNKNOWN)"
  if [ "$state" = "CLOSED" ]; then
    printf '  #%s already closed — skip\n' "$number"
    skipped=$((skipped + 1))
    continue
  fi
  if [ "$state" = "UNKNOWN" ]; then
    printf '  #%s could not be read — stopping\n' "$number" >&2
    exit 1
  fi
  printf '  #%s  %s\n' "$number" "$note"
  if [ "$MODE" = "apply" ]; then
    gh issue comment "$number" --repo "$REPO" --body "Closing per the 2026-10-24 backlog classification (\`$DOC\`), tier C: $note

If this closure is wrong, reopen it — the classification is a snapshot, not a ruling."
    gh issue close "$number" --repo "$REPO" --reason "not planned"
  fi
  closed=$((closed + 1))
done < <(rows_for C)

printf '\n== tiers Q/S/E/D/R/U — not touched ==\n'
rows_for Q | while IFS=$'\t' read -r n _; do [ -n "$n" ] && printf '%s ' "#$n"; done; printf '\n   (quick fixes: pick from the ordered pass list in the document)\n'

printf '\ntotal: %d closable, %d already closed\n' "$closed" "$skipped"
if [ "$MODE" = "dry-run" ]; then
  printf '\nnothing was changed. Re-run with --apply to comment and close.\n'
fi
