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
rows_for() {
  python3 - "$CSV" "$1" <<'PY'
import csv, sys
rows = [r for r in csv.DictReader(open(sys.argv[1])) if r["tier"] == sys.argv[2]]
for r in sorted(rows, key=lambda r: int(r["number"])):
    print(f'{r["number"]}\t{r["note"]}')
PY
}

if [ "$MODE" = "report" ]; then
  python3 - "$CSV" <<'PY'
import csv, sys, collections
tier_name = {"C":"close now","Q":"quick fix","S":"small task","E":"estate campaign",
             "D":"decision","R":"research","U":"umbrella/parked"}
c = collections.Counter(r["tier"] for r in csv.DictReader(open(sys.argv[1])))
total = sum(c.values())
for t in "CQSEDRU":
    print(f"{t}  {tier_name[t]:<17} {c[t]:>3}  {100*c[t]/total:>4.0f}%")
print(f"   {'total':<17} {total:>3}")
PY
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
