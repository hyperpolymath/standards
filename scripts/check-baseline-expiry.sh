#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# check-baseline-expiry.sh — the baseline is a queue, not a graveyard.
#
# WHY THIS EXISTS
# ---------------
# `.hypatia-baseline.json` already had the right idea: an entry may carry
# `expires_at`, and `apply-baseline.sh` drops it from matching once that date
# passes, so an expired acknowledgement starts grading the change again. The
# mechanism works. It was simply optional, and that is the whole failure:
#
#     measured 2026-10-24 in this repository — 63 entries, 40 dated,
#     23 with no date at all, of which 7 said "Baseline permanently"
#     in the note.
#
# An undated acknowledgement is a decision nobody will ever revisit. In the
# file it is indistinguishable from a decision that was right when it was made
# and false now — and the estate has already paid for that difference: a
# baseline that absorbed 255 findings in one commit, an allowlist that grew to
# cover a real secret, 13,206 banned-language files declared once and forgotten
# (check-exemption-ratchet.sh makes the same diagnosis from the other side).
#
# THE RULE
#   undated  = a decision with no review date          => fails
#   expired  = a decision whose review date has passed  => fails
#   expiring = within --warn-days of its date          => warns, does not fail
#
# The warning window is not decoration. A cliff where 23 entries lapse on one
# day and turn every pull request red is exactly how a ledger gets bulk-renewed
# unread — the ratchet clicking back one notch without anyone deciding
# anything. Read them while they are still quiet.
#
# Usage: check-baseline-expiry.sh [--baseline PATH] [--warn-days N]
#                                 [--today YYYY-MM-DD] [--allow-undated] [--quiet]
#
#   --allow-undated  report undated entries but exit 0. For repositories
#                    mid-migration: this script is shared, and a check that
#                    cannot be adopted incrementally is one that gets routed
#                    around (the same reasoning behind the ratchet's
#                    grandfathering of historic anonymous entries).
#   --today          pins "now" so the tests are not calendar-dependent.
#   --quiet          one-line-per-bucket output, GitHub annotations on failure.
#
# EXIT CODES
#   0  every entry is dated and none has expired
#   1  an entry is undated (unless --allow-undated) or has expired
#   2  invalid invocation or unreadable baseline
set -uo pipefail

BASELINE=".hypatia-baseline.json"
WARN_DAYS=45
TODAY="$(date -u +%Y-%m-%d)"
ALLOW_UNDATED=0
QUIET=0

while [ $# -gt 0 ]; do
  case "$1" in
    --baseline)      shift; BASELINE="${1:-}" ;;
    --warn-days)     shift; WARN_DAYS="${1:-45}" ;;
    --today)         shift; TODAY="${1:-}" ;;
    --allow-undated) ALLOW_UNDATED=1 ;;
    --quiet)         QUIET=1 ;;
    -h|--help)       sed -n '2,49p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "error: unknown argument: $1" >&2; exit 2 ;;
  esac
  shift
done

if ! printf '%s' "$TODAY" | grep -qE '^[0-9]{4}-[0-9]{2}-[0-9]{2}$'; then
  echo "error: --today must be YYYY-MM-DD (got '$TODAY')" >&2
  exit 2
fi
if ! printf '%s' "$WARN_DAYS" | grep -qE '^[0-9]+$'; then
  echo "error: --warn-days must be a non-negative integer (got '$WARN_DAYS')" >&2
  exit 2
fi
if [ ! -f "$BASELINE" ]; then
  # No baseline is not a failure. A repository that suppresses nothing has
  # nothing to review, and failing here would make "delete the baseline" the
  # cheapest way to pass a check whose only purpose is to keep it honest.
  [ "$QUIET" = 1 ] || echo "no $BASELINE — nothing to review"
  exit 0
fi
if ! jq -e 'type == "array"' "$BASELINE" >/dev/null 2>&1; then
  echo "error: $BASELINE is not a JSON array" >&2
  exit 2
fi

# One pass computes every bucket and every list, so the counts printed above
# the lists cannot disagree with the entries printed below them.
REPORT="$(jq -c --arg today "$TODAY" --argjson warn "$WARN_DAYS" '
  # An ISO date means midnight UTC of that day; accept a full timestamp too.
  def epoch:
    (if test("T") then . else . + "T00:00:00Z" end) | fromdateiso8601;

  def describe:
    "\(.rule_module)/\(.type) @ \(.file // .file_pattern // "?")";

  def dated:
    (has("expires_at") and ((.expires_at | type) == "string"));

  (($today | epoch) / 86400 | floor) as $today_days

  | def dleft:
      ((.expires_at | epoch) / 86400 | floor) - $today_days;

  [ .[] | select(dated) | { e: ., d: dleft } ] as $with
  | {
      total:    length,
      undated:  [ .[] | select(dated | not) | describe ],
      expired:  [ $with[] | select(.d < 0)     | "\(.e.expires_at)  \(.e | describe)" ],
      expiring: [ $with[] | select(.d >= 0 and .d <= $warn) | "\(.e.expires_at)  \(.e | describe)" ]
    }
  | . + { undated_n: (.undated | length),
          expired_n: (.expired | length),
          expiring_n: (.expiring | length) }
' "$BASELINE")" || { echo "error: could not read $BASELINE" >&2; exit 2; }

n_undated="$(printf '%s' "$REPORT" | jq '.undated_n')"
n_expired="$(printf '%s' "$REPORT" | jq '.expired_n')"
n_expiring="$(printf '%s' "$REPORT" | jq '.expiring_n')"
n_total="$(printf '%s' "$REPORT" | jq '.total')"

fail=0
[ "$n_expired" -gt 0 ] && fail=1
if [ "$n_undated" -gt 0 ] && [ "$ALLOW_UNDATED" != 1 ]; then fail=1; fi

if [ "$QUIET" = 1 ]; then
  echo "baseline expiry ($BASELINE, as of $TODAY): total=$n_total undated=$n_undated expired=$n_expired expiring_${WARN_DAYS}d=$n_expiring"
else
  echo "baseline expiry: $n_total entr(y|ies) in $BASELINE — as of $TODAY"
  echo "  undated: $n_undated    expired: $n_expired    expiring within ${WARN_DAYS}d: $n_expiring"
fi

if [ "$n_undated" -gt 0 ]; then
  if [ "$QUIET" = 1 ]; then
    echo "::warning::$n_undated baseline entr(y|ies) carry no expires_at — an undated acknowledgement is a decision nobody revisits"
  else
    echo
    echo "UNDATED — add an expires_at. The gate only tightens on a date, so an"
    echo "entry without one suppresses its finding for as long as the file exists:"
    printf '%s' "$REPORT" | jq -r '.undated[] | "  \(.)"'
    [ "$ALLOW_UNDATED" = 1 ] || \
      echo '  -> add "expires_at": "YYYY-MM-DD" — any date you will still be here to defend'
  fi
fi

if [ "$n_expired" -gt 0 ]; then
  if [ "$QUIET" = 1 ]; then
    echo "::error::$n_expired baseline entr(y|ies) are past expires_at and no longer suppress anything"
  else
    echo
    echo "EXPIRED — these no longer suppress anything (apply-baseline.sh drops"
    echo "past-dated entries before matching), so the findings are grading again."
    echo "Fix the finding, or re-ack it with a new date and say what you checked:"
    printf '%s' "$REPORT" | jq -r '.expired[] | "  \(.)"'
  fi
fi

if [ "$n_expiring" -gt 0 ]; then
  if [ "$QUIET" = 1 ]; then
    echo "::notice::$n_expiring baseline entr(y|ies) expire within $WARN_DAYS days"
  else
    echo
    echo "EXPIRING — still suppressing today; start grading on the date shown."
    echo "Read them now, while the finding is still quiet:"
    printf '%s' "$REPORT" | jq -r '.expiring[] | "  \(.)"'
  fi
  if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
    {
      echo "### Baseline entries expiring within ${WARN_DAYS} days"
      echo
      printf '%s' "$REPORT" | jq -r '.expiring[] | "- `\(.)`"'
    } >> "$GITHUB_STEP_SUMMARY"
  fi
fi

if [ "$fail" != 0 ]; then
  [ "$QUIET" = 1 ] || echo
  exit 1
fi
[ "$QUIET" = 1 ] || { echo; echo "OK — every acknowledgement is dated and current."; }
exit 0
