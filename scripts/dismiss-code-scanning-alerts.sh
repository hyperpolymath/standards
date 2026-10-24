#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# dismiss-code-scanning-alerts.sh — close the code-scanning alerts that
# `.github/code-scanning-dismissals.json` records as accepted risk.
#
# WHY A SCRIPT AND A COMMITTED LIST. Dismissing an alert in the UI leaves no
# trace in the repository: nobody reviewing the tree can tell a false positive
# from a hidden one, and a dismissed alert silently absorbs the NEXT finding
# with the same rule id and path. So the decision is written down first, in
# git, one entry per alert with its line, its reason and its argument — and
# this script refuses to touch anything that is not on that list. It is the
# code-scanning counterpart of `.hypatia-baseline.json`.
#
# ⚠ It only ever DISMISSES. It never reopens, never touches alerts in the
# `fixed` state, and never guesses: an alert that does not match an entry by
# rule id AND path AND (when the entry names one) line is reported and left
# alone.
#
# USAGE
#   scripts/dismiss-code-scanning-alerts.sh                 # plan (default, no writes)
#   scripts/dismiss-code-scanning-alerts.sh --apply         # dismiss the matches
#   scripts/dismiss-code-scanning-alerts.sh --apply --repo owner/name
#
# OFFLINE / TESTING
#   ALERTS_JSON=<file>  read the alert feed from a file (an array, as returned
#                       by `gh api repos/:owner/:repo/code-scanning/alerts`)
#                       instead of calling the API; makes the matcher testable
#                       and makes --apply a no-op dry run unless GH_TOKEN is set.
#
# PERMISSIONS
#   Reading alerts needs `security-events: read`; dismissing needs
#   `security-events: write`. A fine-grained token without them gets HTTP 403
#   on the first call, which this script reports as such rather than as
#   "nothing to do" — the two must never be confused.
#
# EXIT CODES
#   0  plan printed / every match dismissed / nothing to do
#   1  at least one dismissal failed
#   2  usage error or an invalid allowlist (fails closed, before any write)
#   3  could not read the alert feed (credentials or network)
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ALLOWLIST="${ALLOWLIST:-$ROOT/.github/code-scanning-dismissals.json}"
REPO="${REPO:-$(git -C "$ROOT" remote get-url origin 2>/dev/null | sed -e 's#^git@github.com:##' -e 's#^https://github.com/##' -e 's#\.git$##')}"
APPLY=0

die() { printf 'dismiss-code-scanning-alerts: %s\n' "$*" >&2; exit 2; }

while [ $# -gt 0 ]; do
  case "$1" in
    --apply)   APPLY=1 ;;
    --repo)    shift; [ $# -gt 0 ] || die "--repo needs owner/name"; REPO="$1" ;;
    --allowlist) shift; [ $# -gt 0 ] || die "--allowlist needs a path"; ALLOWLIST="$1" ;;
    -h|--help) sed -n '2,40p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *)         die "unknown argument: $1" ;;
  esac
  shift
done

[ -f "$ALLOWLIST" ] || die "allowlist not found: $ALLOWLIST"
[ -n "$REPO" ] || die "cannot determine the repository; pass --repo owner/name"
command -v jq >/dev/null 2>&1 || die "jq is required"

# ── 1. Validate the allowlist before doing anything else ────────────────────
# Fails closed: a malformed entry must never be read as "match everything".
ERRORS="$(jq -r '
  def reasons: ["false positive", "won'"'"'t fix", "used in tests"];
  [ .dismissals[]? | select(.rule_id == null or .rule_id == "" or (.rule_id|type) != "string")
      | "entry without a rule_id" ],
  [ .dismissals[]? | select((.reason // "") as $r | (reasons | index($r)) | not)
      | "entry for \(.rule_id): reason must be one of \(reasons|join(", "))" ],
  [ .dismissals[]? | select((.comment // "") | length < 40)
      | "entry for \(.rule_id): comment must argue the case (>=40 chars)" ],
  [ .dismissals[]? | select((.line != null) and ((.line|type) != "number"))
      | "entry for \(.rule_id): line must be a number or null" ],
  [ .dismissals[]? | select((.path != null) and ((.path|type) != "string"))
      | "entry for \(.rule_id): path must be a string or null" ]
  | .[]
' "$ALLOWLIST" 2>/dev/null)" || die "allowlist is not valid JSON: $ALLOWLIST"
if [ -n "$ERRORS" ]; then
  printf '%s\n' "$ERRORS" >&2
  die "allowlist invalid; nothing was changed"
fi
N_ENTRIES="$(jq '.dismissals | length' "$ALLOWLIST")"
[ "$N_ENTRIES" -gt 0 ] || die "allowlist has no dismissals; nothing to do"

# ── 2. Read the alert feed ──────────────────────────────────────────────────
if [ -n "${ALERTS_JSON:-}" ]; then
  [ -f "$ALERTS_JSON" ] || die "ALERTS_JSON does not exist: $ALERTS_JSON"
  FEED="$(cat "$ALERTS_JSON")"
else
  command -v gh >/dev/null 2>&1 || die "gh is required (or set ALERTS_JSON)"
  if ! FEED="$(gh api --paginate "repos/$REPO/code-scanning/alerts?state=open&per_page=100" 2>/tmp/.dcsa-err.$$)"; then
    printf 'dismiss-code-scanning-alerts: could not read alerts for %s\n' "$REPO" >&2
    sed 's/^/  /' /tmp/.dcsa-err.$$ >&2
    rm -f /tmp/.dcsa-err.$$
    exit 3
  fi
  rm -f /tmp/.dcsa-err.$$
fi
printf '%s' "$FEED" | jq -e 'type == "array"' >/dev/null 2>&1 || die "alert feed is not a JSON array"

# ── 3. Match, then (only with --apply) dismiss ──────────────────────────────
# The match is deliberately narrow: rule id AND path AND line, with path/line
# nullable only because some scorecard findings carry no location at all.
PLAN="$(jq -n --argjson alerts "$FEED" --argjson allow "$(jq '.dismissals' "$ALLOWLIST")" '
  # Path and line are read off the ALERT ($a), never off the allowlist entry:
  # the entry states the claim, the alert states where it actually is.
  def path_of($a): (($a.most_recent_instance.location.path // $a.location.path // "")
                    | sub("^\\./"; ""));
  def line_of($a): ($a.most_recent_instance.location.start_line
                    // $a.location.start_line // null);
  [ $alerts[] | select(.state == "open") ] as $open
  | [ $open[] as $a
      | $allow[] as $e
      | select($a.rule.id == $e.rule_id)
      # An entry that names a path matches only that path; an entry with NO
      # path (an aggregate finding such as SASTID, which scorecard reports
      # against no file at all) matches only an alert that ALSO has no path.
      # Without that second half, one aggregate ack would silently absorb
      # every future finding carrying the same rule id — including a real one
      # in a file the entry never mentions.
      | select(if $e.path == null then (path_of($a) == "") else path_of($a) == $e.path end)
      | select(if $e.line == null then true else line_of($a) == $e.line end)
      | { number: $a.number,
          rule_id: $a.rule.id,
          path: (path_of($a) | if . == "" then "(no location)" else . end),
          line: line_of($a),
          reason: $e.reason,
          comment: $e.comment }
    ]
  | unique_by(.number)
')"

N_PLAN="$(printf '%s' "$PLAN" | jq 'length')"
N_OPEN="$(printf '%s' "$FEED" | jq '[ .[] | select(.state == "open") ] | length')"
printf 'repo: %s\nalerts open: %s\nallowlist entries: %s\nmatches: %s\n\n' \
  "$REPO" "$N_OPEN" "$N_ENTRIES" "$N_PLAN"

if [ "$N_PLAN" -eq 0 ]; then
  echo "Nothing in the allowlist matches an open alert. Either the alerts are"
  echo "already dismissed, or they moved (a new line or path). Check before"
  echo "assuming they are gone: scripts/dismiss-code-scanning-alerts.sh --help"
  exit 0
fi

printf '%s' "$PLAN" | jq -r '.[] | "  #\(.number)  \(.rule_id)  \(.path):\(.line // "-")  [\(.reason)]"'

if [ "$APPLY" -ne 1 ]; then
  printf '\nPlan only. Re-run with --apply to dismiss these %s alert(s).\n' "$N_PLAN"
  exit 0
fi

FAILED=0
while IFS=$'\t' read -r number reason comment; do
  if gh api -X PATCH "repos/$REPO/code-scanning/alerts/$number" \
       -f state=dismissed \
       -f "dismissed_reason=$reason" \
       -f "dismissed_comment=$comment" >/dev/null 2>&1; then
    printf '  dismissed #%s (%s)\n' "$number" "$reason"
  else
    printf '  FAILED to dismiss #%s — check the token has security-events: write\n' "$number" >&2
    FAILED=$((FAILED + 1))
  fi
done < <(printf '%s' "$PLAN" | jq -r '.[] | [.number, .reason, .comment] | @tsv')

if [ "$FAILED" -ne 0 ]; then
  printf '\n%s dismissal(s) failed.\n' "$FAILED" >&2
  exit 1
fi
printf '\nDismissed %s alert(s).\n' "$N_PLAN"
