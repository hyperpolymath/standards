#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# apply-baseline.sh — filter Hypatia findings against a per-repo baseline.
#
# Inputs:
#   $1 = path to findings JSON (array of {severity, rule_module, type, file, ...})
#   $2 = path to .hypatia-baseline.json (array of baseline entries, see schema)
#   $3 = mode: "advisory" | "blocking"  (default: advisory)
#
# Outputs (stdout):
#   Filtered findings JSON. Each finding gets one of:
#     - removed entirely (matched by baseline, no severity_override)
#     - kept with downgraded `severity` (matched, severity_override set)
#     - kept unchanged with `baseline_status: "new"`
#   Plus a top-level summary written to $GITHUB_STEP_SUMMARY if set.
#
# Exit codes:
#   0 = all unfiltered findings have severity below blocking threshold
#       (or advisory mode)
#   1 = one or more unfiltered findings >= high in blocking mode
#   2 = invalid input (missing files, malformed JSON)
#
# Dependencies: bash, jq. Optional: ajv-cli for schema validation.

set -euo pipefail

FINDINGS_FILE="${1:-}"
BASELINE_FILE="${2:-}"
MODE="${3:-advisory}"
BLOCKING_THRESHOLD="${BLOCKING_THRESHOLD:-high}"
TODAY="$(date -u +%Y-%m-%d)"

if [[ -z "$FINDINGS_FILE" || -z "$BASELINE_FILE" ]]; then
  echo "usage: apply-baseline.sh <findings.json> <.hypatia-baseline.json> [advisory|blocking]" >&2
  exit 2
fi

if [[ ! -f "$FINDINGS_FILE" ]]; then
  echo "error: findings file not found: $FINDINGS_FILE" >&2
  exit 2
fi

# Missing baseline is not an error — treat as empty array.
if [[ ! -f "$BASELINE_FILE" ]]; then
  BASELINE_JSON='[]'
else
  BASELINE_JSON="$(cat "$BASELINE_FILE")"
fi

FINDINGS_JSON="$(cat "$FINDINGS_FILE")"

# Validate inputs are JSON arrays.
echo "$FINDINGS_JSON" | jq -e 'type == "array"' >/dev/null || {
  echo "error: findings JSON is not an array" >&2
  exit 2
}
echo "$BASELINE_JSON" | jq -e 'type == "array"' >/dev/null || {
  echo "error: baseline JSON is not an array" >&2
  exit 2
}

# Pre-filter baseline: drop expired entries (>=today).
ACTIVE_BASELINE="$(jq --arg today "$TODAY" '
  map(select((.expires_at // "9999-12-31") >= $today))
' <<<"$BASELINE_JSON")"

EXPIRED_COUNT="$(jq 'length' <<<"$BASELINE_JSON")"
ACTIVE_COUNT="$(jq 'length' <<<"$ACTIVE_BASELINE")"
EXPIRED_COUNT=$((EXPIRED_COUNT - ACTIVE_COUNT))

# Match each finding against the active baseline. Returns the matched
# baseline entry (or null) for each finding.
ANNOTATED="$(jq -n \
  --argjson findings "$FINDINGS_JSON" \
  --argjson baseline "$ACTIVE_BASELINE" '
  def match_entry(f):
    $baseline
    | map(select(
        .severity == f.severity
        and .rule_module == f.rule_module
        and .type == f.type
        and (
          (.file? // null) == f.file
          or (
            .file_pattern? != null
            and (f.file | test(
              .file_pattern
              | gsub("\\*\\*"; "DOUBLESTAR")
              | gsub("\\*"; "[^/]*")
              | gsub("DOUBLESTAR"; ".*")
              | "^" + . + "$"
            ))
          )
        )
      ))
    | first // null;

  $findings
  | map(
      . as $f
      | match_entry(.) as $m
      | if $m == null then
          . + {baseline_status: "new"}
        else
          . + {
            baseline_status: "acknowledged",
            baseline_note: ($m.note // null),
            baseline_tracking_issue: ($m.tracking_issue // null)
          }
          | if $m.severity_override then
              .severity = $m.severity_override
              | .baseline_status = "downgraded"
            else
              .
            end
        end
    )
')"

# Split into kept (will be evaluated by the gate) and suppressed (silently
# acknowledged, surfaced only in summary).
KEPT="$(jq '[.[] | select(.baseline_status != "acknowledged")]' <<<"$ANNOTATED")"
SUPPRESSED="$(jq '[.[] | select(.baseline_status == "acknowledged")]' <<<"$ANNOTATED")"

# Severity rank for blocking decision.
rank() {
  case "$1" in
    critical) echo 5 ;;
    high)     echo 4 ;;
    medium)   echo 3 ;;
    low)      echo 2 ;;
    info)     echo 1 ;;
    advisory) echo 0 ;;
    *)        echo 0 ;;
  esac
}

THRESHOLD_RANK="$(rank "$BLOCKING_THRESHOLD")"
MAX_KEPT_RANK=0
while IFS= read -r sev; do
  r="$(rank "$sev")"
  if (( r > MAX_KEPT_RANK )); then MAX_KEPT_RANK=$r; fi
done < <(jq -r '.[].severity' <<<"$KEPT")

KEPT_COUNT="$(jq 'length' <<<"$KEPT")"
SUPPRESSED_COUNT="$(jq 'length' <<<"$SUPPRESSED")"
TOTAL_COUNT="$(jq 'length' <<<"$FINDINGS_JSON")"

# Write summary if running inside Actions.
if [[ -n "${GITHUB_STEP_SUMMARY:-}" ]]; then
  {
    echo "## Hypatia baseline filter"
    echo
    echo "| | Count |"
    echo "|---|---|"
    echo "| Total findings | $TOTAL_COUNT |"
    echo "| Acknowledged by baseline | $SUPPRESSED_COUNT |"
    echo "| Remaining (kept for gate) | $KEPT_COUNT |"
    echo "| Expired baseline entries | $EXPIRED_COUNT |"
    echo
    echo "Mode: \`$MODE\` · Blocking threshold: \`$BLOCKING_THRESHOLD\`"
    if (( EXPIRED_COUNT > 0 )); then
      echo
      echo ":warning: $EXPIRED_COUNT baseline entries are past their \`expires_at\` and were ignored."
    fi
  } >> "$GITHUB_STEP_SUMMARY"
fi

# Always emit the annotated findings so downstream steps can use them.
jq -n --argjson kept "$KEPT" --argjson suppressed "$SUPPRESSED" '{
  findings_kept: $kept,
  findings_suppressed: $suppressed
}'

# Gate decision.
if [[ "$MODE" == "advisory" ]]; then
  exit 0
fi

if (( MAX_KEPT_RANK >= THRESHOLD_RANK )); then
  echo "::error::Gate failed: $KEPT_COUNT unfiltered finding(s) at or above '$BLOCKING_THRESHOLD'." >&2
  exit 1
fi

exit 0
