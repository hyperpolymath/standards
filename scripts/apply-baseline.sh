#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
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
#   2 = invalid input (missing files, malformed JSON, or a baseline that
#       violates the schema — see below)
#
# Dependencies: bash, jq.
#
# Schema enforcement: the baseline is structurally validated here, in jq,
# mirroring .machine_readable/hypatia-baseline.schema.json (required keys,
# file XOR file_pattern, closed key set, severity enums, rule_module/type/
# tracking_issue/expires_at shapes). This script is the single place every
# baseline passes through (both reusable workflows call it), so validating
# here needs no extra tooling or checkout — and a malformed baseline FAILS
# (exit 2) rather than silently matching nothing, which is how suppression
# bugs creep in. Keep this mirror in sync when the schema changes.

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

# Structural validation against the baseline schema (see header).
SCHEMA_ERRORS="$(jq -r '
  def known: ["severity","rule_module","type","file","file_pattern",
              "severity_override","expires_at","note","tracking_issue"];
  def sevs: ["critical","high","medium","low","info"];
  [ to_entries[] | .key as $i | .value as $e |
    if ($e|type) != "object" then "entry[\($i)]: not an object"
    else (
      (["severity","rule_module","type"][]
        | select(($e[.]|type) != "string")
        | "entry[\($i)]: required key \(.) missing or not a string"),
      (if (($e|has("file")) == ($e|has("file_pattern")))
       then "entry[\($i)]: exactly one of file / file_pattern is required"
       else empty end),
      ($e | keys[]
        | select(. as $k | known | index($k) | not)
        | "entry[\($i)]: unknown key \(.)"),
      (if ($e.severity|type) == "string" and ((sevs|index($e.severity))|not)
       then "entry[\($i)]: invalid severity \($e.severity)" else empty end),
      (if ($e.rule_module|type) == "string"
          and (($e.rule_module|test("^[a-z][a-z0-9_]*$"))|not)
       then "entry[\($i)]: rule_module fails pattern: \($e.rule_module)"
       else empty end),
      (if ($e.type|type) == "string"
          and (($e.type|test("^([a-z][a-z0-9_]*|[A-Z]{2,3}[0-9]{3})$"))|not)
       then "entry[\($i)]: type fails pattern: \($e.type)" else empty end),
      (if ($e|has("file")) and ((($e.file|type) != "string") or ($e.file == ""))
       then "entry[\($i)]: file must be a non-empty string" else empty end),
      (if ($e|has("file_pattern"))
          and ((($e.file_pattern|type) != "string") or ($e.file_pattern == ""))
       then "entry[\($i)]: file_pattern must be a non-empty string"
       else empty end),
      (if ($e|has("severity_override"))
          and (((sevs + ["advisory"])|index($e.severity_override))|not)
       then "entry[\($i)]: invalid severity_override \($e.severity_override)"
       else empty end),
      (if ($e|has("expires_at"))
          and ((($e.expires_at|type) != "string")
               or (($e.expires_at|test("^[0-9]{4}-[0-9]{2}-[0-9]{2}$"))|not))
       then "entry[\($i)]: expires_at must be an ISO date (YYYY-MM-DD)"
       else empty end),
      (if ($e|has("tracking_issue"))
          and ((($e.tracking_issue|type) != "string")
               or (($e.tracking_issue|test("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+#[0-9]+$"))|not))
       then "entry[\($i)]: tracking_issue must look like owner/repo#N"
       else empty end)
    ) end
  ] | .[]
' <<<"$BASELINE_JSON")"

if [[ -n "$SCHEMA_ERRORS" ]]; then
  echo "error: baseline violates hypatia-baseline.schema.json:" >&2
  echo "$SCHEMA_ERRORS" >&2
  exit 2
fi

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
  # Two captures are essential here:
  #   `f as $finding` — without this, references like `f.file` inside the
  #   select() get re-evaluated against the current baseline entry (the
  #   re-bound `.`), not the finding. Binding $finding once captures the
  #   finding before we enter the map(select()) over the baseline.
  #
  #   `(.file_pattern? // null) as $pat` — inside `test(arg)` the dot
  #   rebinds to the input of test ($finding.file, a string), so
  #   referencing `.file_pattern` there would error with "Cannot index
  #   string". Capture the entry pattern first, then reference $pat
  #   inside the test() regex argument.
  def match_entry(f):
    f as $finding
    | $baseline
    | map(select(
        .severity == $finding.severity
        and .rule_module == $finding.rule_module
        and .type == $finding.type
        and (
          (.file? // null) == $finding.file
          or (
            (.file_pattern? // null) as $pat
            | $pat != null
            and ($finding.file | test(
              $pat
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
