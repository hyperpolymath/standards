#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# verdict-grid.sh — render the compact verdict grid for a findings lane.
# Policy and anatomy: docs/VERDICT-GRID-SPEC.adoc. Logic: scripts/lib/verdict-grid.jq.
#
# Usage:
#   verdict-grid.sh render --out DIR [sources] [identity]
#     sources
#       --hypatia FILE        Hypatia findings array (paths relativised)
#       --panic FILE          panic-attack status envelope
#       --pons FILE           pons asinorum status envelope
#       --report-backs DIR    gitbot-fleet report-back envelopes, one *.json per member
#       --enrolment FILE      gitbot-fleet enrolment manifest for this repository
#       --previous FILE       body of the previous grid comment (delta column)
#     identity
#       --lane NAME           grid lane (default: hypatia)
#       --sha SHA --run ID --url URL --pr N --repo OWNER/NAME
#       --hypatia-ref SHA --panic-ref SHA --pons-ref SHA
#   verdict-grid.sh baseline FILE
#     Print the baseline embedded in a previous comment body, or null.
#
# Outputs in DIR: report.json (structured), report.md (the grid), badge.json
# (shields.io endpoint schema). Exit 0 when a report was rendered, whatever its
# verdict; exit 2 on a usage error. Gating is the caller's decision, made from
# report.json, never from this exit code.
#
# Dependencies: bash, jq, sha256sum, base64.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
JQ_LIB="$SCRIPT_DIR/lib/verdict-grid.jq"

usage() { sed -n '5,30p' "$0" >&2; exit 2; }

# One source record per evidence file: present? valid JSON? parsed document.
source_record() {
  local kind="$1" name="$2" file="${3:-}"
  if [ -z "$file" ] || [ ! -e "$file" ]; then
    jq -n --arg k "$kind" --arg n "$name" '{kind: $k, name: $n, present: false, valid: false, doc: null}'
  elif [ -s "$file" ] && jq -e . "$file" >/dev/null 2>&1; then
    jq -c --arg k "$kind" --arg n "$name" '{kind: $k, name: $n, present: true, valid: true, doc: .}' "$file"
  else
    jq -n --arg k "$kind" --arg n "$name" '{kind: $k, name: $n, present: true, valid: false, doc: null}'
  fi
}

baseline_of() {
  local file="$1" b64
  [ -s "$file" ] || { echo null; return; }
  b64=$( { grep -o '<!-- verdict-grid-baseline:[A-Za-z0-9+/=]* -->' "$file" || true; } | tail -n1 \
        | sed 's/^<!-- verdict-grid-baseline://; s/ -->$//')
  if [ -z "$b64" ] || [ "${#b64}" -gt 32000 ]; then echo null; return; fi
  printf '%s' "$b64" | base64 -d 2>/dev/null | jq -c '
    if type == "object" and (.reporters | type) == "array"
       and all(.reporters[]; (.id | type) == "string" and (.counts | type) == "object"
         and all(.counts[]; type == "number" and . >= 0))
    then . else null end' 2>/dev/null || echo null
}

cmd="${1:-}"; shift || true
case "$cmd" in
  baseline)
    [ -n "${1:-}" ] || usage
    baseline_of "$1"
    ;;
  render)
    out="" hyp="" pan="" pon="" backs="" enrol="" prev="" lane="hypatia"
    sha="" run="local" url="" pr="local" repo="" href="" pref="" qref=""
    while [ $# -gt 0 ]; do
      case "$1" in
        --out) out="$2"; shift 2 ;;
        --hypatia) hyp="$2"; shift 2 ;;
        --panic) pan="$2"; shift 2 ;;
        --pons) pon="$2"; shift 2 ;;
        --report-backs) backs="$2"; shift 2 ;;
        --enrolment) enrol="$2"; shift 2 ;;
        --previous) prev="$2"; shift 2 ;;
        --lane) lane="$2"; shift 2 ;;
        --sha) sha="$2"; shift 2 ;;
        --run) run="$2"; shift 2 ;;
        --url) url="$2"; shift 2 ;;
        --pr) pr="$2"; shift 2 ;;
        --repo) repo="$2"; shift 2 ;;
        --hypatia-ref) href="$2"; shift 2 ;;
        --panic-ref) pref="$2"; shift 2 ;;
        --pons-ref) qref="$2"; shift 2 ;;
        *) echo "unknown option: $1" >&2; usage ;;
      esac
    done
    [ -n "$out" ] || usage
    [ -f "$JQ_LIB" ] || { echo "missing $JQ_LIB" >&2; exit 2; }
    mkdir -p "$out"

    sources=$(mktemp); trap 'rm -f "$sources"' EXIT
    {
      source_record hypatia hypatia "$hyp"
      source_record panic panic-attack "$pan"
      source_record pons pons-asinorum "$pon"
      if [ -n "$backs" ] && [ -d "$backs" ]; then
        for f in "$backs"/*.json; do
          [ -e "$f" ] || continue
          source_record report-back "$(basename "$f" .json)" "$f"
        done
      fi
    } | jq -s . > "$sources"

    enrolment=null
    if [ -n "$enrol" ]; then
      if [ -s "$enrol" ] && jq -e 'type == "object" and (.reporters | type) == "object"' "$enrol" >/dev/null 2>&1; then
        enrolment=$(jq -c . "$enrol")
      else
        # A malformed enrolment manifest is itself evidence that cannot be read.
        enrolment='{"reporters":{"gitbot-fleet enrolment":{"title":"gitbot-fleet enrolment","enrolled":true}}}'
        echo "warning: enrolment manifest unreadable; recorded as missing evidence" >&2
      fi
    fi

    previous=null
    [ -n "$prev" ] && previous=$(baseline_of "$prev")

    # The profile names what produced the grid: renderer, enrolment and tool
    # commits. Two revisions compare only under the same profile.
    profile=$( { cat "$0" "$JQ_LIB"; [ -n "$enrol" ] && [ -f "$enrol" ] && cat "$enrol"; printf '%s\n' "$href" "$pref" "$qref" "$lane"; } | sha256sum | cut -d' ' -f1)

    dirty=false
    if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
      [ -z "$sha" ] && sha=$(git rev-parse HEAD)
      git diff --quiet HEAD 2>/dev/null || dirty=true
    fi

    jq -n --slurpfile sources "$sources" --argjson enrolment "$enrolment" \
      --arg profile "$profile" --arg lane "$lane" --arg sha "$sha" --arg run "$run" --arg url "$url" \
      --arg pr "$pr" --arg repo "$repo" --argjson dirty "$dirty" --arg generated "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
      --arg href "$href" --arg pref "$pref" --arg qref "$qref" \
      '{sources: $sources[0], enrolment: $enrolment, profile: $profile,
        identity: {lane: $lane, sha: $sha, run: $run, url: $url, pr: $pr, repo: $repo, dirty: $dirty,
                   generated: $generated, refs: {hypatia: $href, panic: $pref, pons: $qref}}}' \
      | jq --arg mode build --argjson previous null -f "$JQ_LIB" > "$out/report.json"

    jq -r --arg mode render --argjson previous "$previous" -f "$JQ_LIB" "$out/report.json" > "$out/report.md"
    jq --arg mode badge --argjson previous null -f "$JQ_LIB" "$out/report.json" > "$out/badge.json"

    if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then cat "$out/report.md" >> "$GITHUB_STEP_SUMMARY"; fi
    if [ -n "${GITHUB_OUTPUT:-}" ]; then echo "state=$(jq -r .state "$out/report.json")" >> "$GITHUB_OUTPUT"; fi
    cat "$out/report.md"
    ;;
  *) usage ;;
esac
