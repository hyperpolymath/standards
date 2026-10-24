#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# fork-governance-render.sh — compose the one PR comment that carries every
# governance lane, in a fixed order, with a summary table on top.
#
# WHY ONE COMMENT AND NOT THREE. Three lanes posting three comments is three
# independent update loops writing to the same pull request: the order in the
# conversation depends on which job finished last, a lane that could not render
# leaves the OLD grid standing with no indication that it is stale, and a
# reader has to reconstruct the verdict from fragments. This renderer takes the
# lanes' already-rendered reports as inputs and emits a single body, in a fixed
# order:
#
#   1. the summary table   (DEED syntax, k9-deed conformance, Hypatia)
#   2. the k9-deed conformance grid
#   3. the Hypatia verdict grid
#
# The lane reports are NOT re-rendered here. `k9-grid.sh` and
# `verdict-grid.sh` own their anatomy (docs/VERDICT-GRID-SPEC.adoc); this
# script embeds them verbatim, marker and all, so each lane stays independently
# machine-readable inside the combined comment and its own baseline parser
# keeps working.
#
# A lane with no report is NOT omitted. A missing lane renders as orange with
# the reason, because a summary that silently loses a row reads as "nothing
# wrong there" — the failure mode the whole system exists to prevent.
#
# USAGE
#   scripts/fork-governance-render.sh --out DIR \
#     [--deed-result success|failure|skipped|cancelled] \
#     [--k9-result R] [--k9-verdict green|yellow|orange|red|grey] \
#     [--k9-report PATH] \
#     [--hypatia-result R] [--hypatia-verdict green|yellow|orange|red|grey|purple] \
#     [--hypatia-report PATH] \
#     [--sha SHA] [--run RUN_ID] [--url URL] [--pr N] [--repo owner/name]
#
# OUTPUT
#   DIR/body.md     the comment body (also written to stdout)
#   DIR/badge.json  shields.io endpoint JSON for the overall state
#
# EXIT
#   0  body written
#   2  usage error
#
# The scripts is deliberately silent about *whether the lanes passed*. It
# reports; the lane jobs gate. A renderer that also gated would be a second
# place to encode the policy, and the two would drift.

set -uo pipefail

OUT=""
DEED_RESULT=""
K9_RESULT=""
K9_VERDICT=""
K9_REPORT=""
HYPATIA_RESULT=""
HYPATIA_VERDICT=""
HYPATIA_REPORT=""
SHA=""
RUN_ID=""
RUN_URL=""
PR=""
REPO=""

die() { printf 'fork-governance-render: %s\n' "$*" >&2; exit 2; }

while [ $# -gt 0 ]; do
  case "$1" in
    --out)            shift; OUT="${1:-}" ;;
    --deed-result)    shift; DEED_RESULT="${1:-}" ;;
    --k9-result)      shift; K9_RESULT="${1:-}" ;;
    --k9-verdict)     shift; K9_VERDICT="${1:-}" ;;
    --k9-report)      shift; K9_REPORT="${1:-}" ;;
    --hypatia-result) shift; HYPATIA_RESULT="${1:-}" ;;
    --hypatia-verdict) shift; HYPATIA_VERDICT="${1:-}" ;;
    --hypatia-report) shift; HYPATIA_REPORT="${1:-}" ;;
    --sha)            shift; SHA="${1:-}" ;;
    --run)            shift; RUN_ID="${1:-}" ;;
    --url)            shift; RUN_URL="${1:-}" ;;
    --pr)             shift; PR="${1:-}" ;;
    --repo)           shift; REPO="${1:-}" ;;
    -h|--help)        sed -n '2,45p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *)                die "unknown argument: $1" ;;
  esac
  shift
done

[ -n "$OUT" ] || die "--out is required"
mkdir -p "$OUT" || die "cannot create $OUT"
[ -n "$RUN_URL" ] || RUN_URL="${GITHUB_SERVER_URL:-https://github.com}/${REPO:-${GITHUB_REPOSITORY:-}}/actions/runs/${RUN_ID:-${GITHUB_RUN_ID:-0}}"
[ -n "$SHA" ] || SHA="${GITHUB_SHA:-}"
[ -n "$RUN_ID" ] || RUN_ID="${GITHUB_RUN_ID:-0}"

# ── Status vocabulary ───────────────────────────────────────────────────────
# Two grading families (docs/VERDICT-GRID-SPEC.adoc §Two grading families):
# conformance lanes grade red/orange/yellow/green/grey, findings lanes add
# purple for "ran, worst finding is critical". This renderer never invents a
# state: it maps the lane's own word to its glyph, and treats any word it does
# not recognise as indeterminate rather than as a pass.
glyph_for() {
  case "$1" in
    green)  printf '🟢' ;;
    yellow) printf '🟡' ;;
    orange) printf '🟠' ;;
    red)    printf '🔴' ;;
    purple) printf '🟣' ;;
    blue)   printf '🔵' ;;
    grey)   printf '⚪' ;;
    *)      printf '🟠' ;;
  esac
}
word_ok() {
  case "$1" in green|yellow|orange|red|purple|blue|grey) return 0 ;; *) return 1 ;; esac
}
rank_of() {
  case "$1" in red) printf 4 ;; purple) printf 4 ;; blue) printf 3 ;; orange) printf 3 ;;
                 yellow) printf 2 ;; green) printf 1 ;; *) printf 3 ;; esac
}

# A job result says whether the LANE COMPLETED. A grid state says WHAT IT
# FOUND. They are different claims and both are reported: a job that failed
# while its grid said green is red, because the failure is the newer fact.
lane_state() { # $1 = result, $2 = verdict, $3 = family default
  local result="$1" verdict="$2"
  case "$result" in
    success)
      if word_ok "$verdict"; then printf '%s' "$verdict"; else printf 'orange'; fi ;;
    failure)  printf 'red' ;;
    *)        printf 'orange' ;;
  esac
}

DEED_STATE="$(lane_state "$DEED_RESULT" "")"
K9_STATE="$(lane_state "$K9_RESULT" "$K9_VERDICT")"
HYP_STATE="$(lane_state "$HYPATIA_RESULT" "$HYPATIA_VERDICT")"

# DEED is a conformance lane with no grid of its own: success is green.
[ "$DEED_RESULT" = success ] && DEED_STATE=green

# Human label per state, in the vocabulary the rest of the estate uses.
label_of() {
  case "$1" in
    green)  printf 'conformant' ;;
    yellow) printf 'conformant (advisory debt)' ;;
    orange) printf 'indeterminate' ;;
    red)    printf 'failing' ;;
    purple) printf 'critical' ;;
    blue)   printf 'high findings' ;;
    grey)   printf 'not applicable' ;;
    *)      printf 'indeterminate' ;;
  esac
}

deed_cell="$(glyph_for "$DEED_STATE") $(label_of "$DEED_STATE")"
k9_cell="$(glyph_for "$K9_STATE") $(label_of "$K9_STATE")"
hyp_cell="$(glyph_for "$HYP_STATE") $(label_of "$HYP_STATE")"

# ── Compose ─────────────────────────────────────────────────────────────────
BODY="$OUT/body.md"

{
  # Marker first: the posting step finds its own previous comment by this and
  # updates it in place. One comment per pull request, newest run wins.
  echo '<!-- fork-governance-verdict: regenerated on every run -->'
  echo '# Fork governance'
  echo
  echo '| Lane | Status | Run |'
  echo '|---|---|---|'
  printf '| DEED syntax (repo deed) | %s | [run %s](%s) |\n' "$deed_cell" "$RUN_ID" "$RUN_URL"
  printf '| k9-deed conformance (L0–L2; L3 not verified) | %s (job: %s) | same run |\n' \
    "$k9_cell" "${K9_RESULT:-not run}"
  printf '| Hypatia verdict grid | %s (job: %s) | same run |\n' \
    "$hyp_cell" "${HYPATIA_RESULT:-not run}"
  echo
  echo 'Scope: the three lanes below are static checks at the named commit. A'
  echo 'green row means every applicable check in its declared scope passed;'
  echo 'orange means evidence is missing, not that nothing was found. No row is'
  echo 'a security certification.'
  echo

  # ── Lane 2: k9-deed conformance ──
  echo '<!-- fork-governance:lane k9-deed -->'
  echo '## k9-deed conformance'
  echo
  if [ -n "$K9_REPORT" ] && [ -s "$K9_REPORT" ]; then
    cat "$K9_REPORT"
  else
    printf '🟠 **indeterminate** — no k9-deed grid artifact was produced by the lane (job: %s).\n' \
      "${K9_RESULT:-not run}"
    echo
    echo 'The lane did not run to completion, so this row certifies nothing. Open'
    echo 'the run and read the k9 job log; a missing grid is never a pass.'
  fi
  echo

  # ── Lane 3: Hypatia verdict grid ──
  echo '<!-- fork-governance:lane hypatia -->'
  echo '## Hypatia verdict grid'
  echo
  if [ -n "$HYPATIA_REPORT" ] && [ -s "$HYPATIA_REPORT" ]; then
    cat "$HYPATIA_REPORT"
  else
    printf '🟠 **indeterminate** — no verdict-grid artifact was produced by the Hypatia lane (job: %s).\n' \
      "${HYPATIA_RESULT:-not run}"
    echo
    echo 'The lane did not run to completion, so this row certifies nothing. Open'
    echo 'the run and read the Hypatia job log; a missing grid is never a clean scan.'
  fi
} > "$BODY"

# ── Overall badge ──────────────────────────────────────────────────────────
# Worst lane wins, with red outranking orange so missing evidence cannot hide a
# known failure (VERDICT-GRID-SPEC §Conformance family, precedence).
overall="$DEED_STATE"
for s in "$K9_STATE" "$HYP_STATE"; do
  [ "$(rank_of "$s")" -gt "$(rank_of "$overall")" ] && overall="$s"
done
colour_for() {
  case "$1" in
    green)  printf 'brightgreen' ;;
    yellow) printf 'yellow' ;;
    orange) printf 'orange' ;;
    red)    printf 'red' ;;
    purple) printf 'blueviolet' ;;
    blue)   printf 'blue' ;;
    grey)   printf 'lightgrey' ;;
    *)      printf 'orange' ;;
  esac
}
if command -v jq >/dev/null 2>&1; then
  jq -n --arg m "$(label_of "$overall")" --arg c "$(colour_for "$overall")" \
    '{schemaVersion: 1, label: "Fork governance", message: $m, color: $c}' > "$OUT/badge.json"
else
  printf '{"schemaVersion":1,"label":"Fork governance","message":"%s","color":"%s"}\n' \
    "$(label_of "$overall")" "$(colour_for "$overall")" > "$OUT/badge.json"
fi

cat "$BODY"
