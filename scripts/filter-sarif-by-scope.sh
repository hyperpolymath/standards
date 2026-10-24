#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# filter-sarif-by-scope.sh — remove results under declared out-of-scope path
# prefixes from a SARIF file before upload.
#
# WHY THIS IS A SEPARATE SCRIPT FROM filter-sarif-by-baseline.sh
#
#   A BASELINE says "we know about this finding, here is why we accept it".
#   It is an argument about a specific result, it expires, and removing the
#   entry brings the finding back.
#
#   A SCOPE says "this part of the tree is not what the scanner is for". It is
#   an argument about a whole prefix, it must never expire, and it applies to
#   findings nobody has looked at yet.
#
#   Conflating them was the alternative and it is strictly worse: 18 of this
#   repository's 63 baseline entries existed only to acknowledge findings in
#   9-archive/, which is frozen history that cannot be edited without
#   falsifying the record. Every one of them was a copy of the same argument,
#   and a new file added to the archive would need a nineteenth.
#
# WHY IT MATTERS FOR CODE SCANNING, NOT JUST THE GRID
#
#   CodeQL is configured with `paths-ignore: 9-archive/**`. Hypatia was not, so
#   the same tree produced a stream of alerts that the CodeQL lane would never
#   have raised, and a ruleset blocking on those alerts could not be satisfied
#   by any commit. Excluding the prefix at the SARIF boundary closes that gap
#   for the one producer that lacked it.
#
# FAIL OPEN, ALWAYS.
#
#   Every error path copies the input through unchanged. A filter that
#   silently over-excludes hides real findings behind a green upload; a filter
#   that under-excludes shows findings someone did not want to see. The second
#   failure is visible and recoverable, the first is not.
#
# USAGE
#   filter-sarif-by-scope.sh IN.sarif OUT.sarif PREFIX [PREFIX...]
#
#   Prefixes are matched against `artifactLocation.uri` after normalisation
#   (leading "./" and "/" stripped). Results are removed when any location's
#   URI starts with a prefix. A result with NO location is KEPT: it cannot be
#   shown to be out of scope, and keeping it is the loud direction.
#
# EXIT CODES
#   0  filtered, or copied through unchanged (see the message on stderr)
#   2  usage error
set -uo pipefail

SARIF="${1:-}"
OUT="${2:-}"
shift 2 || true
PREFIXES=("$@")

if [ -z "$SARIF" ] || [ -z "$OUT" ] || [ "${#PREFIXES[@]}" -eq 0 ]; then
  echo "usage: filter-sarif-by-scope.sh IN.sarif OUT.sarif PREFIX [PREFIX...]" >&2
  exit 2
fi

if [ ! -s "$SARIF" ]; then
  echo "  no SARIF at $SARIF — nothing to filter"
  [ "$OUT" != "$SARIF" ] && : > "$OUT"
  exit 0
fi

copy_through() { [ "$OUT" = "$SARIF" ] || cp "$SARIF" "$OUT"; }

command -v jq >/dev/null 2>&1 || { echo "  jq unavailable — SARIF copied unfiltered"; copy_through; exit 0; }

PREFIX_JSON=$(printf '%s\n' "${PREFIXES[@]}" | jq -R -s 'split("\n") | map(select(length > 0))')

BEFORE=$(jq '[.runs[]?.results[]?] | length' "$SARIF" 2>/dev/null) || BEFORE=""

if ! jq --argjson prefixes "$PREFIX_JSON" '
      def norm: (. // "") | sub("^\\./"; "") | sub("^/"; "");
      def out_of_scope:
        [ .locations[]?.physicalLocation.artifactLocation.uri | norm ] as $uris
        | ($uris | length) > 0
          and all($uris[]; . as $u | any($prefixes[]; . as $p | $u | startswith($p)));
      .runs = ((.runs // []) | map(.results = ((.results // []) | map(select(out_of_scope | not)))))
    ' "$SARIF" > "$OUT.tmp" 2>/dev/null; then
  echo "  ::warning::SARIF scope filter failed — uploading UNFILTERED rather than a partial file"
  rm -f "$OUT.tmp"
  copy_through
  exit 0
fi

if ! jq -e 'type == "object" and (.runs | type) == "array"' "$OUT.tmp" >/dev/null 2>&1; then
  echo "  ::warning::SARIF scope filter produced an unreadable file — uploading UNFILTERED"
  rm -f "$OUT.tmp"
  copy_through
  exit 0
fi
mv "$OUT.tmp" "$OUT"

AFTER=$(jq '[.runs[]?.results[]?] | length' "$OUT" 2>/dev/null || echo "?")
echo "  SARIF results: ${BEFORE:-?} -> ${AFTER}  (out of scope: $(printf '%s ' "${PREFIXES[@]}"))"
exit 0
