#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# classify-hypatia-scan.sh — classify per-repo hypatia-scan.yml for #193 sweep.
#
# Canonical (standards/.github/workflows/hypatia-scan.yml): 416 lines,
# single `scan` job. Every estate variant carries the same job set;
# drift is pure propagation lag, manifested as line count.
#
# Classes:
#   TRIVIAL_CURRENT          — 410-416 lines (current canonical-1)
#   SLIM_PROPAGATION_LAG     — 245-260 lines (older slimmer version)
#   OLDER_SLIM               — 205-235 lines (much older slim version)
#   NEEDS_REVIEW             — anything else (multi-job, line count out of band,
#                              or self-source like hypatia/standards repos)

set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=_lib.sh
. "$SCRIPT_DIR/_lib.sh"

INPUT="${1:-/tmp/drift-survey/hypatia-scan-full.json}"
BLOBS_DIR="${BLOBS_DIR:-/tmp/drift-survey/hypatia-blobs}"
mkdir -p "$BLOBS_DIR"

classify_blob() {
  local blob="$1" jobs lines
  jobs=$(awk '/^jobs:[[:space:]]*$/{in_jobs=1; next} in_jobs && /^[A-Za-z]/{exit} in_jobs && /^  [a-z][a-z0-9_-]*:[[:space:]]*$/{sub(/^  /,""); sub(/:.*/,""); print}' "$blob" 2>/dev/null | sort -u | paste -sd, -)
  jobs="${jobs:-NONE}"
  lines=$(wc -l < "$blob")
  if [ "$jobs" != "scan" ]; then
    echo "NEEDS_REVIEW	job_set:$jobs	$lines	$jobs"
    return
  fi
  case "$lines" in
    41[0-9]|42[0-9])  echo "TRIVIAL_CURRENT	-	$lines	scan" ;;
    24[0-9]|25[0-9])  echo "SLIM_PROPAGATION_LAG	+upgrade-to-current	$lines	scan" ;;
    20[5-9]|21[0-9]|22[0-9]|23[0-9])  echo "OLDER_SLIM	+upgrade-to-current	$lines	scan" ;;
    *)  echo "NEEDS_REVIEW	line_count_oob:$lines	$lines	scan" ;;
  esac
}

echo "[fetch] retrieving unique blobs..." >&2
normalize_input "$INPUT" | awk -F'\t' '{print $3 "\t" $1}' | sort -u | while IFS=$'\t' read -r sha repo; do
  [ -z "$sha" ] || [ "$sha" = "null" ] && continue
  blob_file="$BLOBS_DIR/$sha.yml"
  [ -s "$blob_file" ] && continue
  gh api "/repos/hyperpolymath/$repo/git/blobs/$sha" --jq '.content' 2>/dev/null \
    | base64 -d > "$blob_file" || echo "::warn fetch failed for $sha ($repo)" >&2
done

declare -A SHA_CLASS
echo "[classify] classifying unique blobs..." >&2
for blob in "$BLOBS_DIR"/*.yml; do
  [ -s "$blob" ] || continue
  sha=$(basename "$blob" .yml)
  SHA_CLASS[$sha]=$(classify_blob "$blob")
done

normalize_input "$INPUT" | while IFS=$'\t' read -r repo path sha; do
  [ -z "$sha" ] || [ "$sha" = "null" ] && { printf '%s\t%s\t%s\tNEEDS_REVIEW\tnull_sha\t-\t-\n' "$repo" "$path" "$sha"; continue; }
  printf '%s\t%s\t%s\t%s\n' "$repo" "$path" "$sha" "${SHA_CLASS[$sha]:-NEEDS_REVIEW	fetch_failed	-	-}"
done
