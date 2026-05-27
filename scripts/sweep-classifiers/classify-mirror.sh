#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# classify-mirror.sh — classify per-repo mirror.yml for the #187 sweep.
#
# Reads /tmp/drift-survey/mirror-full.json (output of `gh api --paginate
# /search/code` filtered to mirror.yml in .github/workflows). For each
# unique blob SHA, fetches the blob once (cached in $BLOBS_DIR) and
# classifies as:
#
#   TRIVIAL       — exactly the canonical 7 forges, line count in band,
#                   no extra/missing top-level jobs.
#   NEEDS_REVIEW  — anything else (extra forges, missing forges,
#                   structural diff). Reported with reason.
#
# Output: TSV to stdout, one row per repo:
#   <repo>\t<sha>\t<classification>\t<reason>\t<line_count>\t<forges>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=_lib.sh
. "$SCRIPT_DIR/_lib.sh"

INPUT="${1:-/tmp/drift-survey/mirror-full.json}"
BLOBS_DIR="${BLOBS_DIR:-/tmp/drift-survey/blobs}"
mkdir -p "$BLOBS_DIR"

# Canonical 7 forges (alphabetical for stable compare).
CANON_FORGES="bitbucket,codeberg,disroot,gitea,gitlab,radicle,sourcehut"
LINE_MIN=130
LINE_MAX=160

classify_blob() {
  local blob="$1"
  local forges line_count extra_jobs

  # Forges: top-level jobs under `jobs:` starting with mirror-.
  forges=$(awk '
    /^jobs:[[:space:]]*$/ { in_jobs=1; next }
    in_jobs && /^[A-Za-z]/ { exit }
    in_jobs && /^  mirror-[a-z]+:[[:space:]]*$/ {
      sub(/^  mirror-/, ""); sub(/:[[:space:]]*$/, ""); print
    }
  ' "$blob" 2>/dev/null | sort -u | paste -sd, -)
  forges="${forges:-NONE}"
  line_count=$(wc -l < "$blob")

  # Any non-mirror top-level jobs? Scope to the `jobs:` block (between
  # `^jobs:$` and the next column-0 key, EOF, or comment-only/blank).
  extra_jobs=$(awk '
    /^jobs:[[:space:]]*$/ { in_jobs=1; next }
    in_jobs && /^[A-Za-z]/ { exit }   # next column-0 key ends the block
    in_jobs && /^  [a-z][a-z0-9_-]*:[[:space:]]*$/ {
      sub(/^  /, ""); sub(/:[[:space:]]*$/, ""); print
    }
  ' "$blob" 2>/dev/null | grep -vE '^mirror-' | paste -sd, -)
  extra_jobs="${extra_jobs:-}"

  if [ "$forges" != "$CANON_FORGES" ]; then
    echo "NEEDS_REVIEW	forge_set:$forges	$line_count	$forges"
    return
  fi
  if [ -n "$extra_jobs" ]; then
    echo "NEEDS_REVIEW	extra_jobs:$extra_jobs	$line_count	$forges"
    return
  fi
  if [ "$line_count" -lt "$LINE_MIN" ] || [ "$line_count" -gt "$LINE_MAX" ]; then
    echo "NEEDS_REVIEW	line_count_oob:$line_count	$line_count	$forges"
    return
  fi
  echo "TRIVIAL	-	$line_count	$forges"
}

# 1. Fetch each unique SHA's blob (cached).
echo "[fetch] unique SHAs to retrieve..." >&2
normalize_input "$INPUT" | awk -F'\t' '{print $3 "\t" $1}' | sort -u | while IFS=$'\t' read -r sha repo; do
  [ -z "$sha" ] || [ "$sha" = "null" ] && continue
  blob_file="$BLOBS_DIR/$sha.yml"
  [ -s "$blob_file" ] && continue
  gh api "/repos/hyperpolymath/$repo/git/blobs/$sha" --jq '.content' 2>/dev/null \
    | base64 -d > "$blob_file" || echo "::warn fetch failed for $sha ($repo)" >&2
done

# 2. Classify each unique SHA, cache in a map.
declare -A SHA_CLASS
echo "[classify] classifying unique blobs..." >&2
for blob in "$BLOBS_DIR"/*.yml; do
  [ -s "$blob" ] || continue
  sha=$(basename "$blob" .yml)
  SHA_CLASS[$sha]=$(classify_blob "$blob")
done

# 3. Emit per-(repo, path) TSV: repo \t path \t sha \t class \t reason \t lines \t forges
normalize_input "$INPUT" | while IFS=$'\t' read -r repo path sha; do
  [ -z "$sha" ] || [ "$sha" = "null" ] && { printf '%s\t%s\t%s\tNEEDS_REVIEW\tnull_sha\t-\t-\n' "$repo" "$path" "$sha"; continue; }
  printf '%s\t%s\t%s\t%s\n' "$repo" "$path" "$sha" "${SHA_CLASS[$sha]:-NEEDS_REVIEW	fetch_failed	-	-}"
done
