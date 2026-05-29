#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# classify-codeql.sh — classify per-repo codeql.yml for #192 sweep.
#
# Canonical: 49 lines, single `analyze` job, matrix language=javascript-typescript
# build-mode=none.
#
# Classes:
#   TRIVIAL_DEFAULT       — single javascript-typescript language, default wrapper
#   SINGLE_NON_DEFAULT    — single rust or actions language, override wrapper
#   MULTI_LANGUAGE        — 2+ languages, multi-call wrapper
#   NEEDS_REVIEW          — NONE language matrix, or large custom workflow

set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=_lib.sh
. "$SCRIPT_DIR/_lib.sh"

INPUT="${1:-/tmp/drift-survey/codeql-full.json}"
BLOBS_DIR="${BLOBS_DIR:-/tmp/drift-survey/codeql-blobs}"
mkdir -p "$BLOBS_DIR"

classify_blob() {
  local blob="$1" langs lines jobs
  jobs=$(awk '/^jobs:[[:space:]]*$/{in_jobs=1; next} in_jobs && /^[A-Za-z]/{exit} in_jobs && /^  [a-z][a-z0-9_-]*:[[:space:]]*$/{sub(/^  /,""); sub(/:.*/,""); print}' "$blob" 2>/dev/null | sort -u | paste -sd, -)
  langs=$(grep -E "^\s*- language:" "$blob" 2>/dev/null | sed 's/.*language: //; s/[[:space:]]*$//' | sort -u | paste -sd, -)
  lines=$(wc -l < "$blob")
  langs="${langs:-NONE}"

  if [ "$lines" -gt 80 ]; then
    echo "NEEDS_REVIEW	custom_workflow_${lines}_lines	$lines	$langs"
    return
  fi
  case "$langs" in
    javascript-typescript)  echo "TRIVIAL_DEFAULT	-	$lines	$langs" ;;
    rust|actions)           echo "SINGLE_NON_DEFAULT	+language=$langs	$lines	$langs" ;;
    NONE)                   echo "NEEDS_REVIEW	no_language_matrix	$lines	$langs" ;;
    *,*)                    echo "MULTI_LANGUAGE	+per-language-wrapper	$lines	$langs" ;;
    *)                      echo "NEEDS_REVIEW	unknown_lang:$langs	$lines	$langs" ;;
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
