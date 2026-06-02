#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# classify-secret-scanner.sh — classify per-repo secret-scanner.yml for #190 sweep.
#
# Canonical (standards/.github/workflows/secret-scanner.yml @ 080c394):
#   4 jobs — trufflehog, gitleaks, rust-secrets, shell-secrets
#
# Classifies each estate copy as:
#   TRIVIAL                  — exactly the canonical 4 jobs, no extras.
#   MISSING_SHELL_SECRETS    — 3 canonical jobs (trufflehog/gitleaks/rust-secrets);
#                              the post-Cloudflare-leak guardrail is absent.
#   MISSING_RUST_SECRETS     — 2 jobs (trufflehog/gitleaks); slim variant.
#   NEEDS_REVIEW             — extra jobs, missing trufflehog/gitleaks core,
#                              line count out of band, or otherwise unrecognised.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=_lib.sh
. "$SCRIPT_DIR/_lib.sh"

INPUT="${1:-/tmp/drift-survey/secret-scanner-full.json}"
BLOBS_DIR="${BLOBS_DIR:-/tmp/drift-survey/secret-blobs}"
mkdir -p "$BLOBS_DIR"

CORE_JOBS="trufflehog,gitleaks"
ALL_CANONICAL="gitleaks,rust-secrets,shell-secrets,trufflehog"
MISSING_SHELL="gitleaks,rust-secrets,trufflehog"
SLIM_2JOB="gitleaks,trufflehog"

extract_jobs() {
  awk '
    /^jobs:[[:space:]]*$/ { in_jobs=1; next }
    in_jobs && /^[A-Za-z]/ { exit }
    in_jobs && /^  [a-z][a-z0-9_-]*:[[:space:]]*$/ {
      sub(/^  /, ""); sub(/:[[:space:]]*$/, ""); print
    }
  ' "$1" 2>/dev/null | sort -u | paste -sd, -
}

classify_blob() {
  local blob="$1" jobs lines
  jobs=$(extract_jobs "$blob")
  jobs="${jobs:-NONE}"
  lines=$(wc -l < "$blob")

  case "$jobs" in
    "$ALL_CANONICAL")    echo "TRIVIAL	-	$lines	$jobs" ;;
    "$MISSING_SHELL")    echo "MISSING_SHELL_SECRETS	+shell-secrets	$lines	$jobs" ;;
    "$SLIM_2JOB")        echo "MISSING_RUST_SHELL	+rust-secrets,+shell-secrets	$lines	$jobs" ;;
    *)                   echo "NEEDS_REVIEW	job_set:$jobs	$lines	$jobs" ;;
  esac
}

# 1. Fetch unique blobs (cached).
echo "[fetch] retrieving unique blobs..." >&2
normalize_input "$INPUT" | awk -F'\t' '{print $3 "\t" $1}' | sort -u | while IFS=$'\t' read -r sha repo; do
  [ -z "$sha" ] || [ "$sha" = "null" ] && continue
  blob_file="$BLOBS_DIR/$sha.yml"
  [ -s "$blob_file" ] && continue
  gh api "/repos/hyperpolymath/$repo/git/blobs/$sha" --jq '.content' 2>/dev/null \
    | base64 -d > "$blob_file" || echo "::warn fetch failed for $sha ($repo)" >&2
done

# 2. Classify unique blobs.
declare -A SHA_CLASS
echo "[classify] classifying unique blobs..." >&2
for blob in "$BLOBS_DIR"/*.yml; do
  [ -s "$blob" ] || continue
  sha=$(basename "$blob" .yml)
  SHA_CLASS[$sha]=$(classify_blob "$blob")
done

# 3. Per-(repo, path) TSV.
normalize_input "$INPUT" | while IFS=$'\t' read -r repo path sha; do
  [ -z "$sha" ] || [ "$sha" = "null" ] && { printf '%s\t%s\t%s\tNEEDS_REVIEW\tnull_sha\t-\t-\n' "$repo" "$path" "$sha"; continue; }
  printf '%s\t%s\t%s\t%s\n' "$repo" "$path" "$sha" "${SHA_CLASS[$sha]:-NEEDS_REVIEW	fetch_failed	-	-}"
done
