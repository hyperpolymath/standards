#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# readme-vocab-gate.sh — fail-closed vocabulary-preservation gate for the
# README single-source derivation (ADR-004).
#
# Guards against content loss when README.adoc is converted to README.md.
# Extracts the prose vocabulary (lowercased, >=4-char alphanumeric tokens,
# minus a markup stoplist) from BOTH the canonical .adoc and the derived .md,
# then requires the derived file to retain at least THRESHOLD% of the
# canonical file's unique content tokens. Exits non-zero if it falls short,
# so a lossy conversion can never land silently.
#
# Usage: readme-vocab-gate.sh <canonical.adoc> <derived.md> [threshold_pct]
#   threshold_pct defaults to 98.
set -euo pipefail

CANON="${1:?usage: readme-vocab-gate.sh <canonical.adoc> <derived.md> [threshold_pct]}"
DERIVED="${2:?missing derived file}"
THRESHOLD="${3:-98}"

[ -f "$CANON" ]   || { echo "::error::vocab-gate: canonical file not found: $CANON"; exit 2; }
[ -f "$DERIVED" ] || { echo "::error::vocab-gate: derived file not found: $DERIVED"; exit 2; }

# Markup / syntax tokens that appear in one format but not the other and carry
# no prose meaning. Kept deliberately small; extend only with true noise.
STOPLIST='http|https|www|html|adoc|markdown|xmlns|docbook|svg|png|jpg|jpeg|gif|href|link|image|images|toc|sectnums|sectnumlevels|toclevels|revnumber|revdate|revremark|source|highlighter|rouge|icons|font|doctype|imagesdir|experimental|nbsp|middot|span|div|code|pre|nolang|lang'

# Extract unique content tokens from a file.
#   - lowercase
#   - split on any non-alphanumeric
#   - keep tokens with length >= 4
#   - drop pure numbers and stoplist tokens
extract_tokens() {
  tr '[:upper:]' '[:lower:]' < "$1" \
    | tr -c 'a-z0-9' '\n' \
    | awk 'length($0) >= 4 && $0 !~ /^[0-9]+$/' \
    | grep -Evx "$STOPLIST" \
    | sort -u
}

CANON_TOKENS="$(mktemp)"
DERIVED_TOKENS="$(mktemp)"
trap 'rm -f "$CANON_TOKENS" "$DERIVED_TOKENS"' EXIT

extract_tokens "$CANON"   > "$CANON_TOKENS"
extract_tokens "$DERIVED" > "$DERIVED_TOKENS"

TOTAL=$(wc -l < "$CANON_TOKENS" | tr -d ' ')
if [ "$TOTAL" -eq 0 ]; then
  echo "::error::vocab-gate: canonical file has no content tokens — refusing to pass a vacuous gate"
  exit 3
fi

# Tokens present in canonical but MISSING from derived.
MISSING="$(comm -23 "$CANON_TOKENS" "$DERIVED_TOKENS")"
MISSING_COUNT=$(printf '%s\n' "$MISSING" | grep -c . || true)
KEPT=$(( TOTAL - MISSING_COUNT ))

# Integer percentage with one-decimal reporting via *10 arithmetic.
COVERAGE_X10=$(( KEPT * 1000 / TOTAL ))
THRESHOLD_X10=$(( THRESHOLD * 10 ))

printf 'vocab-gate: %d/%d canonical content tokens retained (%d.%d%%), threshold %d%%\n' \
  "$KEPT" "$TOTAL" "$(( COVERAGE_X10 / 10 ))" "$(( COVERAGE_X10 % 10 ))" "$THRESHOLD"

if [ "$COVERAGE_X10" -lt "$THRESHOLD_X10" ]; then
  echo "::error::vocab-gate FAILED — derived README dropped ${MISSING_COUNT} content token(s) below the ${THRESHOLD}% floor."
  echo "First missing tokens:"
  printf '%s\n' "$MISSING" | head -40 | sed 's/^/  - /'
  exit 1
fi

echo "✓ vocab-gate passed"
