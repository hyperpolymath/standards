#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch-run the inline-annotations extractor across an estate.
# Emits one A2ML file per repo under OUT_DIR.
#
# Usage: batch-extract.sh <repo-root-dir> [OUT_DIR]

set -eu

ROOT="${1:-.}"
OUT_DIR="${2:-/tmp/inline-annotations-output}"
EXTRACTOR="$(dirname "$0")/target/release/inline-annotations"
[ -x "$EXTRACTOR" ] || EXTRACTOR="$(dirname "$0")/target/debug/inline-annotations"
[ -x "$EXTRACTOR" ] || { echo "inline-annotations binary not found — run cargo build first"; exit 1; }

mkdir -p "$OUT_DIR"
scanned=0
with_annotations=0

for repo in "$ROOT"/*/; do
    [ -d "$repo" ] || continue
    name="$(basename "$repo")"
    case "$name" in
        .*|archive*|node_modules) continue ;;
    esac
    outfile="$OUT_DIR/$name.a2ml"
    "$EXTRACTOR" --out "$outfile" "$repo" 2>/dev/null
    scanned=$((scanned + 1))
    # check if any @annotation blocks
    if grep -q "^@annotation" "$outfile" 2>/dev/null; then
        with_annotations=$((with_annotations + 1))
    else
        rm "$outfile"  # empty output — don't keep
    fi
done

echo "Scanned $scanned repos, $with_annotations had annotations."
echo "Output: $OUT_DIR/"
