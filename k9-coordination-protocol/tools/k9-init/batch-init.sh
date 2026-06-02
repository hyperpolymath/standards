#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch-run k9-init across multiple repos.
#
# Usage: batch-init.sh [--dry-run] <repo-root-dir>
#   repo-root-dir: directory containing many repos (e.g. ~/Documents/hyperpolymath-repos/)
#
# Skips repos that already have coordination.k9.

set -eu

DRY_RUN=0
if [ "${1:-}" = "--dry-run" ]; then
    DRY_RUN=1
    shift
fi

ROOT="${1:-.}"
K9_INIT="$(dirname "$0")/target/release/k9-init"
[ -x "$K9_INIT" ] || K9_INIT="$(dirname "$0")/target/debug/k9-init"
[ -x "$K9_INIT" ] || { echo "k9-init binary not found — run cargo build first"; exit 1; }

echo "Scanning $ROOT for repos..."
migrated=0
skipped=0
no_6a2=0

for repo in "$ROOT"/*/; do
    [ -d "$repo" ] || continue
    name="$(basename "$repo")"
    case "$name" in
        .*|archive*|node_modules) continue ;;
    esac
    if [ -f "$repo/coordination.k9" ]; then
        skipped=$((skipped + 1))
        continue
    fi
    if [ ! -d "$repo/.machine_readable/6a2" ]; then
        no_6a2=$((no_6a2 + 1))
        continue
    fi
    if [ $DRY_RUN -eq 1 ]; then
        echo "WOULD init: $name"
    else
        echo "init: $name"
        "$K9_INIT" --out "$repo/coordination.k9" "$repo" >/dev/null
    fi
    migrated=$((migrated + 1))
done

echo ""
echo "Summary:"
echo "  migrated:     $migrated"
echo "  already had:  $skipped"
echo "  no 6a2 dir:   $no_6a2"
[ $DRY_RUN -eq 1 ] && echo "(dry-run — no files written)"
