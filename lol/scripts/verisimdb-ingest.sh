#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors
#
# VeriSimDB Ingest Helper
# Runs corpus verification and ingests results into verisimdb-data

set -euo pipefail

SCAN_OUTPUT="${1:-/tmp/lol-scan.json}"
VERISIMDB_DATA="${VERISIMDB_DATA_DIR:-$HOME/Documents/hyperpolymath-repos/verisimdb-data}"

echo "=== lol VeriSimDB Ingest Pipeline ==="

# Step 1: Run corpus verification
echo "[1/3] Running corpus quality analysis..."
deno run -A src/Lang1000.res.mjs verify --output "$SCAN_OUTPUT"
echo "  Scan written to: $SCAN_OUTPUT"

# Step 2: Show summary
echo "[2/3] Scan summary:"
if command -v jq &>/dev/null; then
    weak_count=$(jq '.weak_points | length' "$SCAN_OUTPUT")
    total_lines=$(jq '.statistics.total_lines' "$SCAN_OUTPUT")
    echo "  Weak points: $weak_count"
    echo "  Total lines: $total_lines"
else
    echo "  (install jq for summary stats)"
fi

# Step 3: Ingest into verisimdb-data
echo "[3/3] Ingesting into verisimdb-data..."
if [ -d "$VERISIMDB_DATA" ] && [ -f "$VERISIMDB_DATA/scripts/ingest-scan.sh" ]; then
    cd "$VERISIMDB_DATA"
    ./scripts/ingest-scan.sh lol "$SCAN_OUTPUT"
    echo "  Ingested successfully."
    echo "  Remember to: cd $VERISIMDB_DATA && git push && git push gitlab main"
else
    echo "  verisimdb-data not found at: $VERISIMDB_DATA"
    echo "  Manual ingest: cp $SCAN_OUTPUT <verisimdb-data>/scans/lol.json"
fi

echo "=== Done ==="
