#!/usr/bin/env bash
# SPDX-License-Identifier: CC-BY-SA-4.0
# Migration tool: a2ml to .deed campaign converter
# Priority order: rsr-template-repo, hypatia, .git-private-farm, standards, gitbot-fleet, cicd-squabbler

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="${1:-}"
DRY_RUN=false
LENIENT=false
VERBOSE=false
SPECIFIC_TYPE=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run) DRY_RUN=true ;;
        --lenient) LENIENT=true ;;
        --verbose) VERBOSE=true ;;
        --type) SPECIFIC_TYPE="$2" ; shift ;;
        --all) SPECIFIC_TYPE="all" ;;
        *) REPO_ROOT="$1" ;;
    esac
    shift
 done

if [[ -z "$REPO_ROOT" || ! -d "$REPO_ROOT" ]]; then
    echo "Error: Valid REPO_PATH is required" >&2
    echo "Usage: $0 [--dry-run] [--lenient] [--verbose] [--type TYPE] REPO_PATH" >&2
    exit 1
fi

log() {
    if $VERBOSE; then
        echo "[VERBOSE] $1"
    fi
}

log "Starting migration in $REPO_ROOT"

# Count .a2ml files
A2ML_COUNT=$(find "$REPO_ROOT" -name "*.a2ml" -type f 2>/dev/null | wc -l)
log "Found $A2ML_COUNT .a2ml files"

# For now, just detect and report - full migration to be implemented
if $DRY_RUN; then
    echo "[DRY RUN] Detected $A2ML_COUNT .a2ml files in $REPO_ROOT"
    find "$REPO_ROOT" -name "*.a2ml" -type f 2>/dev/null | while read -r f; do
        echo "  Would process: $f"
    done
    exit 0
fi

echo "Migration tool ready. Currently in dry-run only mode."
echo "To perform actual migration, use: $0 REPO_PATH"
echo "Found $A2ML_COUNT .a2ml files in $REPO_ROOT"
