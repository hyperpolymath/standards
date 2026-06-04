#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
# Migration script for Issue #150: Convert standalone rsr-antipattern.yml to reusable workflow
#
# Usage:
#   ./migrate-rsr-antipattern.sh /path/to/repo [--dry-run]
#
# This script:
# 1. Backs up existing rsr-antipattern.yml
# 2. Replaces it with a call to the reusable workflow
# 3. Reports success/failure

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
REUSABLE_WORKFLOW="hyperpolymath/standards/.github/workflows/rsr-antipattern-reusable.yml@main"
BACKUP_DIR=".rsr-migration-backup"
NEW_FILE_CONTENT='# SPDX-License-Identifier: MPL-2.0
# RSR Anti-Pattern Check - Uses reusable workflow from standards

name: RSR Anti-Pattern Check

on:
  push:
    branches: [main, master, develop]
  pull_request:
    branches: [main, master, develop]

jobs:
  antipattern-check:
    uses: hyperpolymath/standards/.github/workflows/rsr-antipattern-reusable.yml@main
'

# Parse arguments
DRY_RUN=false
REPO_PATH=""

for arg in "$@"; do
    case "$arg" in
        --dry-run)
            DRY_RUN=true
            ;;
        *)
            if [ -z "$REPO_PATH" ]; then
                REPO_PATH="$arg"
            fi
            ;;
    esac
done

if [ -z "$REPO_PATH" ]; then
    echo "${RED}Error: Repository path required${NC}"
    echo "Usage: $0 /path/to/repo [--dry-run]"
    exit 1
fi

if [ ! -d "$REPO_PATH/.github/workflows" ]; then
    echo "${RED}Error: No .github/workflows directory in $REPO_PATH${NC}"
    exit 1
fi

WORKFLOW_FILE="$REPO_PATH/.github/workflows/rsr-antipattern.yml"

if [ ! -f "$WORKFLOW_FILE" ]; then
    echo "${YELLOW}Warning: No rsr-antipattern.yml found in $REPO_PATH${NC}"
    echo "This might be a repo that doesn't have one yet, or it's in a different location."
    exit 0
fi

echo "${BLUE}========================================${NC}"
echo "${BLUE}Repo: $REPO_PATH${NC}"
echo "${BLUE}========================================${NC}"

# Check if it's already migrated
if grep -q "uses:.*rsr-antipattern-reusable.yml" "$WORKFLOW_FILE" 2>/dev/null; then
    echo "${GREEN}✓ Already migrated${NC}"
    exit 0
fi

# Create backup directory
if [ "$DRY_RUN" = false ]; then
    mkdir -p "$REPO_PATH/$BACKUP_DIR/workflows"
    cp "$WORKFLOW_FILE" "$REPO_PATH/$BACKUP_DIR/workflows/rsr-antipattern.yml.$(date +%Y%m%d-%H%M%S).bak"
    echo "${YELLOW}✓ Backed up to: $REPO_PATH/$BACKUP_DIR/workflows/${NC}"
fi

# Show current file size
CURRENT_SIZE=$(wc -l < "$WORKFLOW_FILE")
echo "${YELLOW}Current file: $CURRENT_SIZE lines${NC}"

# Dry run mode
if [ "$DRY_RUN" = true ]; then
    echo "${YELLOW}[DRY RUN] Would replace with:${NC}"
    echo "$NEW_FILE_CONTENT"
    echo "${YELLOW}[DRY RUN] Would delete backup after verification${NC}"
    exit 0
fi

# Write new content
if echo "$NEW_FILE_CONTENT" > "$WORKFLOW_FILE"; then
    NEW_SIZE=$(echo "$NEW_FILE_CONTENT" | wc -l)
    echo "${GREEN}✓ Replaced with new file: $NEW_SIZE lines${NC}"
    
    # Verify the change
    if grep -q "uses:.*rsr-antipattern-reusable.yml" "$WORKFLOW_FILE"; then
        echo "${GREEN}✓ Migration successful${NC}"
        
        # Show git status
        if [ -d "$REPO_PATH/.git" ]; then
            echo "${YELLOW}Git status:${NC}"
            cd "$REPO_PATH" && git status --short .github/workflows/rsr-antipattern.yml 2>/dev/null || true
        fi
        exit 0
    else
        echo "${RED}✗ Migration failed - new content not found${NC}"
        exit 1
    fi
else
    echo "${RED}✗ Failed to write new file${NC}"
    exit 1
fi
