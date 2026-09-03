#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# Script to replace duplicate workflow files with calls to root reusable workflows
# Usage: bash scripts/fix-workflow-duplicates.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

echo "Repository root: $REPO_ROOT"
echo ""

# Function to create a wrapper workflow that calls a reusable
create_wrapper() {
    local target_dir="$1"
    local workflow_name="$2"
    local reusable_name="$3"
    local relative_path="$4"
    
    local target_file="$target_dir/.github/workflows/$workflow_name"
    
    if [ ! -f "$target_file" ]; then
        echo "SKIP: $target_file does not exist"
        return 0
    fi
    
    # Determine the depth and calculate relative path
    # Count how many levels deep the target is
    local depth=$(echo "$target_dir" | grep -o '/' | wc -l)
    local base_path=""
    for ((i=0; i<depth; i++)); do
        base_path="$base_path../"
    done
    base_path="$base_path.github/workflows/$reusable_name"
    
    echo "Processing: $target_file -> uses: $base_path"
    
    # Create the wrapper content
    cat > "$target_file" <<EOF
# SPDX-License-Identifier: MPL-2.0
name: $(echo "$workflow_name" | sed 's/\.yml$//' | sed 's/^/ /' | sed 's/-//g')

on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]

permissions:
  contents: read

jobs:
  job:
    uses: $base_path
EOF
    
    echo "UPDATED: $target_file"
}

# Function to find all subdirectories with .github/workflows/
find_workflow_dirs() {
    find . -type d -path "*/.github/workflows" ! -path "./.github/workflows" ! -path "./.git/*"
}

echo "Finding all workflow directories..."
mapfile -t DIRS < <(find_workflow_dirs)
echo "Found ${#DIRS[@]} workflow directories"
echo ""

# For each directory, check for duplicate workflows and replace them
for dir in "${DIRS[@]}"; do
    # Skip the root .github/workflows
    if [ "$dir" = "./.github/workflows" ]; then
        continue
    fi
    
    echo "Processing directory: $dir"
    
    # Check for standard workflow files
    for workflow in codeql.yml governance.yml hypatia-scan.yml instant-sync.yml mirror.yml scorecard.yml scorecard-enforcer.yml secret-scanner.yml; do
        if [ -f "$dir/$workflow" ]; then
            echo "  Found $workflow"
            # For now, just report - we'll fix in batches
        fi
    done
    
done

echo ""
echo "Scan complete. Use targeted fixes for each subdirectory."
