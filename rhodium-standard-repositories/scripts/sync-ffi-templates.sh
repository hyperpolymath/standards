#!/bin/bash
# sync-ffi-templates.sh — Sync standard FFI/ABI boilerplate across the estate
#
# This script applies standard templates for Zig FFI and Idris2 ABI to repos
# that use the common hyperpolymath FFI pattern.
#
# SPDX-License-Identifier: MPL-2.0

set -e

TEMPLATES_DIR="$(dirname "$0")/../templates"
ZIG_TEMPLATE="$TEMPLATES_DIR/ffi_main.zig.template"
IDR_TEMPLATE="$TEMPLATES_DIR/ffi_foreign.idr.template"

if [ ! -f "$ZIG_TEMPLATE" ] || [ ! -f "$IDR_TEMPLATE" ]; then
    echo "Error: Templates not found in $TEMPLATES_DIR"
    exit 1
fi

# Function to convert kebab-case to snake_case
to_snake_case() {
    echo "$1" | tr '-' '_'
}

# Function to convert kebab-case to SCREAMING_SNAKE_CASE
to_screaming_snake_case() {
    echo "$1" | tr '-' '_' | tr '[:lower:]' '[:upper:]'
}

# Function to convert kebab-case to PascalCase
to_pascal_case() {
    echo "$1" | sed -r 's/(^|-)([a-z])/\U\2/g'
}

# Process a single repository
sync_repo() {
    local repo_path="$1"
    local repo_name=$(basename "$repo_path")
    
    local project_snake=$(to_snake_case "$repo_name")
    local project_screaming=$(to_screaming_snake_case "$repo_name")
    local project_pascal=$(to_pascal_case "$repo_name")

    echo "Syncing $repo_name..."

    # Sync Zig FFI if it exists
    if [ -f "$repo_path/ffi/zig/src/main.zig" ]; then
        echo "  Updating Zig FFI..."
        sed -e "s/{{project}}/$project_snake/g" \
            -e "s/STANDARDS/$project_screaming/g" \
            "$ZIG_TEMPLATE" > "$repo_path/ffi/zig/src/main.zig"
    fi

    # Sync Idris2 ABI if it exists
    if [ -f "$repo_path/src/abi/Foreign.idr" ]; then
        echo "  Updating Idris2 ABI..."
        sed -e "s/{{project}}/$project_snake/g" \
            -e "s/STANDARDS/$project_screaming/g" \
            -e "s/{{Project}}/$project_pascal/g" \
            "$IDR_TEMPLATE" > "$repo_path/src/abi/Foreign.idr"
    fi
}

# Find and process all repos
REPOS_ROOT="/var/mnt/eclipse/repos"

if [ $# -gt 0 ]; then
    for repo_path in "$@"; do
        # Handle relative or absolute paths
        if [[ "$repo_path" != /* ]]; then
            repo_path="$REPOS_ROOT/$repo_path"
        fi
        if [ -d "$repo_path" ]; then
            sync_repo "$repo_path"
        else
            echo "Error: Directory $repo_path not found"
        fi
    done
else
    declare -A processed_repos
    find "$REPOS_ROOT" -maxdepth 2 -type d \( -name "ffi" -o -name "src" \) | while read -r dir; do
        repo_path=$(dirname "$dir")
        if [ -f "$repo_path/ffi/zig/src/main.zig" ] || [ -f "$repo_path/src/abi/Foreign.idr" ]; then
            if [ -z "${processed_repos[$repo_path]}" ]; then
                sync_repo "$repo_path"
                processed_repos["$repo_path"]=1
            fi
        fi
    done
fi

echo "Done."
