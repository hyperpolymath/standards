#!/usr/bin/env bash
# SPDX-License-Identifier: CC-BY-SA-4.0
# KYAML Migration Script
# Standards#1024 (non-bot YAML) and #1025 (workflows)
# Owner ruling: "KYAML everywhere, workflows included" (standards#1023)

set -euo pipefail

CHECK_MODE=false
WORKFLOWS=false
DRY_RUN=false
REPO_PATH="."

while [[ $# -gt 0 ]]; do
    case "$1" in
        --check) CHECK_MODE=true ;;
        --workflows) WORKFLOWS=true ;;
        --dry-run) DRY_RUN=true ;;
        *)
            if [[ -d "$1" ]]; then
                REPO_PATH="$1"
            fi
            ;;
    esac
    shift
done

cd "$REPO_PATH"

echo "=== KYAML Migration ==="
echo "Repository: $(pwd)"
echo "Check mode: $CHECK_MODE"
echo "Include workflows: $WORKFLOWS"
echo "Dry run: $DRY_RUN"
echo ""

# Find YAML files
YAML_FILES=()
while IFS= read -r -d '' file; do
    YAML_FILES+=("$file")
done < <(find . -type f \( -name "*.yaml" -o -name "*.yml" \) \
    ! -path "./.git/*" \
    ! -path "node_modules/*" \
    ! -path "target/*" \
    ! -path "vendor/*" \
    ! -path ".migration-tmp/*" \
    ! -path "archive/*" \
    -print0 2>/dev/null)

# Filter out workflows if not including them
if ! $WORKFLOWS; then
    FILTERED_FILES=()
    for file in "${YAML_FILES[@]}"; do
        if [[ "$file" != *".github/workflows/"* ]]; then
            FILTERED_FILES+=("$file")
        fi
    done
    YAML_FILES=("${FILTERED_FILES[@]}")
fi

echo "Found ${#YAML_FILES[@]} YAML files to check"
echo ""

if $DRY_RUN; then
    echo "Files that would be converted:"
    for file in "${YAML_FILES[@]}"; do
        echo "  $file"
    done
    exit 0
fi

# Scratch file for each candidate rewrite: private (mktemp), never a fixed
# /tmp name, and removed on every exit path.
NEW_FILE="$(mktemp)"
trap 'rm -f "$NEW_FILE"' EXIT

CHANGED=false
ERRORS=0
CONVERTED=0
SKIPPED=0

for file in "${YAML_FILES[@]}"; do
    echo -n "$file ... "
    
    # Check for final newline
    if [[ -n "$(tail -c 1 "$file" 2>/dev/null || true)" ]]; then
        echo "SKIP (no final newline)"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi
    
    # Try to parse with yq
    if ! yq -o kyaml "$file" > "$NEW_FILE" 2>&1; then
        echo "ERROR (parse failed)"
        ERRORS=$((ERRORS + 1))
        : > "$NEW_FILE"
        continue
    fi
    
    # Check if conversion would change the file
    if cmp -s "$file" "$NEW_FILE"; then
        echo "OK (already KYAML)"
        : > "$NEW_FILE"
        continue
    fi
    
    if $CHECK_MODE; then
        echo "WOULD CHANGE"
        CHANGED=true
        : > "$NEW_FILE"
        continue
    fi
    
    # Actually convert the file
    if $DRY_RUN; then
        echo "WOULD CONVERT"
        CHANGED=true
    else
        cat "$NEW_FILE" > "$file"  # keep the target mode; mktemp is 0600
        echo "CONVERTED"
        CONVERTED=$((CONVERTED + 1))
    fi
done

echo ""
echo "=== Summary ==="
echo "Converted: $CONVERTED"
echo "Skipped: $SKIPPED"
echo "Errors: $ERRORS"

if $CHANGED && $CHECK_MODE; then
    exit 2
fi

if [[ $ERRORS -gt 0 ]]; then
    exit 1
fi

exit 0
