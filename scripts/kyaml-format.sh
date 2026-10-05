#!/usr/bin/env bash
# SPDX-License-Identifier: CC-BY-SA-4.0
# KYAML formatter/linter for arbitrary YAML files
# Part of standards#1022: formatter/linter for arbitrary YAML
#
# Usage:
#   kyaml-format.sh [--check] FILE...
#
# Flags:
#   --check    Check only, exit 2 if file would be changed
#
# Exit codes:
#   0  All files OK (no changes needed in --check mode, or all formatted successfully)
#   1  Error (parse failure, missing file, etc.)
#   2  File would be changed (--check mode only)
#
# Preconditions (per YAML-POLICY.adoc §3.3):
#   - GitHub Actions must parse KYAML workflows (proven in standards#1020)
#   - Comment preservation must be proven (standards#1021)
#
# This script runs `yq -o kyaml` verbatim and:
#   - Refuses (exit 1) a file that does not parse
#   - Refuses (exit 1) a file that has no final newline
#   - Refuses (exit 2 in --check mode) a file that would change its data
#   - Is idempotent: yq -o kyaml round-trips cleanly
#
# KYAML definition (KEP-5295):
#   - {} for every map and [] for every list (flow style)
#   - every string value double-quoted; keys unquoted where unambiguous
#   - trailing commas permitted
#   - two-space indentation
#   - --- document header
#
# Note: KYAML is a subset of YAML, not a new format. Every existing YAML
# parser already handles KYAML syntax.

set -euo pipefail

CHECK_MODE=false
FILES=()

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --check)
            CHECK_MODE=true
            shift
            ;;
        *)
            FILES+=("$1")
            shift
            ;;
    esac
done

if [[ ${#FILES[@]} -eq 0 ]]; then
    echo "Error: No files specified" >&2
    echo "Usage: $0 [--check] FILE..." >&2
    exit 1
fi

RETCODE=0
CHANGED=false

for file in "${FILES[@]}"; do
    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file" >&2
        exit 1
    fi

    # Check for final newline
    if [[ -n "$(tail -c 1 "$file" 2>/dev/null)" ]]; then
        echo "Error: File has no final newline: $file" >&2
        exit 1
    fi

    # Try to parse with yq (dry run first)
    if ! yq -o kyaml "$file" >/dev/null 2>&1; then
        echo "Error: File does not parse as YAML: $file" >&2
        exit 1
    fi

    if $CHECK_MODE; then
        # Check if formatting would change the file
        yq -o kyaml "$file" > /tmp/kyaml_check_$$
        if ! cmp -s "$file" /tmp/kyaml_check_$$; then
            echo "File would be changed: $file" >&2
            CHANGED=true
        fi
        rm -f /tmp/kyaml_check_$$
    else
        # Format the file in place
        yq -i -o kyaml "$file"
        if [[ $? -ne 0 ]]; then
            echo "Error: Failed to format: $file" >&2
            exit 1
        fi
    fi
done

if $CHANGED; then
    exit 2
fi

exit $RETCODE
