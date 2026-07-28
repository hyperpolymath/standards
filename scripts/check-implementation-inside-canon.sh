#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# check-implementation-inside-canon.sh — Implementation-inside-Canon Detector
#
# Part of Issue #498: After-eviction hygiene.
#
# This script detects when implementation files (Cargo.toml, deno.json,
# Containerfile, CNAME, Justfile, etc.) appear in directories that should
# contain only canonical spec content. This prevents the "implementation
# creep" problem where product code gradually re-enters the canon repo.
#
# Usage: check-implementation-inside-canon.sh [DIRECTORY]
#   DIRECTORY defaults to current directory
#
# Exit codes:
#   0 = no implementation files found in canon directories
#   1 = implementation files detected
#   2 = error
#
# Implementation files that trigger detection:
#   - Cargo.toml (Rust package manifest)
#   - deno.json (Deno configuration)
#   - Containerfile (container image build)
#   - Dockerfile (container image build - legacy)
#   - CNAME (GitHub Pages custom domain)
#   - Justfile (just build system)
#   - Makefile (legacy build system)
#   - package.json (Node.js - banned but still detected)
#   - go.mod (Go module - banned but still detected)
#   - *.sh (shell scripts in spec directories)
#   - *.rs, *.py, *.ts (source code files in spec directories)
#
# Canon directories (should NOT contain implementation files):
#   - Any directory containing SPEC.adoc, REGISTRY.a2ml, or *.a2ml files
#   - docs/
#   - standards/ (if it exists)
#   - Any directory under .machine_readable/
#
set -euo pipefail

TARGET_DIR="${1:-.}"

# Implementation file patterns
IMPLEMENTATION_PATTERNS=(
  "Cargo.toml"
  "deno.json"
  "Containerfile"
  "Dockerfile"
  "CNAME"
  "Justfile"
  "Makefile"
  "package.json"
  "go.mod"
  "*.sh"
  "*.rs"
  "*.py"
  "*.ts"
)

# Canon directory indicators (if these exist, the dir is canon and shouldn't have impl files)
CANON_INDICATORS=(
  "SPEC.adoc"
  "REGISTRY.a2ml"
  "*.a2ml"
)

# Directories that are always canon
CANON_DIRS=(
  "docs"
  ".machine_readable"
)

errors=0
warnings=0

# Function to check if a directory is a canon directory
is_canon_dir() {
  local dir="$1"
  
  # Check if it's in the explicit canon dirs list
  for canon_dir in "${CANON_DIRS[@]}"; do
    if [[ "$dir" == "$canon_dir" || "$dir" == "$canon_dir/"* ]]; then
      return 0
    fi
  done
  
  # Check for canon indicators
  for pattern in "${CANON_INDICATORS[@]}"; do
    if find "$dir" -maxdepth 2 -name "$pattern" -not -path "*/.git/*" | grep -q .; then
      return 0
    fi
  done
  
  return 1
}

# Function to check for implementation files in a directory
check_dir() {
  local dir="$1"
  
  # Skip if not a canon directory
  if ! is_canon_dir "$dir"; then
    return 0
  fi
  
  # Check for implementation files
  for pattern in "${IMPLEMENTATION_PATTERNS[@]}"; do
    local files
    files=$(find "$dir" -maxdepth 2 -name "$pattern" -not -path "*/.git/*" 2>/dev/null)
    if [ -n "$files" ]; then
      echo "::error file=$dir::Implementation file detected in canon directory: $files"
      echo "  Canon directories should contain only spec, policy, and template files."
      echo "  Implementation files belong in separate repos per Issue #479."
      return 1
    fi
  done
  
  return 0
}

# Main check
echo "Checking for implementation files in canon directories..."

# Check the target directory
if ! check_dir "$TARGET_DIR"; then
  errors=$((errors + 1))
fi

# Check all subdirectories
while IFS= read -r -d '' dir; do
  if ! check_dir "$dir"; then
    errors=$((errors + 1))
  fi
done < <(find "$TARGET_DIR" -mindepth 1 -maxdepth 2 -type d -not -path "*/.git/*" -print0)

if [ $errors -gt 0 ]; then
  echo ""
  echo "❌ Found $errors canon directories with implementation files."
  echo "  See Issue #479 for the carve-out campaign."
  exit 1
else
  echo "✅ No implementation files found in canon directories."
  exit 0
fi
