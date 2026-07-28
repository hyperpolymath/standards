#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# auto-archive-session-detritus.sh — Session Detritus Auto-Archiver
#
# Part of Issue #498: After-eviction hygiene.
#
# This script identifies and archives session detritus files (COMPLETE reports,
# SESSION_SUMMARY files, DEPLOYMENT-SUCCESS.md, etc.) that accumulate at the
# repository root or in spec directories. These files are point-in-time reports
# that should be archived to docs/archive/ for historical reference.
#
# Usage: auto-archive-session-detritus.sh [--dry-run] [--force] [DIRECTORY]
#   --dry-run: Show what would be moved without actually moving
#   --force: Move files even if they haven't been modified in a while
#   DIRECTORY: Directory to scan (defaults to current directory)
#
# Exit codes:
#   0 = success (files archived or nothing to do)
#   1 = files found but not archived (dry-run mode)
#   2 = error
#
# Session detritus patterns:
#   - *-COMPLETE*.md
#   - *SESSION_SUMMARY*.md
#   - *SESSION_SUMMARY*.adoc
#   - DEPLOYMENT-SUCCESS.md
#   - DEVELOPMENT_SESSION_SUMMARY.md
#   - DEPLOYMENT-SESSION_*.md
#   - *SESSION-*.md
#   - *COMPLETE-*.md
#
set -euo pipefail

DRY_RUN=false
FORCE=false
TARGET_DIR="${1:-.}"

# Parse arguments
while [ $# -gt 0 ]; do
  case "$1" in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --force)
      FORCE=true
      shift
      ;;
    -*)
      echo "Unknown option: $1"
      exit 2
      ;;
    *)
      TARGET_DIR="$1"
      shift
      ;;
  esac
done

# Session detritus patterns
DETRITUS_PATTERNS=(
  "*COMPLETE*.md"
  "*SESSION_SUMMARY*.md"
  "*SESSION_SUMMARY*.adoc"
  "DEPLOYMENT-SUCCESS.md"
  "DEVELOPMENT_SESSION_SUMMARY.md"
  "DEPLOYMENT-SESSION_*.md"
  "*SESSION-*.md"
  "*COMPLETE-*.md"
)

# Directories to skip
SKIP_DIRS=(
  ".git"
  "docs/archive"
  "node_modules"
  ".github"
)

# Ensure archive directory exists
mkdir -p "$TARGET_DIR/docs/archive"

moved=0
found=0

# Function to check if a file matches detritus patterns
is_detritus() {
  local file="$1"
  local filename
  filename=$(basename "$file")
  
  for pattern in "${DETRITUS_PATTERNS[@]}"; do
    if [[ "$filename" == $pattern ]]; then
      return 0
    fi
  done
  
  return 1
}

# Function to check if a file is in a skip directory
in_skip_dir() {
  local file="$1"
  
  for skip_dir in "${SKIP_DIRS[@]}"; do
    if [[ "$file" == *"$skip_dir"* ]]; then
      return 0
    fi
  done
  
  return 1
}

# Find and archive session detritus files
echo "Scanning for session detritus files in $TARGET_DIR..."

while IFS= read -r -d '' file; do
  # Skip files in skip directories
  if in_skip_dir "$file"; then
    continue
  fi
  
  # Check if it's a detritus file
  if is_detritus "$file"; then
    found=$((found + 1))
    
    # Generate archive path
    local archive_path
    archive_path="$TARGET_DIR/docs/archive/$(basename "$file" | sed 's/\./_/g')_$(date +%Y%m%d_%H%M%S)"
    
    if [ "$DRY_RUN" = true ]; then
      echo "[DRY-RUN] Would move: $file → $archive_path"
    else
      # Only move if file is recent or force is enabled
      if [ "$FORCE" = true ] || [ -n "$(find "$file" -mmin -1440 2>/dev/null)" ]; then
        mv "$file" "$archive_path"
        echo "✅ Archived: $file → $archive_path"
        moved=$((moved + 1))
      else
        echo "ℹ️  Skipping (old, use --force): $file"
      fi
    fi
  fi
done < <(find "$TARGET_DIR" -maxdepth 1 -type f -not -path "*/.git/*" -print0)

echo ""
if [ $found -gt 0 ]; then
  echo "Found $found session detritus files."
  if [ "$DRY_RUN" = true ]; then
    echo "Use without --dry-run to actually archive them."
    exit 1
  else
    echo "Archived $moved files."
    if [ $moved -lt $found ]; then
      echo "$((found - moved)) files were skipped (old, use --force to archive anyway)."
    fi
    exit 0
  fi
else
  echo "✅ No session detritus files found."
  exit 0
fi
