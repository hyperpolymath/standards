#!/usr/bin/env bash
set -euo pipefail

# Palimpsest Repository Scaffolding Script
# Organizes repository structure and generates LICENSE pointer

# Ensure golden folders exist
mkdir -p LICENSES docs ci

# Move licence texts to LICENSES/
for f in PALIMPSEST-*.txt; do
  [ -e "$f" ] || continue
  mv "$f" LICENSES/
done

# Move stray docs to docs/
for f in *.md; do
  case "$f" in
    README.md|CHANGELOG.md|CONTRIBUTING.md|FUNDING.md|CLAUDE.md) ;;
    *) mv "$f" docs/ ;;
  esac
done

# Generate LICENSE pointer to latest version
latest=$(ls LICENSES/PALIMPSEST-*.txt 2>/dev/null | sort -V | tail -n1 || echo "")
if [ -n "$latest" ]; then
  version=$(basename "$latest" .txt | cut -d- -f2)
  cat > LICENSE <<EOF
Palimpsest License — Canonical Repository
Copyright (c) 2025 Jonathan

The authoritative licence text is maintained in LICENSES/.
Current version: Palimpsest $version
See $latest for full terms.
EOF
  echo "✅ Repo tidied and LICENSE updated to Palimpsest $version"
else
  echo "⚠️  No PALIMPSEST-*.txt files found in LICENSES/"
fi
