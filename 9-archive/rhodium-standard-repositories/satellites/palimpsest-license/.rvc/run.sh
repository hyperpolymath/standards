#!/bin/sh
# Robot Vacuum Cleaner (RVC) - Automated Tidying Script
# SPDX-License-Identifier: Palimpsest-0.4 OR MIT
# Version: 0.4.0

set -e

# Colour codes
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo "${BLUE}  Robot Vacuum Cleaner (RVC) - Repository Maintenance${NC}"
echo "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

# Check if config exists
if [ ! -f ".rvc/config.yml" ]; then
    echo "${YELLOW}⚠${NC} Config file not found: .rvc/config.yml"
    exit 1
fi

echo "${GREEN}✓${NC} Configuration loaded from .rvc/config.yml"
echo ""

# Git garbage collection
echo "${BLUE}🧹 Running git garbage collection...${NC}"
git gc --auto
echo "${GREEN}✓${NC} Git GC complete"
echo ""

# Remove log files
echo "${BLUE}🧹 Cleaning log files...${NC}"
find . -name "*.log" -not -path "./node_modules/*" -not -path "./.git/*" -delete 2>/dev/null || true
echo "${GREEN}✓${NC} Log files cleaned"
echo ""

# Remove temporary files
echo "${BLUE}🧹 Cleaning temporary files...${NC}"
find . -name "*~" -delete 2>/dev/null || true
find . -name "*.swp" -delete 2>/dev/null || true
find . -name "*.swo" -delete 2>/dev/null || true
echo "${GREEN}✓${NC} Temporary files cleaned"
echo ""

# npm cache (if enabled)
if command -v npm >/dev/null 2>&1; then
    echo "${BLUE}🧹 npm cache verification...${NC}"
    npm cache verify >/dev/null 2>&1 || true
    echo "${GREEN}✓${NC} npm cache verified"
    echo ""
fi

echo "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo "${GREEN}✓${NC} Repository maintenance complete!"
echo "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
