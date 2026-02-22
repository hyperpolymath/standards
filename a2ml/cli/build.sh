#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Build script for A2ML CLI

set -euo pipefail

VERSION="0.7.0"
CLI_NAME="a2ml"
BUILD_DIR="build"
SRC_DIR="../src"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() { echo -e "${BLUE}ℹ️  ${NC}$1"; }
success() { echo -e "${GREEN}✓${NC} $1"; }
warn() { echo -e "${YELLOW}⚠️  ${NC}$1"; }

echo "╔══════════════════════════════════════════════════════════╗"
echo "║         Building A2ML CLI v$VERSION                      ║"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""

# Check for Idris2
if ! command -v idris2 >/dev/null 2>&1; then
    echo "Error: idris2 not found. Please install Idris2 from https://idris-lang.org"
    exit 1
fi

info "Idris2 version:"
idris2 --version
echo ""

# Create build directory
info "Creating build directory..."
mkdir -p "$BUILD_DIR"
success "Build directory created"

# Build the CLI
info "Compiling A2ML CLI..."

# Find the .ipkg file
IPKG_FILE="../a2ml.ipkg"
if [ ! -f "$IPKG_FILE" ]; then
    warn "No .ipkg file found, creating one..."
    cat > "$IPKG_FILE" <<EOF
package a2ml

version = $VERSION
authors = "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"
license = "PMPL-1.0-or-later"

sourcedir = "src"
builddir = "build"

modules = A2ML.TypedCore
        , A2ML.Proofs
        , A2ML.Parser
        , A2ML.Converters
        , A2ML.Tests

main = Main
executable = a2ml
EOF
fi

# Build with Idris2
cd ..
if idris2 --build a2ml.ipkg; then
    success "CLI compiled successfully"
else
    echo "Error: Compilation failed"
    exit 1
fi

# Find the executable
if [ -f "build/exec/a2ml" ]; then
    CLI_PATH="build/exec/a2ml"
elif [ -f "build/exec/$CLI_NAME" ]; then
    CLI_PATH="build/exec/$CLI_NAME"
else
    warn "Could not find executable, checking other locations..."
    CLI_PATH=$(find build -name "$CLI_NAME" -type f | head -1)
fi

if [ -n "$CLI_PATH" ] && [ -f "$CLI_PATH" ]; then
    success "Executable created: $CLI_PATH"

    # Test the executable
    info "Testing executable..."
    if "$CLI_PATH" version >/dev/null 2>&1; then
        success "Executable test passed"
    else
        warn "Executable test failed (may still work)"
    fi

    # Show binary info
    echo ""
    info "Binary information:"
    ls -lh "$CLI_PATH"
    echo ""

    # Installation instructions
    echo "╔══════════════════════════════════════════════════════════╗"
    echo "║  ✅ Build Complete!                                     ║"
    echo "╚══════════════════════════════════════════════════════════╝"
    echo ""
    echo "To install:"
    echo "  sudo cp $CLI_PATH /usr/local/bin/$CLI_NAME"
    echo ""
    echo "Or add to PATH:"
    echo "  export PATH=\"\$PATH:$(pwd)/$(dirname $CLI_PATH)\""
    echo ""
    echo "Test with:"
    echo "  $CLI_PATH version"
    echo "  $CLI_PATH help"
    echo ""
else
    echo "Error: Could not find compiled executable"
    exit 1
fi
