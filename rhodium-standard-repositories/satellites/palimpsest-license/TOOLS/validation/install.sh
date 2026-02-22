#!/usr/bin/env bash
# Installation script for Palimpsest Validator

set -e

echo "=== Palimpsest Validator Installation ==="
echo ""

# Detect OS
OS=$(uname -s)
echo "Detected OS: $OS"

# Check for Haskell toolchain
echo ""
echo "Checking for Haskell toolchain..."

if command -v ghc &> /dev/null && command -v cabal &> /dev/null; then
    echo "✓ GHC and Cabal found"
    GHC_VERSION=$(ghc --version | grep -oE '[0-9]+\.[0-9]+\.[0-9]+')
    CABAL_VERSION=$(cabal --version | head -n1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+')
    echo "  GHC version: $GHC_VERSION"
    echo "  Cabal version: $CABAL_VERSION"
    USE_CABAL=true
elif command -v stack &> /dev/null; then
    echo "✓ Stack found"
    STACK_VERSION=$(stack --version | head -n1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+')
    echo "  Stack version: $STACK_VERSION"
    USE_CABAL=false
else
    echo "❌ No Haskell toolchain found!"
    echo ""
    echo "Please install GHCup to get started:"
# WARNING: Pipe-to-shell is unsafe — download and verify first
# WARNING: Pipe-to-shell is unsafe — download and verify first
# WARNING: Pipe-to-shell is unsafe — download and verify first
# WARNING: Pipe-to-shell is unsafe — download and verify first
    echo "  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
    echo ""
    echo "Or install Stack:"
# WARNING: Pipe-to-shell is unsafe — download and verify first
# WARNING: Pipe-to-shell is unsafe — download and verify first
# WARNING: Pipe-to-shell is unsafe — download and verify first
# WARNING: Pipe-to-shell is unsafe — download and verify first
    echo "  curl -sSL https://get.haskellstack.org/ | sh"
    echo ""
    exit 1
fi

# Navigate to Haskell directory
cd "$(dirname "$0")/haskell"

echo ""
echo "Building Palimpsest Validator..."

if [ "$USE_CABAL" = true ]; then
    # Update cabal package list
    echo "Updating Cabal package list..."
    cabal update

    # Build project
    echo "Building project..."
    cabal build

    # Install executable
    echo "Installing executable..."
    cabal install --overwrite-policy=always --installdir="$HOME/.local/bin"

    INSTALL_DIR="$HOME/.local/bin"
else
    # Build with Stack
    echo "Building with Stack..."
    stack build

    # Install executable
    echo "Installing executable..."
    stack install

    INSTALL_DIR="$HOME/.local/bin"
fi

echo ""
echo "✅ Installation complete!"
echo ""
echo "The 'palimpsest-validate' executable has been installed to: $INSTALL_DIR"
echo ""

# Check if install dir is in PATH
if [[ ":$PATH:" == *":$INSTALL_DIR:"* ]]; then
    echo "✓ $INSTALL_DIR is in your PATH"
    echo ""
    echo "You can now run: palimpsest-validate --help"
else
    echo "⚠️  $INSTALL_DIR is not in your PATH"
    echo ""
    echo "Add this to your shell configuration (~/.bashrc or ~/.zshrc):"
    echo "  export PATH=\"\$HOME/.local/bin:\$PATH\""
    echo ""
    echo "Then run: source ~/.bashrc  (or ~/.zshrc)"
fi

echo ""
echo "Usage examples:"
echo "  palimpsest-validate project --root /path/to/palimpsest-license"
echo "  palimpsest-validate license LICENSES/v0.4/palimpsest-v0.4.md"
echo "  palimpsest-validate --help"
