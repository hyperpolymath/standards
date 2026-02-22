#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# install.sh - Install k9-sign system-wide or user-local

set -euo pipefail

VERSION="1.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

info() {
    echo -e "${BLUE}ℹ️  ${NC}$1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

warn() {
    echo -e "${YELLOW}⚠️  ${NC}$1"
}

error() {
    echo -e "${RED}❌ ${NC}$1" >&2
}

usage() {
    cat <<EOF
k9-sign installer v$VERSION

Usage: ./install.sh [OPTIONS]

Options:
  --system        Install system-wide to /usr/local/bin (requires sudo)
  --user          Install to ~/.local/bin (default)
  --prefix PATH   Install to custom prefix PATH/bin
  --build-only    Just build, don't install
  --help          Show this help message

Examples:
  ./install.sh                    # Install to ~/.local/bin
  ./install.sh --system           # Install to /usr/local/bin
  ./install.sh --prefix ~/tools   # Install to ~/tools/bin
  ./install.sh --build-only       # Just build the binary

Environment Variables:
  CARGO_BUILD_JOBS   Number of parallel build jobs (default: auto)
  TARGET             Build target triple (default: native)

Requirements:
  - Rust toolchain (cargo, rustc)
  - ~1GB disk space for build artifacts
  - ~2 minutes build time on modern hardware
EOF
}

check_rust() {
    if ! command -v cargo >/dev/null 2>&1; then
        error "Cargo not found. Install Rust from https://rustup.rs/"
        echo ""
        echo "Quick install:"
# WARNING: Pipe-to-shell is unsafe — download and verify first
# WARNING: Pipe-to-shell is unsafe — download and verify first
        echo "  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
        exit 1
    fi

    info "Rust toolchain found:"
    cargo --version
    rustc --version
}

build_binary() {
    info "Building k9-sign in release mode..."
    echo ""

    cd "$SCRIPT_DIR"

    # Clean previous builds (optional)
    if [ -d target/release ]; then
        warn "Cleaning previous release build..."
        cargo clean --release
    fi

    # Build with optimizations
    if ! cargo build --release; then
        error "Build failed. Check error messages above."
        exit 1
    fi

    BINARY_PATH="$SCRIPT_DIR/target/release/k9-sign"

    if [ ! -f "$BINARY_PATH" ]; then
        error "Binary not found at $BINARY_PATH"
        exit 1
    fi

    BINARY_SIZE=$(du -h "$BINARY_PATH" | cut -f1)
    success "Build complete! Binary size: $BINARY_SIZE"
    echo ""
}

run_tests() {
    info "Running test suite..."
    echo ""

    cd "$SCRIPT_DIR"

    if cargo test --release -- --test-threads=1; then
        success "All tests passed!"
        echo ""
    else
        error "Tests failed. Aborting installation."
        exit 1
    fi
}

install_binary() {
    local install_dir="$1"
    local binary_path="$SCRIPT_DIR/target/release/k9-sign"
    local dest="$install_dir/k9-sign"

    # Create install directory if needed
    if [ ! -d "$install_dir" ]; then
        info "Creating directory: $install_dir"
        mkdir -p "$install_dir"
    fi

    # Check if we need sudo
    local use_sudo=""
    if [ ! -w "$install_dir" ]; then
        warn "Need elevated privileges for $install_dir"
        use_sudo="sudo"
    fi

    # Check if binary already exists
    if [ -f "$dest" ]; then
        warn "k9-sign already installed at $dest"
        local old_version=""
        if old_version=$("$dest" --version 2>/dev/null | head -1); then
            echo "  Current: $old_version"
        fi
        echo "  New: k9-sign $VERSION"
        read -p "Overwrite? [y/N] " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            info "Installation cancelled."
            exit 0
        fi
    fi

    # Install
    info "Installing k9-sign to $dest..."
    if [ -n "$use_sudo" ]; then
        $use_sudo cp "$binary_path" "$dest"
        $use_sudo chmod 755 "$dest"
    else
        cp "$binary_path" "$dest"
        chmod 755 "$dest"
    fi

    success "Installed to $dest"
    echo ""
}

verify_installation() {
    local binary_path="$1"

    info "Verifying installation..."

    if ! command -v k9-sign >/dev/null 2>&1; then
        warn "k9-sign not in PATH"
        echo ""
        echo "Add to PATH by adding this to your shell profile (~/.bashrc or ~/.zshrc):"
        echo "  export PATH=\"$(dirname "$binary_path"):\$PATH\""
        echo ""
        echo "Or run directly:"
        echo "  $binary_path --version"
        echo ""
    else
        success "k9-sign is in PATH"
        k9-sign --version
        echo ""
    fi

    info "Quick test:"
    echo ""
    echo "  k9-sign keygen test-key"
    echo "  k9-sign list"
    echo ""
}

main() {
    local install_mode="user"
    local install_prefix=""
    local build_only=false

    # Parse arguments
    while [ $# -gt 0 ]; do
        case "$1" in
            --system)
                install_mode="system"
                shift
                ;;
            --user)
                install_mode="user"
                shift
                ;;
            --prefix)
                install_prefix="$2"
                shift 2
                ;;
            --build-only)
                build_only=true
                shift
                ;;
            --help|-h)
                usage
                exit 0
                ;;
            *)
                error "Unknown option: $1"
                usage
                exit 1
                ;;
        esac
    done

    echo "╔══════════════════════════════════════════════════════════╗"
    echo "║         k9-sign Installer v$VERSION                      ║"
    echo "╚══════════════════════════════════════════════════════════╝"
    echo ""

    # Determine install directory
    local install_dir=""
    if [ -n "$install_prefix" ]; then
        install_dir="$install_prefix/bin"
        info "Install mode: Custom prefix"
        info "Install directory: $install_dir"
    elif [ "$install_mode" = "system" ]; then
        install_dir="/usr/local/bin"
        info "Install mode: System-wide"
        info "Install directory: $install_dir"
    else
        install_dir="$HOME/.local/bin"
        info "Install mode: User-local"
        info "Install directory: $install_dir"
    fi
    echo ""

    # Check prerequisites
    check_rust
    echo ""

    # Build
    build_binary

    # Test
    run_tests

    # Install or just build
    if [ "$build_only" = true ]; then
        info "Build-only mode, skipping installation."
        info "Binary available at: $SCRIPT_DIR/target/release/k9-sign"
        exit 0
    fi

    install_binary "$install_dir"

    # Verify
    verify_installation "$install_dir/k9-sign"

    success "Installation complete!"
    echo ""
    echo "Next steps:"
    echo "  1. Generate a keypair:  k9-sign keygen"
    echo "  2. Sign a component:    k9-sign sign file.k9.ncl"
    echo "  3. Verify signature:    k9-sign verify file.k9.ncl"
    echo ""
    echo "Documentation: $(dirname "$SCRIPT_DIR")/docs/SECURITY-BEST-PRACTICES.adoc"
    echo "Report issues: https://github.com/hyperpolymath/k9-svc/issues"
}

main "$@"
