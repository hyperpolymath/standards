#!/bin/sh
# Palimpsest License - Git Hooks Setup Script
# SPDX-License-Identifier: Palimpsest-0.4 OR MIT
# Version: 0.4.0
#
# This script installs git hooks from .git-hooks/ directory
# Supports both direct installation and git config approach

set -e

# Colour codes for output (British spelling in comments)
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Colour

# Helper functions
print_success() {
    printf "${GREEN}✓${NC} %s\n" "$1"
}

print_error() {
    printf "${RED}✗${NC} %s\n" "$1"
}

print_warning() {
    printf "${YELLOW}⚠${NC} %s\n" "$1"
}

print_info() {
    printf "${BLUE}ℹ${NC} %s\n" "$1"
}

print_header() {
    echo ""
    printf "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"
    printf "${BLUE}  %s${NC}\n" "$1"
    printf "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"
    echo ""
}

# Check if we're in a git repository
if ! git rev-parse --git-dir >/dev/null 2>&1; then
    print_error "Not a git repository"
    print_info "Run this script from the root of your git repository"
    exit 1
fi

print_header "Palimpsest License - Git Hooks Setup"

# Get git directory
GIT_DIR=$(git rev-parse --git-dir)
HOOKS_DIR="$GIT_DIR/hooks"
SOURCE_DIR=".git-hooks"

# Check if source hooks directory exists
if [ ! -d "$SOURCE_DIR" ]; then
    print_error "Source hooks directory not found: $SOURCE_DIR"
    exit 1
fi

print_info "Git directory: $GIT_DIR"
print_info "Hooks directory: $HOOKS_DIR"
print_info "Source directory: $SOURCE_DIR"

# Create hooks directory if it doesn't exist
if [ ! -d "$HOOKS_DIR" ]; then
    print_info "Creating hooks directory..."
    mkdir -p "$HOOKS_DIR"
    print_success "Hooks directory created"
fi

# ============================================================================
# INSTALLATION METHOD SELECTION
# ============================================================================

echo ""
print_info "Choose installation method:"
echo "  1) Symlink (recommended - auto-updates when hooks change)"
echo "  2) Copy (standalone - doesn't auto-update)"
echo "  3) Git config core.hooksPath (global for this repo)"
echo ""

# Default to symlink if not interactive
if [ -t 0 ]; then
    printf "Select method [1]: "
    read -r method
    method=${method:-1}
else
    print_info "Non-interactive mode, using symlink method"
    method=1
fi

# ============================================================================
# INSTALLATION
# ============================================================================

case "$method" in
    1)
        # Symlink method
        print_header "Installing Hooks (Symlink Method)"

        for hook in "$SOURCE_DIR"/*; do
            if [ -f "$hook" ]; then
                hook_name=$(basename "$hook")
                target="$HOOKS_DIR/$hook_name"

                # Check if hook already exists
                if [ -e "$target" ] || [ -L "$target" ]; then
                    print_warning "Existing hook found: $hook_name"

                    if [ -t 0 ]; then
                        printf "  Overwrite? [y/N]: "
                        read -r overwrite
                        if [ "$overwrite" != "y" ] && [ "$overwrite" != "Y" ]; then
                            print_info "Skipping $hook_name"
                            continue
                        fi
                    else
                        print_info "Non-interactive mode, backing up existing hook"
                        mv "$target" "$target.backup.$(date +%Y%m%d-%H%M%S)"
                    fi

                    rm -f "$target"
                fi

                # Create symlink (use relative path)
                ln -s "../../$hook" "$target"
                chmod +x "$hook"
                print_success "Installed: $hook_name (symlink)"
            fi
        done
        ;;

    2)
        # Copy method
        print_header "Installing Hooks (Copy Method)"

        for hook in "$SOURCE_DIR"/*; do
            if [ -f "$hook" ]; then
                hook_name=$(basename "$hook")
                target="$HOOKS_DIR/$hook_name"

                # Check if hook already exists
                if [ -e "$target" ]; then
                    print_warning "Existing hook found: $hook_name"

                    if [ -t 0 ]; then
                        printf "  Overwrite? [y/N]: "
                        read -r overwrite
                        if [ "$overwrite" != "y" ] && [ "$overwrite" != "Y" ]; then
                            print_info "Skipping $hook_name"
                            continue
                        fi
                    else
                        print_info "Non-interactive mode, backing up existing hook"
                        mv "$target" "$target.backup.$(date +%Y%m%d-%H%M%S)"
                    fi
                fi

                # Copy hook
                cp "$hook" "$target"
                chmod +x "$target"
                print_success "Installed: $hook_name (copy)"
            fi
        done
        ;;

    3)
        # Git config method
        print_header "Installing Hooks (Git Config Method)"

        # Check git version (core.hooksPath requires Git 2.9+)
        git_version=$(git --version | sed 's/git version //')
        print_info "Git version: $git_version"

        # Set core.hooksPath
        git config core.hooksPath "$SOURCE_DIR"
        print_success "Git config set: core.hooksPath = $SOURCE_DIR"

        # Make hooks executable
        for hook in "$SOURCE_DIR"/*; do
            if [ -f "$hook" ]; then
                chmod +x "$hook"
                hook_name=$(basename "$hook")
                print_success "Made executable: $hook_name"
            fi
        done

        print_info "All hooks in $SOURCE_DIR are now active"
        print_info "No need to reinstall when hooks change"
        ;;

    *)
        print_error "Invalid selection: $method"
        exit 1
        ;;
esac

# ============================================================================
# VERIFICATION
# ============================================================================

print_header "Verification"

# List installed hooks
if [ "$method" = "3" ]; then
    # Git config method
    hooks_path=$(git config core.hooksPath)
    print_info "Hooks path: $hooks_path"

    for hook in "$SOURCE_DIR"/*; do
        if [ -f "$hook" ]; then
            hook_name=$(basename "$hook")
            if [ -x "$hook" ]; then
                print_success "$hook_name (active, executable)"
            else
                print_error "$hook_name (not executable)"
            fi
        fi
    done
else
    # Symlink or copy method
    for hook in "$SOURCE_DIR"/*; do
        if [ -f "$hook" ]; then
            hook_name=$(basename "$hook")
            target="$HOOKS_DIR/$hook_name"

            if [ -L "$target" ]; then
                print_success "$hook_name (symlink)"
            elif [ -f "$target" ]; then
                print_success "$hook_name (copy)"
            else
                print_warning "$hook_name (not installed)"
            fi
        fi
    done
fi

# ============================================================================
# ADDITIONAL INFORMATION
# ============================================================================

print_header "Setup Complete"

echo "Installed hooks:"
echo ""
for hook in "$SOURCE_DIR"/*; do
    if [ -f "$hook" ]; then
        hook_name=$(basename "$hook")
        echo "  • $hook_name"

        case "$hook_name" in
            pre-commit)
                echo "    - SPDX header validation"
                echo "    - Format checking (Prettier)"
                echo "    - Spell checking (British English)"
                echo "    - Secret scanning"
                echo "    - License audit"
                ;;
            pre-push)
                echo "    - Full test suite"
                echo "    - Security audit"
                echo "    - Build verification"
                echo "    - RSR compliance check"
                ;;
        esac
        echo ""
    fi
done

print_info "To bypass hooks (not recommended):"
echo "  git commit --no-verify"
echo "  git push --no-verify"
echo ""

print_info "To uninstall hooks:"
if [ "$method" = "3" ]; then
    echo "  git config --unset core.hooksPath"
else
    echo "  rm "$HOOKS_DIR"/pre-commit $HOOKS_DIR/pre-push"
fi
echo ""

print_info "To update hooks:"
if [ "$method" = "1" ]; then
    echo "  Symlinks auto-update when source files change"
elif [ "$method" = "3" ]; then
    echo "  Changes in $SOURCE_DIR automatically apply"
else
    echo "  Re-run this script: ./setup-hooks.sh"
fi
echo ""

print_success "Git hooks setup complete!"

exit 0
