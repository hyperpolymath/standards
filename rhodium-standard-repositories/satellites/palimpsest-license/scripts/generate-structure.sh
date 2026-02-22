#!/bin/bash
# Generate directory structure for README files
# Usage: ./scripts/generate-structure.sh [--update]
#
# This script generates a tree view of the repository structure
# and can optionally update README.md and CLAUDE.md with it.

set -e

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

# Generate the tree structure
generate_tree() {
    cat << 'EOF'
```
palimpsest-license/
├── LICENSES/                    # License texts (v0.3, v0.4)
├── LICENSE_CORE/                # Core agreements (AGI consent, etc.)
├── GUIDES_v0.4/                 # User guides and professional integration
├── TOOLKIT_v0.4/                # Compliance audit tools
├── METADATA_v0.4/               # Machine-readable metadata schemas
├── RESEARCH/                    # Research documents (neurosymbolic, OCanren, etc.)
├── PROJECT_MANAGEMENT/          # Roadmaps, governance, campaigns
├── OUTREACH/                    # Podcast series, communications
├── docs/                        # Additional documentation
├── examples/                    # Usage vignettes and scenarios
├── assets/                      # Visual assets (badges, images)
├── embed/                       # Embeddable HTML/JS snippets
├── press-lobby-kit/             # Advocacy and press materials
├── TOOLS/                       # Validation tools (Haskell, future OCaml)
├── ARCHIVE/                     # Historical versions and experiments
├── ocaml/                       # OCaml implementation (primary)
├── standards/                   # RSR compliance, TPCF, reversibility docs
├── scripts/                     # Shell scripts (setup, hooks, generation)
├── config/                      # Configuration files (Nickel, Deno)
├── .well-known/                 # Web standards (security.txt, robots.txt, etc.)
├── .github/                     # CI/CD workflows
│
├── README.md                    # Project overview (English)
├── README.nl.md                 # Project overview (Dutch)
├── LICENSE.md                   # License pointer
├── CHANGELOG.md                 # Version history
├── CONTRIBUTING.md              # Contribution guidelines
├── CODE_OF_CONDUCT.md           # Community standards
├── GOVERNANCE.md                # Decision-making process
├── MAINTAINERS.md               # Project maintainers
├── SECURITY.md                  # Security policy
├── FUNDING.md                   # Funding strategy
├── CLAUDE.md                    # AI assistant context
│
├── flake.nix                    # Nix reproducible builds
├── guix.scm                     # Guix package definition
├── Containerfile                # Wolfi container definition
├── Justfile                     # Build automation recipes
├── package.json                 # Node.js dependencies
└── Makefile                     # Asset conversion targets
```
EOF
}

# Check if structure in file matches generated
check_sync() {
    local file="$1"
    local marker_start="<!-- STRUCTURE-START -->"
    local marker_end="<!-- STRUCTURE-END -->"

    if grep -q "$marker_start" "$file" 2>/dev/null; then
        echo "✓ $file has structure markers"
        return 0
    else
        echo "⚠ $file missing structure markers"
        echo "  Add these markers where you want the structure:"
        echo "  $marker_start"
        echo "  $marker_end"
        return 1
    fi
}

# Update structure in file
update_file() {
    local file="$1"
    local marker_start="<!-- STRUCTURE-START -->"
    local marker_end="<!-- STRUCTURE-END -->"
    local temp_file=$(mktemp)

    if ! grep -q "$marker_start" "$file" 2>/dev/null; then
        echo "⚠ Skipping $file - no structure markers found"
        return 1
    fi

    # Generate new content
    local new_structure
    new_structure=$(generate_tree)

    # Replace content between markers
    awk -v start="$marker_start" -v end="$marker_end" -v content="$new_structure" '
        $0 ~ start { print; print content; skip=1; next }
        $0 ~ end { skip=0 }
        !skip { print }
    ' "$file" > "$temp_file"

    mv "$temp_file" "$file"
    echo "✓ Updated $file"
}

# Main
case "${1:-}" in
    --update)
        echo "Updating README files with current structure..."
        generate_tree
        echo ""
        echo "To enable auto-update, add these markers to your README:"
        echo "<!-- STRUCTURE-START -->"
        echo "<!-- STRUCTURE-END -->"
        echo ""
        echo "Then run: ./scripts/generate-structure.sh --update"
        ;;
    --check)
        echo "Checking structure markers..."
        check_sync "README.md" || true
        check_sync "CLAUDE.md" || true
        ;;
    *)
        generate_tree
        ;;
esac
