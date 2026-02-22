#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Add arXiv badge to A2ML README after paper acceptance

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

info() { echo -e "${BLUE}ℹ️  ${NC}$1"; }
success() { echo -e "${GREEN}✓${NC} $1"; }
warn() { echo -e "${YELLOW}⚠️  ${NC}$1"; }
error() { echo -e "${RED}✗${NC} $1"; }

echo "╔══════════════════════════════════════════════════════════╗"
echo "║         Add arXiv Badge to A2ML Documentation           ║"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""

# Check for arXiv ID
if [ $# -eq 0 ]; then
    error "Usage: $0 <arxiv-id>"
    echo ""
    echo "Examples:"
    echo "  $0 2601.12345"
    echo "  $0 arXiv:2601.12345"
    echo ""
    echo "After arXiv paper is accepted, run this script with your arXiv ID."
    exit 1
fi

ARXIV_ID="$1"

# Normalize arXiv ID (remove "arXiv:" prefix if present)
ARXIV_ID_CLEAN=$(echo "$ARXIV_ID" | sed 's/^arXiv://')

info "arXiv ID: $ARXIV_ID_CLEAN"
echo ""

# URLs
ARXIV_ABS_URL="https://arxiv.org/abs/${ARXIV_ID_CLEAN}"
ARXIV_PDF_URL="https://arxiv.org/pdf/${ARXIV_ID_CLEAN}.pdf"
BADGE_URL="https://img.shields.io/badge/arXiv-${ARXIV_ID_CLEAN}-b31b1b.svg"

info "URLs:"
echo "  Abstract: $ARXIV_ABS_URL"
echo "  PDF: $ARXIV_PDF_URL"
echo "  Badge: $BADGE_URL"
echo ""

# Verify arXiv ID exists (optional, requires network)
if command -v curl >/dev/null 2>&1; then
    info "Verifying arXiv ID..."
    if curl -s --head "$ARXIV_ABS_URL" | grep "200 OK" >/dev/null; then
        success "arXiv ID verified"
    else
        warn "Could not verify arXiv ID (may not be published yet)"
        read -p "Continue anyway? (y/n): " -n 1 -r
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi
    echo ""
fi

# Update README.adoc
update_readme() {
    local readme="README.adoc"

    if [ ! -f "$readme" ]; then
        error "$readme not found"
        return 1
    fi

    info "Updating $readme..."

    # Check if arXiv badge already exists
    if grep -q "arxiv.org/abs/${ARXIV_ID_CLEAN}" "$readme"; then
        warn "arXiv badge already exists in $readme"
        return 0
    fi

    # Add badge after title, before first content
    # Insert after line 2 (assuming line 1 is SPDX, line 2 is title)

    # Create badge line
    local badge_line="image:${BADGE_URL}[arXiv, link=${ARXIV_ABS_URL}]"

    # Insert after title
    sed -i "3i\\
\\
${badge_line}" "$readme"

    success "Badge added to $readme"

    # Also add link in documents section
    if grep -q "^== Documents" "$readme"; then
        # Find line number of "== Documents"
        local line_num=$(grep -n "^== Documents" "$readme" | cut -d: -f1)
        local insert_line=$((line_num + 2))

        # Add arXiv paper link
        sed -i "${insert_line}i\\* link:${ARXIV_ABS_URL}[arXiv Paper: A2ML v1.0]" "$readme"

        success "arXiv link added to Documents section"
    fi
}

# Update SPEC.a2ml
update_spec() {
    local spec="SPEC.a2ml"

    if [ ! -f "$spec" ]; then
        warn "$spec not found (OK if using SPEC.adoc only)"
        return 0
    fi

    info "Updating $spec..."

    # Check if reference already exists
    if grep -q "arxiv.org/abs/${ARXIV_ID_CLEAN}" "$spec"; then
        warn "arXiv reference already exists in $spec"
        return 0
    fi

    # Add to @refs block at end
    if grep -q "^@refs:" "$spec"; then
        # Find last line of @refs block (line before @end)
        local refs_end=$(grep -n "^@end" "$spec" | tail -1 | cut -d: -f1)
        local insert_line=$((refs_end))

        # Insert new reference
        sed -i "${insert_line}i\\[5] A2ML Paper - arXiv:${ARXIV_ID_CLEAN} (${ARXIV_ABS_URL})" "$spec"

        success "arXiv reference added to $spec"
    fi
}

# Update IANA media type registration
update_iana() {
    local iana_doc="docs/IANA-MEDIA-TYPE.a2ml"

    if [ ! -f "$iana_doc" ]; then
        warn "$iana_doc not found"
        return 0
    fi

    info "Updating $iana_doc..."

    # Check if reference already exists
    if grep -q "arxiv.org/abs/${ARXIV_ID_CLEAN}" "$iana_doc"; then
        warn "arXiv reference already exists in $iana_doc"
        return 0
    fi

    # Add to @refs block
    if grep -q "^@refs:" "$iana_doc"; then
        local refs_end=$(grep -n "^@end" "$iana_doc" | tail -1 | cut -d: -f1)
        local insert_line=$((refs_end))

        sed -i "${insert_line}i\\[4] A2ML Paper - arXiv:${ARXIV_ID_CLEAN} (${ARXIV_ABS_URL})" "$iana_doc"

        success "arXiv reference added to $iana_doc"
    fi
}

# Update IANA registration template
update_iana_registration() {
    local iana_reg="docs/iana/application-vnd.a2ml-registration.txt"

    if [ ! -f "$iana_reg" ]; then
        warn "$iana_reg not found"
        return 0
    fi

    info "Updating $iana_reg..."

    # Replace placeholder with actual arXiv ID
    sed -i "s/arXiv:XXXX.XXXXX/arXiv:${ARXIV_ID_CLEAN}/" "$iana_reg"

    # Add arXiv reference if not present
    if ! grep -q "arXiv Paper:" "$iana_reg"; then
        # Add to References section
        sed -i "/\[4\]/a\\[5] arXiv Paper: A2ML: A Lightweight Markup Language with Formal Proof Obligations\n    arXiv:${ARXIV_ID_CLEAN} ${ARXIV_ABS_URL}" "$iana_reg"
    fi

    success "arXiv reference added to IANA registration"
}

# Main
main() {
    update_readme
    update_spec
    update_iana
    update_iana_registration

    echo ""
    success "All documentation updated!"
    echo ""

    info "Next steps:"
    echo "  1. Review changes: git diff"
    echo "  2. Commit changes: git add -A && git commit -m 'docs: add arXiv badge (arXiv:${ARXIV_ID_CLEAN})'"
    echo "  3. Push changes: git push"
    echo "  4. Update IANA submission with arXiv reference (if not yet submitted)"
    echo "  5. Announce paper publication!"
    echo ""
}

main
