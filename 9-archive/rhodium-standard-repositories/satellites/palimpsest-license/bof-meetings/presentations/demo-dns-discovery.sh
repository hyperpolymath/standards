#!/usr/bin/env bash
#
# DNS License Discovery Demonstration
# For use at BoF sessions (IETF, RIPE, NANOG, UKNOF)
#
# This script demonstrates DNS-based license discovery for the
# Palimpsest License framework.
#

set -e

# Colours for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Presentation helpers
section() {
    echo -e "\n${BLUE}═══════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════${NC}\n"
}

step() {
    echo -e "${GREEN}▶${NC} $1"
}

info() {
    echo -e "${YELLOW}ℹ${NC} $1"
}

command_demo() {
    echo -e "${BLUE}$${NC} $1"
    sleep 1
    eval "$1"
}

# Check prerequisites
check_prereqs() {
    section "Checking Prerequisites"

    for cmd in dig host nslookup curl; do
        if command -v "$cmd" &> /dev/null; then
            step "$cmd is available"
        else
            echo -e "${RED}✗${NC} $cmd is not installed"
        fi
    done
}

# Demonstration 1: Basic DNS TXT record query
demo_basic_txt() {
    section "Demo 1: Basic DNS TXT Record Query"

    info "Querying for license information at _license.example.com"
    echo ""

    # Note: In actual demo, you'd query a real domain you've set up
    # This is a simulation for the presentation

    command_demo "dig +short TXT _license.example.com"

    echo ""
    info "Expected output:"
    echo "  \"palimpsest-v0.4 https://example.com/license.html\""
    echo "  \"ai-consent=interpretive-only\""
    echo ""

    step "License discovered automatically via DNS"
}

# Demonstration 2: DNSSEC validation
demo_dnssec() {
    section "Demo 2: DNSSEC Validation"

    info "Verifying license record authenticity with DNSSEC"
    echo ""

    command_demo "dig +dnssec TXT _license.example.com"

    echo ""
    info "RRSIG record provides cryptographic proof"
    info "Prevents spoofing of license information"
}

# Demonstration 3: Multiple query methods
demo_multiple_methods() {
    section "Demo 3: Multiple Query Methods"

    step "Method 1: Using 'dig'"
    command_demo "dig +short TXT _license.example.com"

    echo ""
    step "Method 2: Using 'host'"
    command_demo "host -t TXT _license.example.com"

    echo ""
    step "Method 3: Using 'nslookup'"
    command_demo "nslookup -type=TXT _license.example.com"
}

# Demonstration 4: Caching behaviour
demo_caching() {
    section "Demo 4: Caching and Performance"

    info "First query (uncached) - measuring latency"
    command_demo "time dig +noall +answer TXT _license.example.com"

    echo ""
    info "Second query (cached) - measuring latency"
    command_demo "time dig +noall +answer TXT _license.example.com"

    echo ""
    step "Cached queries: <1ms latency"
    step "Uncached queries: 5-50ms (depending on distance to authoritative nameserver)"
}

# Demonstration 5: Integration with HTTP workflow
demo_http_integration() {
    section "Demo 5: Integration with HTTP Workflow"

    info "Automated workflow: DNS lookup → HTTP fetch → License validation"
    echo ""

    step "Step 1: Discover license via DNS"
    LICENSE_URL=$(dig +short TXT _license.example.com | head -1 | sed 's/"//g' | cut -d' ' -f2)
    echo "  License URL: $LICENSE_URL"

    echo ""
    step "Step 2: Fetch license document"
    command_demo "curl -s -I \"$LICENSE_URL\""

    echo ""
    step "Step 3: Validate license terms"
    info "AI scraper would now check consent requirements"
}

# Demonstration 6: Zone file example
demo_zone_file() {
    section "Demo 6: DNS Zone File Configuration"

    info "How to configure license records in your zone file:"
    echo ""

    cat << 'EOF'
; License discovery records for example.com
_license.example.com.  3600  IN  TXT  "palimpsest-v0.4 https://example.com/license.html"
_license.example.com.  3600  IN  TXT  "ai-consent=interpretive-only"
_license.example.com.  3600  IN  TXT  "emotional-lineage=protest-song;cultural-heritage"

; DNSSEC signing (if enabled)
_license.example.com.  3600  IN  RRSIG  TXT 13 4 3600 (
    20251201000000 20251122000000 12345 example.com.
    [signature-data-here] )
EOF

    echo ""
    step "Simple to deploy across existing DNS infrastructure"
}

# Demonstration 7: Comparison with alternatives
demo_comparison() {
    section "Demo 7: Comparison with Alternative Approaches"

    info "Why DNS instead of..."
    echo ""

    step "robots.txt:"
    echo "  ✗ Easily ignored by scrapers"
    echo "  ✗ Not cryptographically authenticated"
    echo "  ✗ Requires HTTP request to discover"
    echo ""

    step "HTML meta tags:"
    echo "  ✗ Requires fetching entire HTML document"
    echo "  ✗ Stripped during text extraction"
    echo "  ✗ Not applicable to non-HTML content"
    echo ""

    step "DNS TXT records:"
    echo "  ✓ Lightweight (150 bytes)"
    echo "  ✓ Highly cacheable (<1ms cached latency)"
    echo "  ✓ DNSSEC authentication available"
    echo "  ✓ Universal (applies to entire domain)"
}

# Demonstration 8: Performance metrics
demo_performance() {
    section "Demo 8: Performance Impact Analysis"

    info "Measuring real-world performance impact"
    echo ""

    step "Baseline: HTTP request without DNS lookup"
    command_demo "time curl -s -o /dev/null -w '%{http_code}' https://example.com/"

    echo ""
    step "With DNS license discovery"
    command_demo "time (dig +short TXT _license.example.com > /dev/null && curl -s -o /dev/null -w '%{http_code}' https://example.com/)"

    echo ""
    info "Additional overhead: ~1-2ms (cached DNS)"
    info "Acceptable for automated scraping workflows"
}

# Main demonstration flow
main() {
    echo -e "${BLUE}"
    cat << 'EOF'
╔═══════════════════════════════════════════════════════════════╗
║                                                               ║
║   Palimpsest License Framework                               ║
║   DNS-Based License Discovery Demonstration                  ║
║                                                               ║
║   For BoF Sessions at IETF, RIPE, NANOG, UKNOF              ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
EOF
    echo -e "${NC}"

    check_prereqs

    # Run demonstrations
    demo_basic_txt
    read -p "Press Enter to continue to next demo..."

    demo_dnssec
    read -p "Press Enter to continue to next demo..."

    demo_multiple_methods
    read -p "Press Enter to continue to next demo..."

    demo_caching
    read -p "Press Enter to continue to next demo..."

    demo_http_integration
    read -p "Press Enter to continue to next demo..."

    demo_zone_file
    read -p "Press Enter to continue to next demo..."

    demo_comparison
    read -p "Press Enter to continue to next demo..."

    demo_performance

    # Summary
    section "Summary"
    echo "DNS-based license discovery provides:"
    echo "  ✓ Automated license detection"
    echo "  ✓ Low latency (<1ms cached)"
    echo "  ✓ Cryptographic authentication (DNSSEC)"
    echo "  ✓ Easy deployment (standard DNS infrastructure)"
    echo "  ✓ Universal applicability (all content types)"
    echo ""
    echo "Questions?"
    echo ""
    echo -e "${BLUE}https://palimpsest-license.org${NC}"
}

# Run main if executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
