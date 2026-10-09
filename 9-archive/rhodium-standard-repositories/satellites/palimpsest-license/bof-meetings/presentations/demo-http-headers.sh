#!/usr/bin/env bash
#
# HTTP Header License Signalling Demonstration
# For use at BoF sessions (IETF, RIPE, NANOG, UKNOF)
#
# This script demonstrates HTTP header-based consent signalling
# for the Palimpsest License framework.
#

set -e

# Colours for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

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

# Demo 1: Basic header inspection
demo_basic_headers() {
    section "Demo 1: License Headers in HTTP Response"

    info "Fetching headers from example site with Palimpsest license"
    echo ""

    # Simulated response (in actual demo, query real server)
    cat << 'EOF'
$ curl -I https://example.com/content.html

HTTP/2 200
date: Fri, 22 Nov 2025 10:30:00 GMT
content-type: text/html; charset=utf-8
x-license-uri: https://example.com/palimpsest-v0.4.html
x-ai-consent: non-interpretive-prohibited
x-attribution-chain: sha256:abc123def456789...
x-emotional-lineage: protest-song; cultural-heritage
content-length: 4567
EOF

    echo ""
    step "License information embedded in every HTTP response"
    step "~200 bytes overhead per request"
}

# Demo 2: Header breakdown and explanation
demo_header_breakdown() {
    section "Demo 2: Header Field Breakdown"

    step "X-License-URI: URL to full license text"
    echo "  Purpose: Direct link to human and machine-readable license"
    echo "  Example: https://example.com/palimpsest-v0.4.html"
    echo ""

    step "X-AI-Consent: Permission level for AI systems"
    echo "  Values:"
    echo "    - interpretive-only: Search, recommendations OK; generative AI prohibited"
    echo "    - non-interpretive-prohibited: No AI training without explicit agreement"
    echo "    - unrestricted: All AI uses permitted"
    echo ""

    step "X-Attribution-Chain: Cryptographic lineage tracking"
    echo "  Format: sha256:[hash]"
    echo "  Purpose: Immutable record of derivative relationships"
    echo ""

    step "X-Emotional-Lineage: Cultural and narrative context"
    echo "  Format: Semicolon-separated tags"
    echo "  Examples: protest-song; cultural-heritage; trauma-narrative"
}

# Demo 3: nginx configuration
demo_nginx_config() {
    section "Demo 3: nginx Configuration"

    info "How to add license headers in nginx:"
    echo ""

    cat << 'EOF'
# /etc/nginx/sites-available/example.com

server {
    listen 443 ssl http2;
    server_name example.com;

    # Add license headers to all responses
    add_header X-License-URI "https://example.com/palimpsest.html" always;
    add_header X-AI-Consent "non-interpretive-prohibited" always;
    add_header X-Emotional-Lineage "cultural-heritage" always;

    # Preserve attribution chain from upstream
    proxy_pass_header X-Attribution-Chain;

    location / {
        root /var/www/example.com;
        index index.html;
    }
}
EOF

    echo ""
    step "Simple configuration, applies to all content served"
    step "Restart nginx: sudo systemctl reload nginx"
}

# Demo 4: Apache configuration
demo_apache_config() {
    section "Demo 4: Apache Configuration"

    info "How to add license headers in Apache:"
    echo ""

    cat << 'EOF'
# /etc/apache2/sites-available/example.com.conf

<VirtualHost *:443>
    ServerName example.com
    DocumentRoot /var/www/example.com

    # Add license headers
    Header always set X-License-URI "https://example.com/palimpsest.html"
    Header always set X-AI-Consent "non-interpretive-prohibited"
    Header always set X-Emotional-Lineage "cultural-heritage"

    # Preserve upstream headers
    ProxyPassMatch ^/(.*) http://backend/$1

    SSLEngine on
    SSLCertificateFile /etc/ssl/certs/example.com.crt
    SSLCertificateKeyFile /etc/ssl/private/example.com.key
</VirtualHost>
EOF

    echo ""
    step "Enable headers module: sudo a2enmod headers"
    step "Reload Apache: sudo systemctl reload apache2"
}

# Demo 5: Cloudflare Worker
demo_cloudflare_worker() {
    section "Demo 5: Cloudflare Worker (Edge Computing)"

    info "Adding headers at the CDN edge:"
    echo ""

    cat << 'EOF'
// Cloudflare Worker: license-headers.js

addEventListener('fetch', event => {
  event.respondWith(handleRequest(event.request))
})

async function handleRequest(request) {
  // Fetch from origin
  const response = await fetch(request)

  // Clone response to modify headers
  const newResponse = new Response(response.body, response)

  // Add license headers
  newResponse.headers.set('X-License-URI',
    'https://example.com/palimpsest.html')
  newResponse.headers.set('X-AI-Consent',
    'non-interpretive-prohibited')
  newResponse.headers.set('X-Emotional-Lineage',
    'cultural-heritage')

  // Preserve existing attribution chain
  const chain = response.headers.get('X-Attribution-Chain')
  if (chain) {
    newResponse.headers.set('X-Attribution-Chain', chain)
  }

  return newResponse
}
EOF

    echo ""
    step "Deployed globally at Cloudflare edge locations"
    step "Minimal latency impact (<1ms)"
}

# Demo 6: Header validation in AI scraper
demo_scraper_validation() {
    section "Demo 6: AI Scraper Validation Example"

    info "How an ethical AI scraper should validate consent:"
    echo ""

    cat << 'EOF'
#!/usr/bin/env python3
# ethical_scraper.py

import requests

def check_ai_consent(url):
    """Check if URL permits AI training."""
    response = requests.head(url)

    consent = response.headers.get('X-AI-Consent', 'unrestricted')
    license_uri = response.headers.get('X-License-URI')

    if consent == 'non-interpretive-prohibited':
        print(f"❌ {url}: AI training prohibited")
        print(f"   License: {license_uri}")
        return False
    elif consent == 'interpretive-only':
        print(f"⚠️  {url}: Generative AI prohibited")
        print(f"   Search/recommendations OK")
        return False
    else:
        print(f"✓ {url}: AI training permitted")
        return True

# Example usage
urls = [
    'https://example.com/article1.html',
    'https://example.com/article2.html',
]

for url in urls:
    if check_ai_consent(url):
        # Proceed with scraping
        content = requests.get(url).text
        # ... process content
    else:
        # Skip this URL, respect creator wishes
        pass
EOF

    echo ""
    step "Ethical scrapers honour consent signals"
    step "Enforcement through platform policies and regulation"
}

# Demo 7: Performance benchmarking
demo_performance() {
    section "Demo 7: Performance Impact Measurement"

    info "Measuring overhead of license headers"
    echo ""

    step "Baseline request (no license headers):"
    cat << 'EOF'
$ curl -w "\nTime: %{time_total}s\nSize: %{size_download} bytes\n" \
    -o /dev/null -s https://example.com/page.html

Time: 0.123s
Size: 45678 bytes
EOF

    echo ""
    step "Request with license headers:"
    cat << 'EOF'
$ curl -w "\nTime: %{time_total}s\nSize: %{size_download} bytes\n" \
    -o /dev/null -s https://example.com/page.html

Time: 0.124s
Size: 45878 bytes
EOF

    echo ""
    info "Additional overhead: ~200 bytes, <1ms latency"
    info "Negligible impact on page load times"
}

# Demo 8: CDN caching behaviour
demo_cdn_caching() {
    section "Demo 8: CDN Caching and Header Propagation"

    info "Headers must propagate through CDN caching layers:"
    echo ""

    step "Cache MISS (fetched from origin):"
    cat << 'EOF'
$ curl -I https://cdn.example.com/image.jpg

HTTP/2 200
x-cache: MISS
x-license-uri: https://example.com/palimpsest.html
x-ai-consent: non-interpretive-prohibited
content-type: image/jpeg
content-length: 234567
EOF

    echo ""
    step "Cache HIT (served from CDN edge):"
    cat << 'EOF'
$ curl -I https://cdn.example.com/image.jpg

HTTP/2 200
x-cache: HIT
x-license-uri: https://example.com/palimpsest.html
x-ai-consent: non-interpretive-prohibited
content-type: image/jpeg
content-length: 234567
EOF

    echo ""
    info "License headers preserved through caching"
    info "CDN configuration: preserve custom X-* headers"
}

# Main demonstration flow
main() {
    echo -e "${BLUE}"
    cat << 'EOF'
╔═══════════════════════════════════════════════════════════════╗
║                                                               ║
║   Palimpsest License Framework                               ║
║   HTTP Header Consent Signalling Demonstration               ║
║                                                               ║
║   For BoF Sessions at IETF, RIPE, NANOG, UKNOF              ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
EOF
    echo -e "${NC}"

    demo_basic_headers
    read -p "Press Enter to continue..."

    demo_header_breakdown
    read -p "Press Enter to continue..."

    demo_nginx_config
    read -p "Press Enter to continue..."

    demo_apache_config
    read -p "Press Enter to continue..."

    demo_cloudflare_worker
    read -p "Press Enter to continue..."

    demo_scraper_validation
    read -p "Press Enter to continue..."

    demo_performance
    read -p "Press Enter to continue..."

    demo_cdn_caching

    section "Summary"
    echo "HTTP header-based consent signalling:"
    echo "  ✓ Inline with every HTTP response"
    echo "  ✓ Machine-readable and actionable"
    echo "  ✓ Minimal overhead (~200 bytes, <1ms)"
    echo "  ✓ CDN-compatible (preserved through caching)"
    echo "  ✓ Easy to deploy (nginx, Apache, edge workers)"
    echo ""
    echo "Questions?"
    echo ""
    echo -e "${BLUE}https://palimpsest-license.org${NC}"
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
