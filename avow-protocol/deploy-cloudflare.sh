#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Automated Cloudflare deployment script for AVOW Protocol

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
PROJECT_NAME="avow-protocol"
DOMAIN="avow-protocol.org"
BUILD_COMMAND="deno task build"
OUTPUT_DIR="."

# Functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_prerequisites() {
    log_info "Checking prerequisites..."

    # Check for wrangler
    if ! command -v wrangler &> /dev/null; then
        log_error "wrangler CLI not found. Installing..."
        npm install -g wrangler
    fi

    # Check for deno
    if ! command -v deno &> /dev/null; then
        log_error "Deno not found. Please install Deno first."
        exit 1
    fi

    log_success "All prerequisites satisfied"
}

check_authentication() {
    log_info "Checking Cloudflare authentication..."

    if [ -n "${CLOUDFLARE_API_TOKEN:-}" ]; then
        log_success "Using CLOUDFLARE_API_TOKEN environment variable"
        return 0
    fi

    if [ -n "${CLOUDFLARE_ACCOUNT_ID:-}" ]; then
        log_success "Using CLOUDFLARE_ACCOUNT_ID environment variable"
        return 0
    fi

    log_warning "No Cloudflare credentials found in environment"
    log_info "Running wrangler login..."
    wrangler login
}

build_project() {
    log_info "Building project..."

    # Clean previous build
    if [ -d "lib" ]; then
        rm -rf lib
    fi

    # Build ReScript
    if ! $BUILD_COMMAND; then
        log_error "Build failed"
        exit 1
    fi

    log_success "Build completed"
}

create_pages_project() {
    log_info "Creating Cloudflare Pages project..."

    # Check if project exists
    if wrangler pages project list 2>/dev/null | grep -q "$PROJECT_NAME"; then
        log_warning "Project '$PROJECT_NAME' already exists, skipping creation"
    else
        if wrangler pages project create "$PROJECT_NAME" --production-branch=main; then
            log_success "Project created"
        else
            log_error "Failed to create project"
            exit 1
        fi
    fi
}

deploy_to_pages() {
    log_info "Deploying to Cloudflare Pages..."

    if wrangler pages deploy "$OUTPUT_DIR" \
        --project-name="$PROJECT_NAME" \
        --branch=main \
        --commit-dirty=true; then
        log_success "Deployment successful!"
    else
        log_error "Deployment failed"
        exit 1
    fi
}

configure_custom_domain() {
    log_info "Configuring custom domain..."

    log_warning "Custom domain setup requires manual configuration in Cloudflare Dashboard:"
    echo "  1. Go to Pages → $PROJECT_NAME → Custom domains"
    echo "  2. Add domain: $DOMAIN"
    echo "  3. Add domain: www.$DOMAIN"
    echo "  4. Wait for DNS propagation"
}

configure_dns() {
    log_info "DNS configuration steps:"

    cat <<EOF

${YELLOW}Manual DNS Configuration Required:${NC}

1. Import DNS zone from: cloudflare-dns-zone.txt
   - Go to Cloudflare Dashboard → DNS → Import
   - Or use Cloudflare API

2. Update placeholder values in DNS records:
   - Replace 192.0.2.1 with your origin IP
   - Replace 2001:db8::1 with your IPv6
   - Update tunnel-id with your Cloudflare Tunnel ID
   - Update SSHFP fingerprints with your SSH keys
   - Update TLSA records with certificate hashes

3. Enable DNSSEC:
   - Dashboard → DNS → Settings → DNSSEC

4. Configure CAA records for Let's Encrypt and DigiCert

EOF
}

configure_security_headers() {
    log_info "Security headers configuration:"

    cat <<EOF

${YELLOW}Security Headers Setup:${NC}

Add Transform Rules in Cloudflare Dashboard → Rules → Transform Rules:

Name: AVOW Security Headers
When incoming requests match: All incoming requests
Then set static HTTP response headers:

- Strict-Transport-Security: max-age=63072000; includeSubDomains; preload
- X-Content-Type-Options: nosniff
- X-Frame-Options: DENY
- X-XSS-Protection: 1; mode=block
- Content-Security-Policy: default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'
- Referrer-Policy: strict-origin-when-cross-origin
- Permissions-Policy: geolocation=(), microphone=(), camera=()

EOF
}

show_deployment_url() {
    log_success "Deployment complete!"

    echo ""
    echo "======================================"
    echo "  AVOW Protocol Deployment Summary"
    echo "======================================"
    echo ""
    echo "Production URL: https://$PROJECT_NAME.pages.dev"
    echo "Custom Domain:  https://$DOMAIN (after DNS setup)"
    echo ""
    echo "Next steps:"
    echo "  1. Configure custom domain (see above)"
    echo "  2. Import DNS zone file (see cloudflare-dns-zone.txt)"
    echo "  3. Set up security headers (Transform Rules)"
    echo "  4. Configure Zero Trust/SDP (see CLOUDFLARE-SETUP.md)"
    echo "  5. Test deployment: curl -I https://$PROJECT_NAME.pages.dev"
    echo ""
    echo "Complete setup guide: CLOUDFLARE-SETUP.md"
    echo ""
}

main() {
    echo ""
    log_info "Starting AVOW Protocol Cloudflare deployment..."
    echo ""

    check_prerequisites
    check_authentication
    build_project
    create_pages_project
    deploy_to_pages

    echo ""
    configure_custom_domain
    configure_dns
    configure_security_headers
    show_deployment_url
}

# Run main function
main "$@"
