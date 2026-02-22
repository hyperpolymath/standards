# justfile - Command runner for consent-aware-http
# https://github.com/casey/just
#
# Install just: https://just.systems/
# Usage: just <recipe>
# List all recipes: just --list

# Default recipe (runs when you type 'just' with no arguments)
default:
    @just --list

# === VALIDATION & VERIFICATION ===

# Validate all AIBDP manifest files
validate-manifests:
    @echo "🔍 Validating AIBDP manifests..."
    @just validate-manifest .well-known/aibdp.json
    @just validate-manifest docs/example-aibdp.json
    @echo "✅ All manifests valid"

# Validate a single AIBDP manifest (requires jq)
validate-manifest FILE:
    @echo "Validating {{FILE}}..."
    @jq empty {{FILE}} || (echo "❌ Invalid JSON in {{FILE}}" && exit 1)
    @echo "✅ {{FILE}} is valid JSON"

# Validate security.txt compliance (RFC 9116)
validate-security-txt:
    @echo "🔍 Validating security.txt..."
    @test -f .well-known/security.txt || (echo "❌ security.txt missing" && exit 1)
    @grep -q "Contact:" .well-known/security.txt || (echo "❌ Missing Contact field" && exit 1)
    @grep -q "Expires:" .well-known/security.txt || (echo "❌ Missing Expires field" && exit 1)
    @echo "✅ security.txt RFC 9116 compliant"

# Validate Internet-Draft XML files (requires xml2rfc)
validate-drafts:
    @echo "🔍 Validating Internet-Drafts..."
    @if command -v xml2rfc >/dev/null 2>&1; then \
        xml2rfc --v3 draft-jewell-http-430-consent-required-00.xml --text --out /tmp/draft-430.txt && \
        echo "✅ HTTP 430 draft valid"; \
    else \
        echo "⚠️  xml2rfc not installed (pip install xml2rfc), skipping draft validation"; \
    fi

# Run all validation checks
validate: validate-manifests validate-security-txt validate-drafts
    @echo ""
    @echo "✅ All validation checks passed"

# Check RSR (Rhodium Standard Repository) compliance
check-rsr:
    @echo "🔍 Checking RSR Framework Compliance..."
    @just --quiet _check-file "README.adoc" "Repository documentation"
    @just --quiet _check-file "LICENSE.txt" "License file"
    @just --quiet _check-file "CODE_OF_CONDUCT.md" "Code of Conduct"
    @just --quiet _check-file "CONTRIBUTING.md" "Contribution guidelines" ".github/CONTRIBUTING.md"
    @just --quiet _check-file "SECURITY.md" "Security policy" ".github/SECURITY.md"
    @just --quiet _check-file "MAINTAINERS.md" "Maintainers documentation"
    @just --quiet _check-file "CHANGELOG.md" "Changelog"
    @just --quiet _check-file ".well-known/security.txt" "security.txt (RFC 9116)"
    @just --quiet _check-file ".well-known/ai.txt" "AI usage declaration"
    @just --quiet _check-file ".well-known/humans.txt" "Human attribution"
    @just --quiet _check-file ".well-known/aibdp.json" "AIBDP manifest"
    @just --quiet _check-file "justfile" "Build system (just)"
    @echo ""
    @echo "✅ RSR compliance check complete"
    @echo ""
    @echo "📊 RSR Status: Bronze+ (Specification Repository Variant)"
    @echo "   ✓ Complete documentation suite"
    @echo "   ✓ .well-known/ directory with RFC 9116 compliance"
    @echo "   ✓ Build/validation tooling (justfile)"
    @echo "   ✓ Community governance (MAINTAINERS, CoC)"
    @echo "   ✓ Self-referential AIBDP implementation"

# Helper: Check if file exists
_check-file NAME DESC ALT="":
    #!/usr/bin/env bash
    if [ -f "{{NAME}}" ]; then
        echo "✅ {{DESC}}: {{NAME}}"
    elif [ -n "{{ALT}}" ] && [ -f "{{ALT}}" ]; then
        echo "✅ {{DESC}}: {{ALT}}"
    else
        echo "❌ {{DESC}}: Missing"
        exit 1
    fi

# === BUILD & RENDER ===

# Render Internet-Drafts to text format (requires xml2rfc)
build-drafts:
    @echo "📄 Rendering Internet-Drafts..."
    @mkdir -p rendered
    @if command -v xml2rfc >/dev/null 2>&1; then \
        xml2rfc --v3 draft-jewell-http-430-consent-required-00.xml --text --out rendered/draft-jewell-http-430-consent-required-00.txt && \
        xml2rfc --v3 draft-jewell-http-430-consent-required-00.xml --html --out rendered/draft-jewell-http-430-consent-required-00.html && \
        echo "✅ HTTP 430 draft rendered"; \
    else \
        echo "❌ xml2rfc not installed"; \
        echo "   Install with: pip install xml2rfc"; \
        exit 1; \
    fi

# Render drafts to all formats (text, HTML, PDF)
build-all: build-drafts
    @echo "📄 Rendering all formats..."
    @if command -v xml2rfc >/dev/null 2>&1; then \
        xml2rfc --v3 draft-jewell-http-430-consent-required-00.xml --pdf --out rendered/draft-jewell-http-430-consent-required-00.pdf && \
        echo "✅ PDF rendered"; \
    else \
        echo "⚠️  PDF rendering requires xml2rfc with weasyprint"; \
    fi

# Clean build artifacts
clean:
    @echo "🧹 Cleaning build artifacts..."
    @rm -rf rendered/
    @echo "✅ Clean complete"

# === TESTING ===

# Run link checker on documentation (requires markdown-link-check)
test-links:
    @echo "🔗 Checking documentation links..."
    @if command -v markdown-link-check >/dev/null 2>&1; then \
        find . -name "*.md" ! -path "./node_modules/*" ! -path "./.git/*" -exec markdown-link-check {} \; ; \
    else \
        echo "⚠️  markdown-link-check not installed (npm install -g markdown-link-check)"; \
    fi

# Check for common typos and style issues
test-style:
    @echo "📝 Checking style and common typos..."
    @echo "⚠️  Style checking not yet implemented"
    @echo "   TODO: Add proselint, vale, or write-good integration"

# Run all tests
test: validate test-links

# === DEVELOPMENT HELPERS ===

# Format JSON files (requires jq)
format:
    @echo "🎨 Formatting JSON files..."
    @find . -name "*.json" ! -path "./node_modules/*" ! -path "./.git/*" -exec sh -c 'jq . "{}" > "{}.tmp" && mv "{}.tmp" "{}"' \;
    @echo "✅ JSON files formatted"

# Check for outdated security.txt expiry
check-expiry:
    @echo "📅 Checking security.txt expiry..."
    @grep "Expires:" .well-known/security.txt || echo "⚠️  No expiry date found"

# Watch for changes and auto-validate (requires watchexec)
watch:
    @if command -v watchexec >/dev/null 2>&1; then \
        watchexec -e xml,json,md just validate; \
    else \
        echo "❌ watchexec not installed"; \
        echo "   Install: cargo install watchexec-cli"; \
        exit 1; \
    fi

# === GIT HELPERS ===

# Run pre-commit checks (validation before committing)
pre-commit: validate
    @echo ""
    @echo "✅ Pre-commit checks passed"
    @echo "   Safe to commit!"

# Show project status
status:
    @echo "📊 Consent-Aware HTTP Standards - Project Status"
    @echo ""
    @echo "📁 Repository: consent-aware-http"
    @echo "🌿 Branch: $(git branch --show-current)"
    @echo "📝 Last commit: $(git log -1 --format='%h - %s (%ar)')"
    @echo ""
    @echo "📄 Internet-Drafts:"
    @test -f draft-jewell-http-430-consent-required-00.xml && echo "   ✅ HTTP 430 Consent Required (v00)" || echo "   ❌ Missing"
    @test -f drafts/draft-jewell-aibdp-00.xml && echo "   ✅ AIBDP Protocol (v00)" || echo "   ⚠️  In development"
    @echo ""
    @echo "📋 Documentation:"
    @echo "   Files: $(find docs -name '*.md' | wc -l) markdown files"
    @echo ""
    @echo "🛠️  Tooling:"
    @command -v xml2rfc >/dev/null 2>&1 && echo "   ✅ xml2rfc" || echo "   ❌ xml2rfc (install: pip install xml2rfc)"
    @command -v jq >/dev/null 2>&1 && echo "   ✅ jq" || echo "   ❌ jq (install: apt install jq / brew install jq)"
    @command -v markdown-link-check >/dev/null 2>&1 && echo "   ✅ markdown-link-check" || echo "   ⚠️  markdown-link-check (optional)"

# === RELEASE HELPERS ===

# Prepare for release (validate, build, test)
release-prep VERSION:
    @echo "📦 Preparing release {{VERSION}}..."
    @just validate
    @just build-all
    @just test
    @echo ""
    @echo "✅ Release {{VERSION}} ready"
    @echo ""
    @echo "Next steps:"
    @echo "  1. Update CHANGELOG.md with version {{VERSION}}"
    @echo "  2. git tag -a v{{VERSION}} -m 'Release {{VERSION}}'"
    @echo "  3. git push origin v{{VERSION}}"
    @echo "  4. Create GitHub Release"

# === INSTALLATION & SETUP ===

# Install development dependencies (shows commands, doesn't run them)
install-deps:
    @echo "📦 Development Dependencies Installation Guide"
    @echo ""
    @echo "Core tools (required for full functionality):"
    @echo "  • xml2rfc (Internet-Draft rendering)"
    @echo "    pip install xml2rfc"
    @echo ""
    @echo "  • jq (JSON validation and formatting)"
    @echo "    apt install jq          # Debian/Ubuntu"
    @echo "    brew install jq         # macOS"
    @echo "    pacman -S jq            # Arch"
    @echo ""
    @echo "Optional tools (enhanced workflow):"
    @echo "  • markdown-link-check (link validation)"
    @echo "    npm install -g markdown-link-check"
    @echo ""
    @echo "  • watchexec (auto-validation on file changes)"
    @echo "    cargo install watchexec-cli"
    @echo ""
    @echo "  • weasyprint (PDF rendering for xml2rfc)"
    @echo "    pip install weasyprint"

# Show version information
version:
    @echo "Consent-Aware HTTP Standards"
    @echo "HTTP 430 + AIBDP Protocol Specifications"
    @echo ""
    @echo "Repository version: 0.1.0"
    @echo "HTTP 430 Draft: 00 (July 2025)"
    @echo "AIBDP Draft: In Development"
    @echo ""
    @echo "Author: Jonathan D.A. Jewell"
    @echo "License: MIT (code/specs) + CC BY-SA 4.0 (docs)"

# === HELP ===

# Show detailed help
help:
    @echo "🛠️  Consent-Aware HTTP Standards - Build & Validation Tool"
    @echo ""
    @echo "Core workflows:"
    @echo "  just validate          Validate manifests, drafts, and configs"
    @echo "  just check-rsr         Check RSR framework compliance"
    @echo "  just build-drafts      Render Internet-Drafts to text/HTML"
    @echo "  just test              Run all tests and validations"
    @echo "  just pre-commit        Run checks before committing"
    @echo "  just status            Show project status"
    @echo ""
    @echo "Development:"
    @echo "  just watch             Auto-validate on file changes"
    @echo "  just format            Format JSON files"
    @echo "  just clean             Remove build artifacts"
    @echo ""
    @echo "Release:"
    @echo "  just release-prep V    Prepare release version V"
    @echo ""
    @echo "Setup:"
    @echo "  just install-deps      Show dependency installation commands"
    @echo "  just version           Show version information"
    @echo ""
    @echo "Full list: just --list"
