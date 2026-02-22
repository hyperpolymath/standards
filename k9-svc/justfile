# SPDX-License-Identifier: AGPL-3.0-or-later
# justfile - K9 SVC Orchestration (The Muscle)
#
# Part of the must-just-nickel triad.
# Usage: ./must run <recipe> OR just <recipe>

set shell := ["sh", "-eu", "-c"]
set dotenv-load

# Default recipe: show status
default:
    @./must status

# ─────────────────────────────────────────────────────────────
# Environment & Setup
# ─────────────────────────────────────────────────────────────

# Ensure the triad is installed
ensure:
    @./must ensure

# Show K9 environment report
status:
    @./must status

# Check all dependencies are available
check-deps:
    @echo "K9: Checking dependencies..."
    @command -v nickel >/dev/null 2>&1 && echo "  ✓ Nickel: $(nickel --version 2>/dev/null || echo 'installed')" || echo "  ✗ Nickel: NOT FOUND"
    @command -v just >/dev/null 2>&1 && echo "  ✓ Just: $(just --version)" || echo "  ✗ Just: NOT FOUND"
    @command -v podman >/dev/null 2>&1 && echo "  ✓ Podman: $(podman --version)" || echo "  ○ Podman: not installed (optional)"
    @command -v file >/dev/null 2>&1 && echo "  ✓ file: available" || echo "  ○ file: not installed"
    @echo "K9: Dependency check complete."

# Sync .machine_readable metadata into 6scm mirrors
sync-6scm:
    @mkdir -p .machine_readable/6scm
    @for f in AGENTIC.scm ECOSYSTEM.scm META.scm NEUROSYM.scm PLAYBOOK.scm STATE.scm; do \
      if [ -f ".machine_readable/$$f" ]; then cp -f ".machine_readable/$$f" ".machine_readable/6scm/$$f"; fi; \
    done

# Check .machine_readable 6scm mirrors are in sync
check-6scm:
    @scripts/check-6scm.sh

# ─────────────────────────────────────────────────────────────
# MIME Registration
# ─────────────────────────────────────────────────────────────

# Register .k9 MIME type (user-level, Linux)
install-mime:
    @echo "K9: Installing MIME type for current user..."
    @mkdir -p ~/.local/share/mime/packages
    @cp mime/k9.xml ~/.local/share/mime/packages/
    @update-mime-database ~/.local/share/mime 2>/dev/null || true
    @echo "K9: User MIME registration complete."

# Register .k9 MIME type (system-wide, requires sudo)
install-mime-system:
    @echo "K9: Installing MIME type system-wide..."
    @sudo cp mime/k9.xml /usr/share/mime/packages/
    @sudo update-mime-database /usr/share/mime
    @echo "K9: System MIME registration complete."

# Install magic file for file(1) detection
install-magic:
    @echo "K9: Installing magic file..."
    @mkdir -p ~/.local/share/file
    @cp mime/k9.magic ~/.local/share/file/
    @cat mime/k9.magic >> ~/.magic 2>/dev/null || cp mime/k9.magic ~/.magic
    @file --compile ~/.magic 2>/dev/null || true
    @echo "K9: Magic file installed. Test with: file examples/hello.k9"

# Install macOS UTI (macOS only)
install-uti:
    @echo "K9: Installing macOS UTI..."
    @mkdir -p ~/Library/Application\ Support/K9
    @cp mime/k9.uti.plist ~/Library/Application\ Support/K9/
    @echo "K9: UTI installed. May require logout/login to take effect."

# Verify MIME registration
verify-mime:
    @echo "K9: Verifying MIME registration..."
    @file --mime-type examples/hello.k9 2>/dev/null || echo "  Note: Install magic file with 'just install-magic'"
    @xdg-mime query filetype examples/hello.k9 2>/dev/null || echo "  Note: MIME database may need update"

# Full MIME setup (user-level)
setup-mime: install-mime install-magic
    @echo "K9: Full MIME setup complete."

# ─────────────────────────────────────────────────────────────
# Validation (The Brain)
# ─────────────────────────────────────────────────────────────

# Typecheck all Nickel schemas
typecheck:
    @echo "K9: Typechecking all schemas..."
    @nickel typecheck pedigree.ncl
    @nickel typecheck register.ncl
    @nickel typecheck leash.ncl
    @echo "K9: All schemas valid."

# Validate the pedigree schema
validate-pedigree:
    @echo "K9: Validating pedigree.ncl..."
    @nickel typecheck pedigree.ncl
    @echo "K9: Pedigree schema valid."

# Validate a .k9 component against the pedigree
validate file:
    @echo "K9: Validating {{file}} against pedigree..."
    @nickel typecheck {{file}}
    @echo "K9: {{file}} passes validation."

# Full validation suite
validate-all: typecheck
    @echo "K9: Validating example components..."
    @nickel typecheck examples/config.k9.ncl || true
    @nickel typecheck examples/deploy.k9.ncl || true
    @echo "K9: Validation suite complete."

# ─────────────────────────────────────────────────────────────
# Security (The Leash)
# ─────────────────────────────────────────────────────────────

# Check security level of a component
leash-level file:
    @echo "K9: Checking security level for {{file}}..."
    @if grep -q "trust_level.*'Hunt" {{file}} 2>/dev/null; then \
        echo "  Level: 'Hunt (FULL EXECUTION - requires authorization)"; \
    elif grep -q "trust_level.*'Yard" {{file}} 2>/dev/null; then \
        echo "  Level: 'Yard (Nickel evaluation only)"; \
    else \
        echo "  Level: 'Kennel (pure data, safe)"; \
    fi

# List security levels of all examples
leash-list:
    @echo "K9: Security levels of example components:"
    @echo ""
    @for f in examples/*.k9 examples/*.k9.ncl; do \
        if [ -f "$$f" ]; then \
            printf "  %-30s " "$$f:"; \
            if grep -q "trust_level.*'Hunt" "$$f" 2>/dev/null; then \
                echo "'Hunt ⚠️"; \
            elif grep -q "trust_level.*'Yard" "$$f" 2>/dev/null; then \
                echo "'Yard"; \
            else \
                echo "'Kennel ✓"; \
            fi; \
        fi; \
    done

# Authorize a Hunt-level component (placeholder - would involve signing)
authorize file:
    @echo "K9: Authorization requested for {{file}}..."
    @echo "  ⚠️  WARNING: Hunt-level execution can modify your system."
    @echo ""
    @echo "  In production, this would:"
    @echo "  1. Verify the component's cryptographic signature"
    @echo "  2. Check against trusted public keys"
    @echo "  3. Record authorization in audit log"
    @echo ""
    @echo "  For now, this is a placeholder. Review the file manually."

# ─────────────────────────────────────────────────────────────
# Build & Deploy
# ─────────────────────────────────────────────────────────────

# Build documentation
docs:
    @echo "K9: Generating documentation..."
    @mkdir -p docs
    @if command -v asciidoctor >/dev/null 2>&1; then \
        asciidoctor -o docs/README.html README.adoc 2>/dev/null || true; \
        asciidoctor -o docs/SPEC.html SPEC.adoc 2>/dev/null || true; \
        echo "K9: HTML docs generated in docs/"; \
    else \
        echo "K9: asciidoctor not found, skipping HTML generation"; \
    fi

# Build container image
build-container:
    @echo "K9: Building container image..."
    @podman build -t k9-svc:latest .
    @echo "K9: Container built as k9-svc:latest"

# Run container
run-container *args:
    @podman run --rm -it k9-svc:latest {{args}}

# Deploy via Podman (if available)
deploy-podman: build-container
    @echo "K9: Deploying via Podman..."
    @podman run --rm k9-svc:latest dogfood
    @echo "K9: Podman deployment complete."

# Deploy natively (no container)
deploy-native:
    @echo "K9: Native deployment..."
    @./must ensure
    @just validate-all
    @just setup-mime
    @echo "K9: Native deployment complete."

# Auto-select deployment method
deploy:
    @if command -v podman >/dev/null 2>&1; then \
        just deploy-podman; \
    else \
        just deploy-native; \
    fi

# ─────────────────────────────────────────────────────────────
# Development
# ─────────────────────────────────────────────────────────────

# Format Nickel files
fmt:
    @echo "K9: Formatting Nickel files..."
    @nickel format pedigree.ncl 2>/dev/null || true
    @nickel format register.ncl 2>/dev/null || true
    @nickel format leash.ncl 2>/dev/null || true
    @echo "K9: Formatting complete."

# Interactive Nickel REPL with pedigree loaded
repl:
    @nickel repl --import pedigree.ncl

# Export pedigree schema as JSON (for tooling)
export-schema:
    @nickel export pedigree.ncl --format json

# Export leash levels as JSON
export-leash:
    @nickel export leash.ncl --format json

# Show example config as JSON
show-config:
    @nickel export examples/config.k9.ncl --field config --format json

# ─────────────────────────────────────────────────────────────
# Dogfooding
# ─────────────────────────────────────────────────────────────

# The ultimate test: use K9 to validate K9
dogfood:
    @echo "K9: Dogfooding - validating ourselves..."
    @./must ensure
    @just typecheck
    @just leash-list
    @just verify-mime || true
    @echo ""
    @echo "K9: Dogfooding complete. We eat what we cook. 🐕"

# ─────────────────────────────────────────────────────────────
# Clean
# ─────────────────────────────────────────────────────────────

# Clean generated files
clean:
    @echo "K9: Cleaning generated files..."
    @rm -rf docs/*.html
    @rm -f ~/.magic.mgc
    @echo "K9: Clean complete."

# Uninstall MIME registration
uninstall-mime:
    @echo "K9: Removing MIME registration..."
    @rm -f ~/.local/share/mime/packages/k9.xml
    @update-mime-database ~/.local/share/mime 2>/dev/null || true
    @rm -f ~/.magic
    @echo "K9: MIME registration removed."

# ─────────────────────────────────────────────────────────────
# Signing (Hunt Authorization)
# ─────────────────────────────────────────────────────────────

# Generate a new signing keypair
keygen name="primary":
    @./sign.sh keygen {{name}}

# Sign a component file
sign file name="primary":
    @./sign.sh sign {{file}} {{name}}

# Verify a component's signature
verify file:
    @./sign.sh verify {{file}}

# Full Hunt authorization check
authorize file:
    @./sign.sh authorize {{file}}

# Add a public key to trusted keys
trust pubkey:
    @./sign.sh trust {{pubkey}}

# List all keys
list-keys:
    @./sign.sh list

# ─────────────────────────────────────────────────────────────
# Testing
# ─────────────────────────────────────────────────────────────

# Run the test suite
test:
    @./test.sh

# Quick validation test
test-quick:
    @echo "K9: Quick test..."
    @nickel typecheck pedigree.ncl
    @nickel typecheck leash.ncl
    @echo "K9: Quick test passed."
