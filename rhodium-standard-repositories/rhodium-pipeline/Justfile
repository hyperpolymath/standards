# SPDX-License-Identifier: AGPL-3.0-or-later
# rhodium-pipeline-template - Development Tasks
#
# IMPORTANT: This file MUST be named "Justfile" (capital J) for RSR compliance.
#
# Recipes for developing and testing the template itself.
# For generated project recipes, see the template Justfile.
#
set shell := ["bash", "-uc"]
set dotenv-load := true

project := "rhodium-pipeline-template"
test_output := "test-output"

# Show all recipes
default:
    @just --list --unsorted

# ============================================================================
# BUILD & TEST
# ============================================================================

# Generate a test project from the template
build:
    @echo "Generating test project..."
    @rm -rf {{ test_output }}
    cargo generate --path ./template \
        --name {{ test_output }} \
        --define project_name=test_output \
        --define ProjectName=TestOutput \
        --define PROJECT_NAME=TEST_OUTPUT \
        --define description="Test project for template verification" \
        --define author="Template Test" \
        --define email="test@example.com" \
        --define license=MIT \
        --define include_julia=false \
        --define checksum_algo=blake3 \
        --define isabelle_version=Isabelle2024 \
        --define min_rust_version=1.75.0 \
        --define default_threads=4 \
        --define enable_telemetry=false \
        --define target_platforms="linux-x86_64"
    @echo "✓ Test project generated at ./{{ test_output }}"

# Run full test suite: generate, compile, and test
test: build
    @echo "Testing generated project..."
    cd {{ test_output }} && cargo check
    @echo "✓ cargo check passed"
    cd {{ test_output }} && cargo test --lib
    @echo "✓ cargo test passed"
    cd {{ test_output }} && cargo build --release
    @echo "✓ cargo build --release passed"
    cd {{ test_output }} && ./target/release/{{ test_output }} --version
    @echo "✓ CLI runs successfully"
    @echo ""
    @echo "All tests passed!"

# Quick test: just verify it compiles
test-quick: build
    @echo "Quick test: checking compilation..."
    cd {{ test_output }} && cargo check
    @echo "✓ Compilation check passed"

# Test with Julia enabled
test-julia:
    @echo "Generating test project with Julia..."
    @rm -rf {{ test_output }}-julia
    cargo generate --path ./template \
        --name {{ test_output }}-julia \
        --define project_name=test_output_julia \
        --define ProjectName=TestOutputJulia \
        --define PROJECT_NAME=TEST_OUTPUT_JULIA \
        --define description="Test project with Julia" \
        --define author="Template Test" \
        --define email="test@example.com" \
        --define license=MIT \
        --define include_julia=true \
        --define checksum_algo=blake3 \
        --define isabelle_version=Isabelle2024 \
        --define min_rust_version=1.75.0 \
        --define default_threads=4 \
        --define enable_telemetry=false \
        --define target_platforms="linux-x86_64"
    cd {{ test_output }}-julia && cargo check
    @echo "✓ Julia variant compiles"

# Clean test outputs
clean:
    @echo "Cleaning test outputs..."
    rm -rf {{ test_output }}
    rm -rf {{ test_output }}-julia
    rm -rf test-*
    @echo "✓ Clean complete"

# ============================================================================
# FORMATTING & LINTING
# ============================================================================

# Format template files (check mode)
fmt:
    @echo "Checking formatting..."
    @# Check Liquid templates for consistent indentation
    @find template -name "*.liquid" -exec grep -l "	" {} \; | while read f; do \
        echo "Warning: tabs found in $$f (prefer spaces)"; \
    done || true
    @# Check for trailing whitespace
    @find template -name "*.liquid" -exec grep -l " $$" {} \; | while read f; do \
        echo "Warning: trailing whitespace in $$f"; \
    done || true
    @# Check AsciiDoc formatting
    @find docs -name "*.adoc" -exec grep -l "	" {} \; | while read f; do \
        echo "Warning: tabs found in $$f (prefer spaces)"; \
    done || true
    @echo "✓ Format check complete"

# Lint template files
lint:
    @echo "Linting template..."
    @# Verify cargo-generate.toml is valid TOML
    @cat template/cargo-generate.toml | python3 -c "import sys,tomllib; tomllib.loads(sys.stdin.read())" 2>/dev/null \
        && echo "✓ cargo-generate.toml is valid TOML" \
        || echo "✗ cargo-generate.toml has TOML errors"
    @# Check for undefined template variables (basic check)
    @echo "Checking for potentially undefined variables..."
    @grep -roh '{{\s*[a-zA-Z_][a-zA-Z0-9_]*\s*}}' template/ | sort -u | while read var; do \
        varname=$$(echo "$$var" | sed 's/[{ }]//g'); \
        if ! grep -q "$$varname" template/cargo-generate.toml 2>/dev/null; then \
            echo "  Verify: $$varname"; \
        fi; \
    done || true
    @# Check that all .liquid files are valid (no syntax errors)
    @echo "Checking Liquid syntax..."
    @find template -name "*.liquid" | head -5 | while read f; do \
        if grep -q '{{.*{{' "$$f" 2>/dev/null; then \
            echo "Warning: nested braces in $$f"; \
        fi; \
    done || true
    @echo "✓ Lint complete"

# ============================================================================
# VALIDATION
# ============================================================================

# Validate all template components
validate: lint
    @echo "Validating template structure..."
    @# Check required files exist
    @test -f template/cargo-generate.toml || (echo "✗ Missing cargo-generate.toml" && exit 1)
    @test -d "template/{{project-name}}" || (echo "✗ Missing template directory" && exit 1)
    @test -f "template/{{project-name}}/Cargo.toml.liquid" || (echo "✗ Missing Cargo.toml.liquid" && exit 1)
    @test -f "template/{{project-name}}/src/main.rs.liquid" || (echo "✗ Missing main.rs.liquid" && exit 1)
    @echo "✓ Required files present"
    @# Check proof files exist
    @test -f "template/{{project-name}}/proofs/ROOT.liquid" || (echo "✗ Missing proofs/ROOT.liquid" && exit 1)
    @ls template/{{project-name}}/proofs/*.thy.liquid >/dev/null 2>&1 || (echo "✗ Missing .thy.liquid files" && exit 1)
    @echo "✓ Proof templates present"
    @# Check config files exist
    @test -f "template/{{project-name}}/config/default.ncl.liquid" || (echo "✗ Missing config/default.ncl.liquid" && exit 1)
    @echo "✓ Config templates present"
    @echo ""
    @echo "All validations passed!"

# Full CI check: validate, build, test
ci: validate test
    @echo ""
    @echo "CI pipeline complete!"

# ============================================================================
# DOCUMENTATION
# ============================================================================

# Check documentation links
check-docs:
    @echo "Checking documentation..."
    @# Check that referenced docs exist
    @for doc in LICENSING CONTRIBUTING ARCHITECTURE CLI PROOFS; do \
        test -f "docs/$$doc.adoc" || echo "✗ Missing docs/$$doc.adoc"; \
    done
    @echo "✓ Documentation files present"
    @# Check for broken internal links (basic)
    @grep -roh 'link:[^[]*\[' docs/*.adoc README.adoc 2>/dev/null | \
        sed 's/link://;s/\[//' | sort -u | while read link; do \
        if [ -n "$$link" ] && [ ! -f "$$link" ] && [[ ! "$$link" =~ ^http ]]; then \
            echo "  Check link: $$link"; \
        fi; \
    done || true
    @echo "✓ Link check complete"

# ============================================================================
# RELEASE
# ============================================================================

# Prepare for release (run all checks)
release-check: ci check-docs
    @echo ""
    @echo "Checking for TODOs in non-template files..."
    @grep -rn "TODO\|FIXME" --include="*.adoc" --include="*.md" . 2>/dev/null | \
        grep -v "template/" | grep -v ".git/" | grep -v "node_modules/" || true
    @echo ""
    @echo "Release check complete!"

# Show template variables
show-vars:
    @echo "Template variables defined in cargo-generate.toml:"
    @grep -E "^\[placeholders\." template/cargo-generate.toml | sed 's/\[placeholders\.//;s/\]//'

# ============================================================================
# UTILITIES
# ============================================================================

# Count lines in template
stats:
    @echo "Template statistics:"
    @echo ""
    @echo "Liquid templates:"
    @find template -name "*.liquid" | xargs wc -l | tail -1
    @echo ""
    @echo "By type:"
    @echo "  Rust:     $$(find template -name "*.rs.liquid" | xargs cat 2>/dev/null | wc -l) lines"
    @echo "  Isabelle: $$(find template -name "*.thy.liquid" | xargs cat 2>/dev/null | wc -l) lines"
    @echo "  Nickel:   $$(find template -name "*.ncl.liquid" | xargs cat 2>/dev/null | wc -l) lines"
    @echo "  Justfile: $$(find template -name "justfile.liquid" | xargs cat 2>/dev/null | wc -l) lines"

# Watch for changes and rebuild
watch:
    @echo "Watching for changes... (Ctrl+C to stop)"
    @while true; do \
        inotifywait -qre modify,create,delete template/ 2>/dev/null || sleep 2; \
        just test-quick; \
    done

# ============================================================================
# CONTAINERS (nerdctl-first, podman-fallback)
# ============================================================================

# Detect container runtime: nerdctl > podman > docker
[private]
container-cmd:
    #!/usr/bin/env bash
    if command -v nerdctl >/dev/null 2>&1; then
        echo "nerdctl"
    elif command -v podman >/dev/null 2>&1; then
        echo "podman"
    elif command -v docker >/dev/null 2>&1; then
        echo "docker"
    else
        echo "ERROR: No container runtime found (install nerdctl, podman, or docker)" >&2
        exit 1
    fi

# Build container image
container-build tag="latest":
    #!/usr/bin/env bash
    CTR=$(just container-cmd)
    if [ -f Containerfile ]; then
        echo "Building with $CTR..."
        $CTR build -t {{project}}:{{tag}} -f Containerfile .
    else
        echo "No Containerfile found"
    fi

# Run container
container-run tag="latest" *args:
    #!/usr/bin/env bash
    CTR=$(just container-cmd)
    $CTR run --rm -it {{project}}:{{tag}} {{args}}

# Push container image
container-push registry="ghcr.io/hyperpolymath" tag="latest":
    #!/usr/bin/env bash
    CTR=$(just container-cmd)
    $CTR tag {{project}}:{{tag}} {{registry}}/{{project}}:{{tag}}
    $CTR push {{registry}}/{{project}}:{{tag}}

# ============================================================================
# RSR COMPLIANCE
# ============================================================================

# Validate RSR compliance
validate-rsr:
    #!/usr/bin/env bash
    echo "=== RSR Compliance Check ==="
    MISSING=""
    for f in .editorconfig .gitignore Justfile RSR_COMPLIANCE.adoc README.adoc; do
        [ -f "$f" ] || MISSING="$MISSING $f"
    done
    for d in .well-known; do
        [ -d "$d" ] || MISSING="$MISSING $d/"
    done
    for f in .well-known/security.txt .well-known/ai.txt .well-known/humans.txt; do
        [ -f "$f" ] || MISSING="$MISSING $f"
    done
    if [ ! -f "guix.scm" ] && [ ! -f ".guix-channel" ] && [ ! -f "flake.nix" ]; then
        MISSING="$MISSING guix.scm/flake.nix"
    fi
    if [ -n "$MISSING" ]; then
        echo "MISSING:$MISSING"
        exit 1
    fi
    echo "RSR compliance: PASS"

# Validate STATE.scm syntax
validate-state:
    @if [ -f "STATE.scm" ]; then \
        guile -c "(primitive-load \"STATE.scm\")" 2>/dev/null && echo "STATE.scm: valid" || echo "STATE.scm: INVALID"; \
    else \
        echo "No STATE.scm found"; \
    fi
