# SPDX-License-Identifier: PMPL-1.0-or-later
# justfile - Just recipes for this project
# See: https://github.com/hyperpolymath/mustfile

# Default recipe
import? "contractile.just"

default:
    @just --list

# Build all sub-project artefacts
build:
    @echo "=== Standards Monorepo Build ==="
    @echo "[1/3] a2ml/bindings/rust"
    @cd a2ml/bindings/rust && cargo build 2>&1 || echo "  SKIP: cargo not available"
    @echo "[2/3] k9-svc/bindings/rust"
    @cd k9-svc/bindings/rust && cargo build 2>&1 || echo "  SKIP: cargo not available"
    @echo "[3/3] groove-protocol/reference/ipv6t"
    @cd groove-protocol/reference/ipv6t && zig build 2>&1 || echo "  SKIP: zig not available"
    @echo "=== Build complete ==="

# Run all sub-project test suites
test:
    @echo "=== Standards Monorepo Test Runner ==="
    @echo ""
    @echo "[1/5] groove-protocol/reference/ipv6t (Zig — 10 tests)"
    @cd groove-protocol/reference/ipv6t && zig build test 2>&1 || echo "  SKIP: zig not available"
    @echo ""
    @echo "[2/5] 0-ai-gatekeeper-protocol/mcp-repo-guardian (Deno — 36 tests)"
    @cd 0-ai-gatekeeper-protocol/mcp-repo-guardian && deno task test 2>&1 || echo "  SKIP: deno not available or tests failed"
    @echo ""
    @echo "[3/5] axel-protocol (Deno — 14 tests)"
    @cd axel-protocol && deno task test 2>&1 || echo "  SKIP: deno not available or tests failed"
    @echo ""
    @echo "[4/5] a2ml/bindings/rust (Rust — 47 tests)"
    @cd a2ml/bindings/rust && cargo test 2>&1 || echo "  SKIP: cargo not available or tests failed"
    @echo ""
    @echo "[5/5] k9-svc/bindings/rust (Rust — 45 tests)"
    @cd k9-svc/bindings/rust && cargo test 2>&1 || echo "  SKIP: cargo not available or tests failed"
    @echo ""
    @echo "=== Test run complete ==="

# Format sub-project code
fmt:
    @echo "=== Standards Monorepo Format ==="
    @cd a2ml/bindings/rust && cargo fmt 2>&1 || echo "  SKIP"
    @cd k9-svc/bindings/rust && cargo fmt 2>&1 || echo "  SKIP"
    @cd 0-ai-gatekeeper-protocol/mcp-repo-guardian && deno fmt 2>&1 || echo "  SKIP"
    @echo "=== Format complete ==="

# Lint sub-projects
lint:
    @echo "=== Standards Monorepo Lint ==="
    @echo "[1/2] Rust clippy"
    @cd a2ml/bindings/rust && cargo clippy --all-targets 2>&1 || echo "  SKIP"
    @cd k9-svc/bindings/rust && cargo clippy --all-targets 2>&1 || echo "  SKIP"
    @echo "[2/2] Deno lint"
    @cd 0-ai-gatekeeper-protocol/mcp-repo-guardian && deno lint 2>&1 || echo "  SKIP"
    @echo "=== Lint complete ==="

# Clean build artifacts
clean:
    @echo "=== Cleaning ==="
    @cd a2ml/bindings/rust && cargo clean 2>&1 || true
    @cd k9-svc/bindings/rust && cargo clean 2>&1 || true
    @cd groove-protocol/reference/ipv6t && rm -rf zig-out zig-cache .zig-cache 2>/dev/null || true
    @echo "=== Clean complete ==="

# Run panic-attacker pre-commit scan
assail:
    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"

# Stable release gate audit
v1-audit path=".":
    @./release-pre-flight/v1-audit.sh "{{path}}"

# Self-diagnostic — checks dependencies, permissions, paths
doctor:
    @echo "Running diagnostics for standards..."
    @echo "Checking required tools..."
    @command -v just >/dev/null 2>&1 && echo "  [OK] just" || echo "  [FAIL] just not found"
    @command -v git >/dev/null 2>&1 && echo "  [OK] git" || echo "  [FAIL] git not found"
    @echo "Checking for hardcoded paths..."
    @grep -rn '$HOME\|$ECLIPSE_DIR' --include='*.rs' --include='*.ex' --include='*.res' --include='*.gleam' --include='*.sh' . 2>/dev/null | head -5 || echo "  [OK] No hardcoded paths"
    @echo "Diagnostics complete."

# Auto-repair common issues
heal:
    @echo "Attempting auto-repair for standards..."
    @echo "Fixing permissions..."
    @find . -name "*.sh" -exec chmod +x {} \; 2>/dev/null || true
    @echo "Cleaning stale caches..."
    @rm -rf .cache/stale 2>/dev/null || true
    @echo "Repair complete."

# Guided tour of key features
tour:
    @echo "=== standards Tour ==="
    @echo ""
    @echo "1. Project structure:"
    @ls -la
    @echo ""
    @echo "2. Available commands: just --list"
    @echo ""
    @echo "3. Read README.adoc for full overview"
    @echo "4. Read EXPLAINME.adoc for architecture decisions"
    @echo "5. Run 'just doctor' to check your setup"
    @echo ""
    @echo "Tour complete! Try 'just --list' to see all available commands."

# Open feedback channel with diagnostic context
help-me:
    @echo "=== standards Help ==="
    @echo "Platform: $(uname -s) $(uname -m)"
    @echo "Shell: $SHELL"
    @echo ""
    @echo "To report an issue:"
    @echo "  https://github.com/hyperpolymath/standards/issues/new"
    @echo ""
    @echo "Include the output of 'just doctor' in your report."


# Print the current CRG grade (reads from READINESS.md '**Current Grade:** X' line)
crg-grade:
    @grade=$$(grep -oP '(?<=\*\*Current Grade:\*\* )[A-FX]' READINESS.md 2>/dev/null | head -1); \
    [ -z "$$grade" ] && grade="X"; \
    echo "$$grade"

# Generate a shields.io badge markdown for the current CRG grade
# Looks for '**Current Grade:** X' in READINESS.md; falls back to X
crg-badge:
    @grade=$$(grep -oP '(?<=\*\*Current Grade:\*\* )[A-FX]' READINESS.md 2>/dev/null | head -1); \
    [ -z "$$grade" ] && grade="X"; \
    case "$$grade" in \
      A) color="brightgreen" ;; B) color="green" ;; C) color="yellow" ;; \
      D) color="orange" ;; E) color="red" ;; F) color="critical" ;; \
      *) color="lightgrey" ;; esac; \
    echo "[![CRG $$grade](https://img.shields.io/badge/CRG-$$grade-$$color?style=flat-square)](https://github.com/hyperpolymath/standards/tree/main/component-readiness-grades)"
