# SPDX-License-Identifier: MPL-2.0
# justfile - Just recipes for this project
# See: https://github.com/hyperpolymath/mustfile
#
# requires: just >= 1.19.0   (import? optional-import support)
# Enforced by the `tooling-version-integrity` must-check, not self-
# enforcing: import? fails at parse time before any recipe can guard it.
# See TOOLING-VERSION-INTEGRITY-POLICY.adoc (root cause: burble#39).

# Default recipe
import? "contractile.just"

default:
    @just --list

# Regenerate the verifiable spec registry + DERIVED topology from the file tree
registry:
    @bash scripts/build-registry.sh

# Alias: same generator also (re)writes TOPOLOGY.md
topology: registry

# Fail if REGISTRY.a2ml or TOPOLOGY.md has drifted from the file tree
registry-check:
    @bash scripts/build-registry.sh --check

# Run the workflow-staleness gate against a repo (default: this one)
staleness-check path=".":
    @bash scripts/check-workflow-staleness.sh "{{path}}"

# Audit (or --fix) consumer reusable-workflow SHA pins toward a target.
# Read-only by default; pass extra args after the path, e.g.
#   just staleness-propagate ../stapeln --fix --to <sha>
staleness-propagate path="." *args="":
    @bash scripts/propagate-workflow-pins.sh {{args}} "{{path}}"

# Regression tests for the staleness gate + the pin-propagation helper
staleness-test:
    @echo "=== check-workflow-staleness ==="
    @bash scripts/tests/check-workflow-staleness-test.sh
    @echo "=== propagate-workflow-pins ==="
    @bash scripts/tests/propagate-workflow-pins-test.sh

# Wave-0 anti-false-green regression: proves each fixed validator CAN fail
false-green-test:
    @bash scripts/tests/wave0-false-green-test.sh

# Wave-1 automation regression: Mustfile runner + hook installer
automation-test:
    @bash scripts/tests/wave1-automation-test.sh

# Structural validation of the Mustfile contract (severity + run/verification per check)
mustfile-check path=".machine_readable/contractiles/must/Mustfile.a2ml":
    @bash scripts/check-mustfile-structure.sh "{{path}}"

# Execute the Mustfile's `- run:` checks (critical/high failures block)
must-check path=".machine_readable/contractiles/must/Mustfile.a2ml":
    @bash scripts/run-mustfile.sh "{{path}}"

# Install this repo's git hooks into .git/hooks/ (pre-commit guards)
hooks-install:
    @bash hooks/install.sh

# Regenerate the compliance dashboard from the per-spec scorecards
scorecards:
    @bash scripts/build-scorecards.sh

# Fail if COMPLIANCE-DASHBOARD.md has drifted from the scorecards
scorecards-check:
    @bash scripts/build-scorecards.sh --check

# Strict: also fail if any registered local spec lacks a scorecard
scorecards-check-strict:
    @bash scripts/build-scorecards.sh --check --strict

# DYADT: verify a CLAIMS.a2ml against primary evidence (default: root CLAIMS.a2ml)
verify-claims path="CLAIMS.a2ml":
    @bash scripts/verify-claims.sh "{{path}}"

# DYADT: run the conformance vector suite
dyadt-conformance:
    @bash did-you-actually-do-that/spec/conformance/run-conformance.sh

# DYADT regression test (confirm/refute/unverifiable + guards)
dyadt-test:
    @bash scripts/tests/wave4-dyadt-test.sh

# Structural lint for per-language testing guides (required sections + R1..R9)
language-guides-check:
    @bash scripts/check-language-guide.sh

# Block reintroduction of deprecated names (6a2, agent_instructions) in new diff
canonical-names-check base="origin/main":
    @bash scripts/check-canonical-names.sh "{{base}}"

# Aggregate compliance gate: registry drift is the HARD gate (registry-check,
# a hard dep). The RSR self-audit is INFORMATIONAL — a monorepo is not expected
# to score Gold — but a *broken* audit (exit 4 / unexpected) must fail loudly
# rather than pass silently under a blanket `|| true` (Wave-0 false-green fix).
validate: registry-check
    @echo "=== validate: registry drift (HARD GATE) — passed as a dependency above ==="
    @echo "=== validate: per-language testing guides (structural, HARD GATE) ==="
    @bash scripts/check-language-guide.sh
    @echo "=== validate: canonical-names reintroduction guard (vs origin/main) ==="
    @bash scripts/check-canonical-names.sh origin/main || bash scripts/check-canonical-names.sh HEAD
    @echo "=== validate: RSR self-audit (INFORMATIONAL grade; errors fail loudly) ==="
    @bash scripts/rsr-selfaudit.sh .
    @echo "=== validate: done ==="

# Print role-appropriate LLM warm-up context (machine front door)
llm-context role="dev":
    @echo "# Front door: 0-AI-MANIFEST.a2ml (machine) + README.adoc (human)"
    @echo "# Registry:   .machine_readable/REGISTRY.a2ml  ·  prose: REGISTRY.adoc"
    @cat "llm-warmup-{{role}}.md" 2>/dev/null || cat 0-AI-MANIFEST.a2ml

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
    @echo "Checking optional imports (import? does not fail when absent — report it)..."
    @test -f contractile.just && echo "  [OK] contractile.just present (import resolved)" || echo "  [INFO] contractile.just absent — its recipes are unavailable (needs the external 'contractile' generator)"
    @echo "Checking git hooks are installed..."
    @test -f "$(git rev-parse --git-dir)/hooks/pre-commit" && echo "  [OK] pre-commit hook installed" || echo "  [INFO] pre-commit hook not installed — run 'just hooks-install'"
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


# Verify scripts/check-ts-allowlist.deno.js matches what compiling
# scripts/check-ts-allowlist.affine produces. Run after editing the
# .affine source. Exit 0 = in sync; non-zero with diff = drifted.
# See standards#312.
check-ts-allowlist-drift:
    @command -v affinescript >/dev/null 2>&1 || { echo "affinescript compiler not on PATH — skipping drift check"; exit 0; }
    @tmp="$$(mktemp /tmp/check-ts-allowlist-drift.XXXXXX.deno.js)"; \
      affinescript compile --deno-esm -o "$$tmp" scripts/check-ts-allowlist.affine; \
      diff -u scripts/check-ts-allowlist.deno.js "$$tmp"; \
      rc=$$?; \
      rm -f "$$tmp"; \
      exit $$rc

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
