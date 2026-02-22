#!/bin/bash
#
# Main test runner script for the Palimpsest project.
# Runs all available test suites: Deno, OCaml, Haskell

set -e

echo "🧪 Starting Palimpsest project tests..."
echo ""

FAILED=0

# ---------------------------------------------------------------------------
# 1. Deno: Lint and format check for JavaScript
# ---------------------------------------------------------------------------
echo "📦 [1/4] Deno lint & format check..."
if command -v deno &> /dev/null; then
    deno lint --quiet 2>/dev/null || echo "   ⚠️  Deno lint warnings (non-blocking)"
    deno fmt --check --quiet 2>/dev/null || echo "   ⚠️  Deno format warnings (non-blocking)"
    echo "   ✅ Deno checks complete"
else
    echo "   ⏭️  Deno not installed, skipping"
fi
echo ""

# ---------------------------------------------------------------------------
# 2. OCaml: Run parser/compliance tests
# ---------------------------------------------------------------------------
echo "🐫 [2/4] OCaml tests..."
if [ -d "ocaml" ] && [ -f "ocaml/dune-project" ]; then
    if command -v opam &> /dev/null; then
        cd ocaml
        opam exec -- dune test 2>/dev/null && echo "   ✅ OCaml tests passed" || {
            echo "   ❌ OCaml tests failed"
            FAILED=1
        }
        cd ..
    else
        echo "   ⏭️  opam not installed, skipping OCaml tests"
    fi
else
    echo "   ⏭️  No OCaml project found"
fi
echo ""

# ---------------------------------------------------------------------------
# 3. Haskell: Run validation tests
# ---------------------------------------------------------------------------
echo "λ  [3/4] Haskell tests..."
if [ -d "TOOLS/validation/haskell" ] && [ -f "TOOLS/validation/haskell/package.yaml" ]; then
    if command -v stack &> /dev/null; then
        cd TOOLS/validation/haskell
        stack test --fast 2>/dev/null && echo "   ✅ Haskell tests passed" || {
            echo "   ❌ Haskell tests failed"
            FAILED=1
        }
        cd ../../..
    else
        echo "   ⏭️  stack not installed, skipping Haskell tests"
    fi
else
    echo "   ⏭️  No Haskell project found"
fi
echo ""

# ---------------------------------------------------------------------------
# 4. Markdown: Link and structure validation
# ---------------------------------------------------------------------------
echo "📝 [4/4] Markdown validation..."
if command -v markdownlint &> /dev/null; then
    markdownlint "*.md" --ignore node_modules 2>/dev/null || echo "   ⚠️  Markdown lint warnings (non-blocking)"
    echo "   ✅ Markdown check complete"
else
    echo "   ⏭️  markdownlint not installed, skipping"
fi
echo ""

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [ $FAILED -eq 0 ]; then
    echo "✅ All tests passed successfully!"
    exit 0
else
    echo "❌ Some tests failed. See output above."
    exit 1
fi