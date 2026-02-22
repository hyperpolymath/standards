#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Performance benchmarks for A2ML parser vs Markdown/AsciiDoc

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() { echo -e "${BLUE}ℹ️  ${NC}$1"; }
success() { echo -e "${GREEN}✓${NC} $1"; }
warn() { echo -e "${YELLOW}⚠️  ${NC}$1"; }

echo "╔══════════════════════════════════════════════════════════╗"
echo "║         A2ML Parser Performance Benchmarks              ║"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""

# Check dependencies
if ! command -v a2ml >/dev/null 2>&1; then
    warn "a2ml CLI not found. Building..."
    cd ../cli && ./build.sh
    export PATH="../build/exec:$PATH"
fi

if ! command -v cmark >/dev/null 2>&1; then
    warn "cmark (CommonMark reference) not found. Install: sudo dnf install cmark"
fi

if ! command -v asciidoctor >/dev/null 2>&1; then
    warn "asciidoctor not found. Install: sudo dnf install asciidoctor"
fi

# Create test documents of various sizes
create_test_docs() {
    mkdir -p test-docs

    info "Creating test documents..."

    # Small doc (1KB - ~50 lines)
    cat > test-docs/small.a2ml <<'EOF'
# Small Test Document

@abstract:
This is a small test document for benchmarking A2ML parsing performance.
@end

## Introduction

This document contains:
- A few sections
- Some bullet lists
- Code blocks

## Methods

@opaque(lang="bash"):
echo "Hello, world!"
date
ls -la
@end

## Results

The results show:
- Fast parsing
- Low memory usage
- Good performance

@refs:
[1] A2ML Specification
[2] Performance Guide
@end
EOF

    # Medium doc (10KB - ~500 lines)
    {
        echo "# Medium Test Document"
        echo ""
        echo "@abstract:"
        echo "This is a medium-sized test document with multiple sections and content."
        echo "@end"
        echo ""

        for i in {1..50}; do
            echo "## Section $i"
            echo ""
            echo "This is section $i with some content:"
            echo ""
            echo "- Point 1"
            echo "- Point 2"
            echo "- Point 3"
            echo ""
            echo "@opaque(lang=\"text\"):"
            echo "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
            echo "Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
            echo "@end"
            echo ""
        done

        echo "@refs:"
        for i in {1..10}; do
            echo "[$i] Reference $i"
        done
        echo "@end"
    } > test-docs/medium.a2ml

    # Large doc (100KB - ~5000 lines)
    {
        echo "# Large Test Document"
        echo ""
        echo "@abstract:"
        echo "This is a large test document simulating a full academic paper or RFC."
        echo "@end"
        echo ""

        for i in {1..500}; do
            echo "## Section $i"
            echo ""
            echo "This is section $i with substantial content:"
            echo ""
            echo "- Point 1 with longer text to increase size"
            echo "- Point 2 with more details and examples"
            echo "- Point 3 with comprehensive explanations"
            echo "- Point 4 with additional context"
            echo ""
            echo "Paragraph with more text to simulate real documents. "
            echo "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
            echo "Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
            echo ""
            echo "@opaque(lang=\"text\"):"
            for j in {1..5}; do
                echo "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
            done
            echo "@end"
            echo ""
        done

        echo "@refs:"
        for i in {1..50}; do
            echo "[$i] Reference $i with full citation details"
        done
        echo "@end"
    } > test-docs/large.a2ml

    # Convert to Markdown and AsciiDoc for comparison
    for size in small medium large; do
        # Markdown version (simplified conversion)
        sed 's/@abstract://' test-docs/${size}.a2ml | \
        sed 's/@opaque.*:/```/' | \
        sed 's/@end/```/' | \
        sed 's/@refs:/## References/' > test-docs/${size}.md

        # AsciiDoc version
        sed 's/^# /= /' test-docs/${size}.a2ml | \
        sed 's/^## /== /' test-docs/${size}.a2ml | \
        sed 's/@abstract:/[abstract]/' | \
        sed 's/@opaque.*:/----/' | \
        sed 's/@end/----/' > test-docs/${size}.adoc
    done

    success "Test documents created"
}

# Benchmark function
benchmark() {
    local desc="$1"
    local cmd="$2"
    local iterations=$3

    info "Benchmarking: $desc"

    local start=$(date +%s%3N)
    for ((i=0; i<iterations; i++)); do
        eval "$cmd" >/dev/null 2>&1 || true
    done
    local end=$(date +%s%3N)

    local total=$((end - start))
    local avg=$(echo "scale=2; $total / $iterations" | bc)

    echo "  Iterations: $iterations"
    echo "  Total time: ${total}ms"
    echo "  Average:    ${avg}ms"
    echo ""
}

# Run benchmarks
run_benchmarks() {
    info "Running parser benchmarks..."
    echo ""

    # Small document benchmarks
    echo "═══ Small Document (1KB) ═══"
    benchmark "A2ML parse (small)" "a2ml check test-docs/small.a2ml" 100
    if command -v cmark >/dev/null 2>&1; then
        benchmark "CommonMark (small)" "cmark test-docs/small.md" 100
    fi
    if command -v asciidoctor >/dev/null 2>&1; then
        benchmark "AsciiDoc (small)" "asciidoctor -o /dev/null test-docs/small.adoc" 100
    fi

    # Medium document benchmarks
    echo "═══ Medium Document (10KB) ═══"
    benchmark "A2ML parse (medium)" "a2ml check test-docs/medium.a2ml" 50
    if command -v cmark >/dev/null 2>&1; then
        benchmark "CommonMark (medium)" "cmark test-docs/medium.md" 50
    fi
    if command -v asciidoctor >/dev/null 2>&1; then
        benchmark "AsciiDoc (medium)" "asciidoctor -o /dev/null test-docs/medium.adoc" 50
    fi

    # Large document benchmarks
    echo "═══ Large Document (100KB) ═══"
    benchmark "A2ML parse (large)" "a2ml check test-docs/large.a2ml" 10
    if command -v cmark >/dev/null 2>&1; then
        benchmark "CommonMark (large)" "cmark test-docs/large.md" 10
    fi
    if command -v asciidoctor >/dev/null 2>&1; then
        benchmark "AsciiDoc (large)" "asciidoctor -o /dev/null test-docs/large.adoc" 10
    fi
}

# Memory profiling
memory_profile() {
    info "Memory profiling..."
    echo ""

    if ! command -v valgrind >/dev/null 2>&1; then
        warn "valgrind not found. Skipping memory profiling."
        return
    fi

    echo "═══ Memory Usage (Small Document) ═══"
    valgrind --tool=massif --massif-out-file=massif.out a2ml check test-docs/small.a2ml 2>&1 | grep -E "total heap usage|peak memory"

    echo ""
    echo "═══ Memory Usage (Large Document) ═══"
    valgrind --tool=massif --massif-out-file=massif.out a2ml check test-docs/large.a2ml 2>&1 | grep -E "total heap usage|peak memory"

    rm -f massif.out
}

# Main
main() {
    create_test_docs
    run_benchmarks
    memory_profile

    echo ""
    success "Benchmarks complete!"
    echo ""
    info "Results summary:"
    echo "  - Small docs: <100ms target"
    echo "  - Medium docs: <500ms target"
    echo "  - Large docs: <2s target"
}

main
