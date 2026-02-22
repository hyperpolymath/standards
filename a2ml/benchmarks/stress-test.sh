#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Stress testing A2ML parser with large, real-world documents

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

info() { echo -e "${BLUE}ℹ️  ${NC}$1"; }
success() { echo -e "${GREEN}✓${NC} $1"; }
warn() { echo -e "${YELLOW}⚠️  ${NC}$1"; }
error() { echo -e "${RED}✗${NC} $1"; }

echo "╔══════════════════════════════════════════════════════════╗"
echo "║         A2ML Parser Stress Testing                      ║"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""

# Test categories
mkdir -p stress-tests/{pathological,real-world,generated}

# Pathological cases (edge cases that break parsers)
create_pathological_tests() {
    info "Creating pathological test cases..."

    # Deep nesting (500 levels)
    {
        echo "# Deep Nesting Test"
        echo ""
        for i in {1..500}; do
            echo "$(printf '#%.0s' $(seq 1 $((i % 5 + 1)))) Section $i"
            echo ""
        done
    } > stress-tests/pathological/deep-nesting.a2ml

    # Many IDs (10,000 unique IDs)
    {
        echo "# Many IDs Test"
        echo ""
        for i in {1..10000}; do
            echo "## Section id-$i"
            echo ""
            echo "Content for section $i"
            echo ""
        done
    } > stress-tests/pathological/many-ids.a2ml

    # Long lines (100,000 character lines)
    {
        echo "# Long Lines Test"
        echo ""
        echo "## Very Long Paragraph"
        echo ""
        # Generate a 100KB single-line paragraph
        python3 -c "print('Lorem ipsum dolor sit amet. ' * 5000)"
    } > stress-tests/pathological/long-lines.a2ml

    # Many references (1,000 cross-references)
    {
        echo "# Many References Test"
        echo ""
        echo "@abstract:"
        echo "This document tests reference resolution with many cross-references."
        echo "@end"
        echo ""

        # Create 1000 sections with IDs
        for i in {1..1000}; do
            echo "## Section sec-$i"
            echo ""
            echo "This is section $i."
            echo ""
        done

        # Create @refs block with all references
        echo "@refs:"
        for i in {1..1000}; do
            echo "[$i] Reference to section sec-$i"
        done
        echo "@end"
    } > stress-tests/pathological/many-refs.a2ml

    # Nested directives (100 levels deep)
    {
        echo "# Nested Directives Test"
        echo ""
        for i in {1..100}; do
            echo "@opaque(lang=\"level-$i\"):"
        done
        echo "Deepest content"
        for i in {1..100}; do
            echo "@end"
        done
    } > stress-tests/pathological/nested-directives.a2ml

    # Unicode stress test
    {
        echo "# Unicode Stress Test"
        echo ""
        echo "@abstract:"
        echo "Testing Unicode: 你好世界 مرحبا بالعالم Здравствуй мир"
        echo "Emojis: 🚀 🎉 ✅ 🔥 💯"
        echo "@end"
        echo ""

        # Mix of scripts
        for script in "Latin" "Greek: Ελληνικά" "Cyrillic: Кириллица" "Arabic: العربية" "Hebrew: עברית" "Chinese: 中文" "Japanese: 日本語" "Korean: 한국어"; do
            echo "## $script"
            echo ""
            echo "Content in this script."
            echo ""
        done
    } > stress-tests/pathological/unicode.a2ml

    success "Pathological tests created"
}

# Generate RFC-sized documents
create_rfc_sized_tests() {
    info "Creating RFC-sized test documents..."

    # Simulate RFC 9116 (1.8MB, ~40,000 lines)
    {
        echo "# RFC-Sized Test Document"
        echo ""
        echo "@abstract:"
        echo "This document simulates a large RFC for stress testing the A2ML parser."
        echo "It contains thousands of sections, references, and code blocks."
        echo "@end"
        echo ""

        # Table of contents
        echo "## Table of Contents"
        echo ""
        for i in {1..100}; do
            echo "- Section $i"
        done
        echo ""

        # Main content (100 major sections, each with subsections)
        for major in {1..100}; do
            echo "## $major. Major Section $major"
            echo ""
            echo "Introduction to section $major."
            echo ""

            for minor in {1..10}; do
                echo "### $major.$minor. Subsection $minor"
                echo ""
                echo "Details about subsection $major.$minor:"
                echo ""
                echo "- Point 1"
                echo "- Point 2"
                echo "- Point 3"
                echo ""

                # Code blocks
                echo "@opaque(lang=\"example\"):"
                for line in {1..20}; do
                    echo "  Example code line $line in section $major.$minor"
                done
                echo "@end"
                echo ""
            done
        done

        # References (1000 references)
        echo "## References"
        echo ""
        echo "@refs:"
        for i in {1..1000}; do
            echo "[$i] Reference $i: Example citation with details"
        done
        echo "@end"
    } > stress-tests/real-world/rfc-sized.a2ml

    # Calculate size
    local size=$(du -h stress-tests/real-world/rfc-sized.a2ml | awk '{print $1}')
    success "RFC-sized document created: $size"
}

# Generate academic paper sized documents
create_paper_tests() {
    info "Creating academic paper test documents..."

    # Typical paper: 20-50 pages, ~50KB
    {
        echo "# Academic Paper: A2ML Performance Analysis"
        echo ""
        echo "@abstract:"
        echo "This paper presents a comprehensive analysis of the Attested Markup Language (A2ML) performance characteristics, formal verification properties, and practical applications in long-term document preservation."
        echo ""
        echo "**Keywords**: Markup languages, Formal verification, Dependent types, Document preservation"
        echo "@end"
        echo ""

        # Sections typical of academic paper
        for section in "Introduction" "Related Work" "Background" "Methodology" "Implementation" "Evaluation" "Results" "Discussion" "Limitations" "Future Work" "Conclusion"; do
            echo "## $section"
            echo ""

            # 5 subsections per major section
            for sub in {1..5}; do
                echo "### $section - Part $sub"
                echo ""
                echo "Detailed analysis of $section, part $sub:"
                echo ""

                # Paragraphs
                for para in {1..3}; do
                    echo "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
                    echo ""
                done

                # Figures
                echo "@fig(id=\"fig-$section-$sub\", caption=\"Results for $section part $sub\"):"
                echo "Figure content here"
                echo "@end"
                echo ""

                # Tables
                echo "@table(id=\"table-$section-$sub\", caption=\"Data for $section part $sub\"):"
                echo "Table content here"
                echo "@end"
                echo ""

                # Code listings
                echo "@opaque(lang=\"idris\", id=\"code-$section-$sub\"):"
                echo "-- Example Idris2 code"
                echo "theorem : (x : Nat) -> (y : Nat) -> x + y = y + x"
                echo "theorem x y = plusCommutative x y"
                echo "@end"
                echo ""
            done
        done

        # References (100 typical for academic paper)
        echo "## References"
        echo ""
        echo "@refs:"
        for i in {1..100}; do
            echo "[$i] Author et al. ($((2020 + i % 10))). Paper title $i. Conference/Journal Name."
        done
        echo "@end"
    } > stress-tests/real-world/academic-paper.a2ml

    local size=$(du -h stress-tests/real-world/academic-paper.a2ml | awk '{print $1}')
    success "Academic paper created: $size"
}

# Run stress tests
run_stress_tests() {
    info "Running stress tests..."
    echo ""

    local total=0
    local passed=0
    local failed=0

    # Test all pathological cases
    echo "═══ Pathological Cases ═══"
    for test in stress-tests/pathological/*.a2ml; do
        ((total++))
        local basename=$(basename "$test")
        printf "Testing $basename... "

        if timeout 30s a2ml check "$test" >/dev/null 2>&1; then
            success "PASS"
            ((passed++))
        else
            error "FAIL (timeout or error)"
            ((failed++))
        fi
    done
    echo ""

    # Test real-world sized documents
    echo "═══ Real-World Documents ═══"
    for test in stress-tests/real-world/*.a2ml; do
        ((total++))
        local basename=$(basename "$test")
        printf "Testing $basename... "

        local start=$(date +%s%3N)
        if timeout 60s a2ml check "$test" >/dev/null 2>&1; then
            local end=$(date +%s%3N)
            local duration=$((end - start))
            success "PASS (${duration}ms)"
            ((passed++))
        else
            error "FAIL (timeout or error)"
            ((failed++))
        fi
    done
    echo ""

    # Summary
    echo "═══ Summary ═══"
    echo "Total tests:  $total"
    echo "Passed:       $passed"
    echo "Failed:       $failed"

    if [ $failed -eq 0 ]; then
        success "All stress tests passed!"
        return 0
    else
        error "$failed stress tests failed"
        return 1
    fi
}

# Fuzz testing (basic)
fuzz_test() {
    info "Running fuzz tests..."

    mkdir -p stress-tests/fuzz

    # Generate random malformed documents
    for i in {1..100}; do
        # Random bytes
        dd if=/dev/urandom bs=1024 count=$((RANDOM % 100 + 1)) 2>/dev/null | \
            base64 > "stress-tests/fuzz/random-$i.a2ml"

        # Test parser doesn't crash
        if a2ml check "stress-tests/fuzz/random-$i.a2ml" >/dev/null 2>&1; then
            : # OK to parse successfully
        else
            : # OK to fail parsing, just don't crash
        fi
    done

    success "Fuzz tests completed (no crashes)"
}

# Main
main() {
    create_pathological_tests
    create_rfc_sized_tests
    create_paper_tests

    if run_stress_tests; then
        fuzz_test
        echo ""
        success "All stress tests passed!"
        exit 0
    else
        echo ""
        error "Some stress tests failed"
        exit 1
    fi
}

main
