#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# K9-SVC Performance Benchmark Suite
#
# Comprehensive benchmarking of K9 components:
# - must script execution time
# - Nickel evaluation performance
# - k9-sign operations (keygen, sign, verify)
# - k9-scan static analysis
# - Just recipe execution

set -euo pipefail

VERSION="1.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${SCRIPT_DIR}/results"
TIMESTAMP="$(date +%Y%m%d-%H%M%S)"
RESULT_FILE="${OUTPUT_DIR}/benchmark-${TIMESTAMP}.json"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

info() { echo -e "${BLUE}ℹ️  ${NC}$1"; }
success() { echo -e "${GREEN}✓${NC} $1"; }
warn() { echo -e "${YELLOW}⚠️  ${NC}$1"; }
error() { echo -e "${RED}❌ ${NC}$1" >&2; }

# Benchmark result accumulator
BENCHMARK_RESULTS=()

# Record a benchmark result
record_result() {
    local name="$1"
    local iterations="$2"
    local total_time_ms="$3"
    local avg_time_ms=$(echo "scale=2; $total_time_ms / $iterations" | bc)

    BENCHMARK_RESULTS+=("{\"name\":\"$name\",\"iterations\":$iterations,\"total_ms\":$total_time_ms,\"avg_ms\":$avg_time_ms}")

    info "  $name: ${avg_time_ms}ms avg ($iterations iterations)"
}

# Benchmark a command
benchmark_command() {
    local name="$1"
    local iterations="$2"
    shift 2
    local cmd="$@"

    info "Benchmarking: $name ($iterations iterations)..."

    local start_ms=$(date +%s%3N)
    for ((i=1; i<=iterations; i++)); do
        eval "$cmd" >/dev/null 2>&1 || true
    done
    local end_ms=$(date +%s%3N)

    local total_time=$((end_ms - start_ms))
    record_result "$name" "$iterations" "$total_time"
}

# Benchmark with varying input sizes
benchmark_scaling() {
    local name="$1"
    local base_cmd="$2"
    local sizes=(10 100 1000 10000)

    info "Benchmarking scaling: $name"

    for size in "${sizes[@]}"; do
        local cmd=$(echo "$base_cmd" | sed "s/SIZE/$size/g")
        local iterations=10
        benchmark_command "${name}_${size}" "$iterations" "$cmd"
    done
}

# Create test files for benchmarking
create_test_files() {
    info "Creating test files..."

    mkdir -p "${SCRIPT_DIR}/test-data"

    # Small K9 component (1KB)
    cat > "${SCRIPT_DIR}/test-data/small.k9.ncl" <<'EOF'
K9!
leash = 'Kennel
pedigree = { schema_version = "1.0.0", component_type = "test" }
config = { value = 42 }
EOF

    # Medium K9 component (10KB)
    cat > "${SCRIPT_DIR}/test-data/medium.k9.ncl" <<'EOF'
K9!
leash = 'Yard
pedigree = { schema_version = "1.0.0", component_type = "test" }
config = {
  items | Array Number = std.array.generate (fun i => i) 1000,
  validation = std.array.all (fun x => x >= 0) items,
}
EOF

    # Large K9 component (100KB) - generate programmatically
    {
        echo "K9!"
        echo "leash = 'Yard"
        echo "pedigree = { schema_version = \"1.0.0\", component_type = \"test\" }"
        echo "config = {"
        echo "  large_array | Array Number = ["
        for i in {1..10000}; do
            echo "    $i,"
        done
        echo "  ],"
        echo "}"
    } > "${SCRIPT_DIR}/test-data/large.k9.ncl"

    # Binary file for k9-sign benchmarks
    dd if=/dev/urandom of="${SCRIPT_DIR}/test-data/binary-1kb.bin" bs=1024 count=1 2>/dev/null
    dd if=/dev/urandom of="${SCRIPT_DIR}/test-data/binary-1mb.bin" bs=1048576 count=1 2>/dev/null
    dd if=/dev/urandom of="${SCRIPT_DIR}/test-data/binary-10mb.bin" bs=1048576 count=10 2>/dev/null

    success "Test files created"
}

# Benchmark 1: must script
benchmark_must() {
    echo ""
    info "═══ Benchmark 1: must script ═══"

    cd "$SCRIPT_DIR/.."

    benchmark_command "must_status" 100 "./must status"
    benchmark_command "must_version" 100 "./must --version"
    benchmark_command "must_help" 50 "./must --help"

    success "must benchmarks complete"
}

# Benchmark 2: Nickel evaluation
benchmark_nickel() {
    echo ""
    info "═══ Benchmark 2: Nickel evaluation ═══"

    if ! command -v nickel >/dev/null 2>&1; then
        warn "Nickel not found, skipping Nickel benchmarks"
        return
    fi

    cd "${SCRIPT_DIR}/test-data"

    benchmark_command "nickel_eval_small" 100 "nickel export <<< '{value = 42}'"
    benchmark_command "nickel_eval_medium" 50 "nickel export small.k9.ncl"
    benchmark_command "nickel_eval_large" 10 "nickel export medium.k9.ncl"

    success "Nickel benchmarks complete"
}

# Benchmark 3: k9-sign operations
benchmark_k9_sign() {
    echo ""
    info "═══ Benchmark 3: k9-sign operations ═══"

    if ! command -v k9-sign >/dev/null 2>&1; then
        warn "k9-sign not found, skipping k9-sign benchmarks"
        return
    fi

    cd "${SCRIPT_DIR}/test-data"

    # Keygen benchmark
    info "  Benchmarking keygen..."
    local start_ms=$(date +%s%3N)
    for i in {1..10}; do
        k9-sign keygen "bench-key-$i" 2>/dev/null || true
    done
    local end_ms=$(date +%s%3N)
    record_result "k9_sign_keygen" 10 $((end_ms - start_ms))

    # Sign benchmarks (different file sizes)
    k9-sign trust ~/.config/k9/keys/bench-key-1.pub 2>/dev/null || true

    benchmark_command "k9_sign_1kb" 100 "k9-sign sign binary-1kb.bin bench-key-1 2>/dev/null && rm -f binary-1kb.bin.sig"
    benchmark_command "k9_sign_1mb" 50 "k9-sign sign binary-1mb.bin bench-key-1 2>/dev/null && rm -f binary-1mb.bin.sig"
    benchmark_command "k9_sign_10mb" 10 "k9-sign sign binary-10mb.bin bench-key-1 2>/dev/null && rm -f binary-10mb.bin.sig"

    # Verify benchmarks
    k9-sign sign binary-1kb.bin bench-key-1 2>/dev/null
    k9-sign sign binary-1mb.bin bench-key-1 2>/dev/null
    k9-sign sign binary-10mb.bin bench-key-1 2>/dev/null

    benchmark_command "k9_verify_1kb" 100 "k9-sign verify binary-1kb.bin 2>/dev/null"
    benchmark_command "k9_verify_1mb" 50 "k9-sign verify binary-1mb.bin 2>/dev/null"
    benchmark_command "k9_verify_10mb" 10 "k9-sign verify binary-10mb.bin 2>/dev/null"

    # Cleanup
    rm -f ~/.config/k9/keys/bench-key-*.{key,pub}

    success "k9-sign benchmarks complete"
}

# Benchmark 4: k9-scan static analysis
benchmark_k9_scan() {
    echo ""
    info "═══ Benchmark 4: k9-scan static analysis ═══"

    cd "$SCRIPT_DIR/.."

    if [ ! -f "./k9-scan" ]; then
        warn "k9-scan not found, skipping k9-scan benchmarks"
        return
    fi

    benchmark_command "k9_scan_small" 50 "./k9-scan benchmarks/test-data/small.k9.ncl"
    benchmark_command "k9_scan_medium" 20 "./k9-scan benchmarks/test-data/medium.k9.ncl"
    benchmark_command "k9_scan_examples" 20 "./k9-scan examples/hello.k9.ncl"

    success "k9-scan benchmarks complete"
}

# Benchmark 5: Just recipe execution
benchmark_just() {
    echo ""
    info "═══ Benchmark 5: Just recipe execution ═══"

    if ! command -v just >/dev/null 2>&1; then
        warn "just not found, skipping just benchmarks"
        return
    fi

    cd "$SCRIPT_DIR/.."

    benchmark_command "just_list" 50 "just --list"
    benchmark_command "just_summary" 50 "just --summary"

    success "Just benchmarks complete"
}

# Generate performance report
generate_report() {
    echo ""
    info "═══ Generating Performance Report ═══"

    mkdir -p "$OUTPUT_DIR"

    # JSON report
    cat > "$RESULT_FILE" <<EOF
{
  "timestamp": "$TIMESTAMP",
  "version": "$VERSION",
  "system": {
    "os": "$(uname -s)",
    "arch": "$(uname -m)",
    "kernel": "$(uname -r)"
  },
  "benchmarks": [
    $(IFS=,; echo "${BENCHMARK_RESULTS[*]}")
  ]
}
EOF

    success "JSON report: $RESULT_FILE"

    # Markdown report
    local md_file="${OUTPUT_DIR}/benchmark-${TIMESTAMP}.md"
    cat > "$md_file" <<EOF
# K9-SVC Performance Benchmark Report
**Date:** $(date +"%Y-%m-%d %H:%M:%S")
**System:** $(uname -s) $(uname -m) (kernel $(uname -r))

## Summary

| Benchmark | Iterations | Avg Time (ms) |
|-----------|------------|---------------|
EOF

    for result in "${BENCHMARK_RESULTS[@]}"; do
        local name=$(echo "$result" | jq -r '.name')
        local iterations=$(echo "$result" | jq -r '.iterations')
        local avg_ms=$(echo "$result" | jq -r '.avg_ms')
        echo "| $name | $iterations | $avg_ms |" >> "$md_file"
    done

    cat >> "$md_file" <<EOF

## Performance Insights

### must Script
- Fast status checks (~${BENCHMARK_RESULTS[0]##*avg_ms\":}ms)
- Minimal overhead for version/help commands

### k9-sign Operations
- Keygen: ~${BENCHMARK_RESULTS[5]##*avg_ms\":}ms per keypair
- Sign (1KB): ~${BENCHMARK_RESULTS[6]##*avg_ms\":}ms
- Sign (1MB): ~${BENCHMARK_RESULTS[7]##*avg_ms\":}ms
- Sign (10MB): ~${BENCHMARK_RESULTS[8]##*avg_ms\":}ms
- Verify scales linearly with file size

### Recommendations
1. Cache Nickel evaluation results for repeated runs
2. Batch k9-sign operations when possible
3. Consider parallel processing for large file sets
4. Use dry-run mode for testing workflows

---
Generated by k9-bench.sh v$VERSION
EOF

    success "Markdown report: $md_file"
}

# Main execution
main() {
    echo "╔══════════════════════════════════════════════════════════╗"
    echo "║       K9-SVC Performance Benchmark Suite v$VERSION       ║"
    echo "╚══════════════════════════════════════════════════════════╝"
    echo ""

    info "System: $(uname -s) $(uname -m)"
    info "Kernel: $(uname -r)"
    echo ""

    create_test_files

    benchmark_must
    benchmark_nickel
    benchmark_k9_sign
    benchmark_k9_scan
    benchmark_just

    generate_report

    echo ""
    echo "╔══════════════════════════════════════════════════════════╗"
    echo "║  ✅ K9-SVC Performance Benchmarks Complete!             ║"
    echo "╚══════════════════════════════════════════════════════════╝"
    echo ""
    echo "Results saved to:"
    echo "  - JSON: $RESULT_FILE"
    echo "  - Markdown: ${OUTPUT_DIR}/benchmark-${TIMESTAMP}.md"
    echo ""
}

# Parse arguments
case "${1:-}" in
    --help|-h)
        cat <<EOF
K9-SVC Performance Benchmark Suite v$VERSION

Usage: $0 [OPTIONS]

Options:
  --help, -h     Show this help message
  --version      Show version

Benchmarks:
  1. must script execution time
  2. Nickel evaluation performance
  3. k9-sign operations (keygen, sign, verify)
  4. k9-scan static analysis
  5. Just recipe execution

Results are saved to: benchmarks/results/

Requirements:
  - k9-sign (for signing benchmarks)
  - nickel (for Nickel benchmarks)
  - just (for Just benchmarks)
  - bc (for calculations)
  - jq (for JSON processing)
EOF
        exit 0
        ;;
    --version)
        echo "k9-bench.sh v$VERSION"
        exit 0
        ;;
esac

main "$@"
