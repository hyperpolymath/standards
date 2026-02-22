#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# K9-SVC Quick Performance Benchmark
#
# Minimal benchmark focusing on core K9 components without external dependencies.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
K9_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Simple timing function
time_command() {
    local desc="$1"
    local iterations=$2
    shift 2
    local cmd="$@"

    local start=$(date +%s%3N)
    for ((i=0; i<iterations; i++)); do
        eval "$cmd" >/dev/null 2>&1 || true
    done
    local end=$(date +%s%3N)

    local total=$((end - start))
    local avg=$(echo "scale=2; $total / $iterations" | bc)

    printf "%-40s %6s ms/op (%d iterations)\n" "$desc" "$avg" "$iterations"
}

echo "╔══════════════════════════════════════════════════════════╗"
echo "║         K9-SVC Quick Performance Benchmark              ║"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""

cd "$K9_ROOT"

# Benchmark 1: must script
echo "Must Script:"
time_command "  must status" 50 "./must status"
time_command "  must --version" 50 "./must --version"
time_command "  must --help" 20 "./must --help"
echo ""

# Benchmark 2: k9-scan (if exists)
if [ -f "./k9-scan" ]; then
    echo "K9-Scan:"
    time_command "  k9-scan (small file)" 20 "./k9-scan examples/hello.k9.ncl"
    echo ""
fi

# Benchmark 3: k9-sign (if installed)
if command -v k9-sign >/dev/null 2>&1; then
    echo "K9-Sign:"

    # Create test file
    dd if=/dev/urandom of=/tmp/k9-test-1kb.bin bs=1024 count=1 2>/dev/null

    # Generate test key if needed
    if [ ! -f ~/.config/k9/keys/perf-test.key ]; then
        k9-sign keygen perf-test 2>/dev/null
    fi

    k9-sign trust ~/.config/k9/keys/perf-test.pub 2>/dev/null

    time_command "  k9-sign keygen" 10 "k9-sign keygen temp-key-\$RANDOM 2>/dev/null && rm -f ~/.config/k9/keys/temp-key-*.{key,pub}"
    time_command "  k9-sign sign (1KB)" 50 "k9-sign sign /tmp/k9-test-1kb.bin perf-test 2>/dev/null && rm -f /tmp/k9-test-1kb.bin.sig"

    # Sign once for verify test
    k9-sign sign /tmp/k9-test-1kb.bin perf-test 2>/dev/null
    time_command "  k9-sign verify (1KB)" 50 "k9-sign verify /tmp/k9-test-1kb.bin 2>/dev/null"

    # Cleanup
    rm -f /tmp/k9-test-1kb.bin /tmp/k9-test-1kb.bin.sig
    echo ""
fi

echo "╔══════════════════════════════════════════════════════════╗"
echo "║  ✅ Quick Benchmark Complete                            ║"
echo "╚══════════════════════════════════════════════════════════╝"
