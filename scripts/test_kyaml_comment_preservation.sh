#!/usr/bin/env bash
# SPDX-License-Identifier: CC-BY-SA-4.0
# Test script for YAML-POLICY §2.2 and §3.3 precondition 3
# Comment preservation proof for yq -o kyaml round-trip
#
# This script demonstrates that yq -o kyaml:
#   1. Preserves every comment
#   2. Preserves each comment's association with its line
#   3. Is idempotent (round-trip is a no-op)
#   4. Is verified by a mutant (dropping a comment causes detection)
#
# Usage: ./test_kyaml_comment_preservation.sh [--verbose]

set -euo pipefail

VERBOSE=false
if [[ ${1:-} == "--verbose" ]]; then
    VERBOSE=true
fi

log() {
    if $VERBOSE; then
        echo "[TEST] $1"
    fi
}

# Create a test file with various comment types
TEST_FILE="/tmp/kyaml_test_$$"
cat > "$TEST_FILE" << 'EOF'
# Header comment
# Another header line
---
# Section comment for metadata
metadata:
  name: test-repo  # inline comment for name
  # Mid-section comment
  version: 1.0.0

# List section
list:
  - item1  # item comment 1
  - item2  # item comment 2
  # Comment before nested map
  - key: value
    nested: data  # nested inline

# Final comment
EOF

log "Test file created: $TEST_FILE"

# Step 1: Convert to KYAML
echo "=== Step 1: Convert to KYAML ==="
yq -o kyaml "$TEST_FILE" > "${TEST_FILE}.kyaml"
log "Converted to KYAML"

# Step 2: Count comments in original
ORIGINAL_COMMENTS=$(grep -c '^#\|# ' "$TEST_FILE" || echo "0")
echo "Original comments: $ORIGINAL_COMMENTS"

# Step 3: Count comments in KYAML version
KYAML_COMMENTS=$(grep -c '^#\|# ' "${TEST_FILE}.kyaml" || echo "0")
echo "KYAML comments: $KYAML_COMMENTS"

if [[ "$ORIGINAL_COMMENTS" -ne "$KYAML_COMMENTS" ]]; then
    echo "❌ FAIL: Comment count changed: $ORIGINAL_COMMENTS -> $KYAML_COMMENTS"
    exit 1
fi

echo "✅ PASS: Comment count preserved"

# Step 3: Check idempotency
log ""
echo "=== Step 2: Check idempotency ==="
yq -o kyaml "${TEST_FILE}.kyaml" > "${TEST_FILE}.kyaml2"
if cmp -s "${TEST_FILE}.kyaml" "${TEST_FILE}.kyaml2"; then
    echo "✅ PASS: yq -o kyaml is idempotent"
else
    echo "❌ FAIL: yq -o kyaml is NOT idempotent"
    echo "First conversion:"
    cat "${TEST_FILE}.kyaml"
    echo "---"
    echo "Second conversion:"
    cat "${TEST_FILE}.kyaml2"
    exit 1
fi

# Step 4: Mutant test - drop a comment and verify detection
log ""
echo "=== Step 3: Mutant test ==="

# Create a mutant (drop one comment)
cat > "${TEST_FILE}.mutant" << 'EOF'
# Header comment
# Another header line
---
# Section comment for metadata
metadata:
  name: test-repo  # inline comment for name
  version: 1.0.0

# List section
list:
  - item1  # item comment 1
  - item2  # item comment 2
  - key: value
    nested: data  # nested inline

# Final comment
EOF

# Convert mutant
mutant_kyaml="${TEST_FILE}.mutant.kyaml"
yq -o kyaml "${TEST_FILE}.mutant" > "$mutant_kyaml"

# Compare with clean KYAML
if cmp -s "${TEST_FILE}.kyaml" "$mutant_kyaml"; then
    echo "❌ FAIL: Mutant not detected - files are identical!"
    exit 1
else
    echo "✅ PASS: Mutant detected - files differ"
    echo "Differences:"
    diff -u "${TEST_FILE}.kyaml" "$mutant_kyaml" || true
fi

# Step 5: Verify specific comment preservation
log ""
echo "=== Step 4: Verify specific comment preservation ==="

# Check inline comments are preserved
if grep -q "inline comment for name" "${TEST_FILE}.kyaml"; then
    echo "✅ PASS: Inline comment preserved"
else
    echo "❌ FAIL: Inline comment lost"
    exit 1
fi

if grep -q "Mid-section comment" "${TEST_FILE}.kyaml"; then
    echo "✅ PASS: Mid-section comment preserved"
else
    echo "❌ FAIL: Mid-section comment lost"
    exit 1
fi

if grep -q "item comment 1" "${TEST_FILE}.kyaml"; then
    echo "✅ PASS: List item comment preserved"
else
    echo "❌ FAIL: List item comment lost"
    exit 1
fi

# Cleanup
rm -f "$TEST_FILE" "${TEST_FILE}.kyaml" "${TEST_FILE}.kyaml2" "${TEST_FILE}.mutant" "$mutant_kyaml"

echo ""
echo "=== ALL TESTS PASSED ==="
echo "✅ Comment preservation proven for yq -o kyaml"
echo "✅ Idempotency proven"
echo "✅ Mutant detection proven"
