#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# apply-baseline-test.sh — regression test for apply-baseline.sh's
# file_pattern glob handling.
#
# The original implementation referenced `.file_pattern` inside `test(...)`,
# where jq rebinds `.` to test's input (a string), causing a "Cannot index
# string" error that was silently masked by `select(...)`'s error-tolerance
# and produced an always-matches result. This test pins both the exact-file
# and glob paths so the regression cannot recur.
#
# Run: bash scripts/tests/apply-baseline-test.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
APPLY="$SCRIPT_DIR/../apply-baseline.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

pass=0
fail=0

assert_status() {
  local label="$1" findings="$2" baseline="$3" expected="$4"
  local got
  got=$("$APPLY" "$findings" "$baseline" advisory 2>/dev/null \
    | jq -r '"\(.findings_suppressed | length),\(.findings_kept | length)"')
  if [ "$got" = "$expected" ]; then
    echo "PASS: $label  (suppressed,kept=$got)"
    pass=$((pass + 1))
  else
    echo "FAIL: $label  expected=$expected got=$got"
    fail=$((fail + 1))
  fi
}

# === Case 1: exact `file` match ===
cat > "$WORK/findings1.json" <<'EOF'
[{"severity":"high","rule_module":"cicd_rules","type":"banned_language_file","file":"src/Legacy.kt"}]
EOF
cat > "$WORK/baseline1.json" <<'EOF'
[{"severity":"high","rule_module":"cicd_rules","type":"banned_language_file","file":"src/Legacy.kt"}]
EOF
assert_status "exact file match suppresses" \
  "$WORK/findings1.json" "$WORK/baseline1.json" "1,0"

# === Case 2: file_pattern `examples/**` matches nested file ===
cat > "$WORK/findings2.json" <<'EOF'
[{"severity":"high","rule_module":"cicd_rules","type":"banned_language_file","file":"examples/kotlin/Nop.kt"}]
EOF
cat > "$WORK/baseline2.json" <<'EOF'
[{"severity":"high","rule_module":"cicd_rules","type":"banned_language_file","file_pattern":"examples/**"}]
EOF
assert_status "file_pattern matches nested file" \
  "$WORK/findings2.json" "$WORK/baseline2.json" "1,0"

# === Case 3: file_pattern MUST NOT match unrelated file (regression
# against the always-matches bug from `.file_pattern` inside test()) ===
cat > "$WORK/findings3.json" <<'EOF'
[{"severity":"high","rule_module":"cicd_rules","type":"banned_language_file","file":"src/Unrelated.kt"}]
EOF
assert_status "file_pattern does not over-match" \
  "$WORK/findings3.json" "$WORK/baseline2.json" "0,1"

# === Case 4: single-segment * does not cross / ===
cat > "$WORK/findings4a.json" <<'EOF'
[{"severity":"high","rule_module":"cicd_rules","type":"banned_language_file","file":"vendor/acme/legacy.java"}]
EOF
cat > "$WORK/findings4b.json" <<'EOF'
[{"severity":"high","rule_module":"cicd_rules","type":"banned_language_file","file":"vendor/acme/deep/legacy.java"}]
EOF
cat > "$WORK/baseline4.json" <<'EOF'
[{"severity":"high","rule_module":"cicd_rules","type":"banned_language_file","file_pattern":"vendor/*/legacy.java"}]
EOF
assert_status "single * matches one segment" \
  "$WORK/findings4a.json" "$WORK/baseline4.json" "1,0"
assert_status "single * does not cross slash" \
  "$WORK/findings4b.json" "$WORK/baseline4.json" "0,1"

# === Case 5: empty baseline keeps the finding ===
echo '[]' > "$WORK/empty.json"
assert_status "empty baseline keeps finding" \
  "$WORK/findings2.json" "$WORK/empty.json" "0,1"

# === Case 6: emitted hyphenated Hypatia rule IDs are representable ===
cat > "$WORK/findings6.json" <<'EOF'
[{"severity":"medium","rule_module":"implementation_inside_canon","type":"HYP-S009","file":"spec/Cargo.toml"}]
EOF
cat > "$WORK/baseline6.json" <<'EOF'
[{"severity":"medium","rule_module":"implementation_inside_canon","type":"HYP-S009","file":"spec/Cargo.toml"}]
EOF
assert_status "hyphenated HYP-S009 rule type is valid and matches" \
  "$WORK/findings6.json" "$WORK/baseline6.json" "1,0"

# A malformed doubled separator must remain invalid.
cat > "$WORK/baseline6-invalid.json" <<'EOF'
[{"severity":"medium","rule_module":"implementation_inside_canon","type":"HYP--S009","file":"spec/Cargo.toml"}]
EOF
if "$APPLY" "$WORK/findings6.json" "$WORK/baseline6-invalid.json" advisory >/dev/null 2>&1; then
  echo "FAIL: malformed HYP--S009 rule type was accepted"
  fail=$((fail + 1))
else
  echo "PASS: malformed HYP--S009 rule type is rejected"
  pass=$((pass + 1))
fi

echo
echo "Total: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
