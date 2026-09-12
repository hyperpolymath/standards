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
APPLY="${APPLY_BASELINE_TARGET:-$SCRIPT_DIR/../apply-baseline.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

pass=0
fail=0

assert_status() {
  local label="$1" findings="$2" baseline="$3" expected="$4"
  local got
  got=$(bash "$APPLY" "$findings" "$baseline" advisory \
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
if bash "$APPLY" "$WORK/findings6.json" "$WORK/baseline6-invalid.json" advisory >/dev/null 2>&1; then
  echo "FAIL: malformed HYP--S009 rule type was accepted"
  fail=$((fail + 1))
else
  echo "PASS: malformed HYP--S009 rule type is rejected"
  pass=$((pass + 1))
fi

# Regex metacharacters and the former sentinel must remain literal glob text.
for literal in 'src/foo.rs' 'src/a+b(1)[2]{x}^$?.rs' 'src/DOUBLESTAR.rs'; do
  jq -n --arg file "$literal" '[{severity:"high",rule_module:"cicd_rules",type:"banned_language_file",file:$file}]' > "$WORK/literal.json"
  jq -n --arg pattern "$literal" '[{severity:"high",rule_module:"cicd_rules",type:"banned_language_file",file_pattern:$pattern}]' > "$WORK/literal-baseline.json"
  assert_status "literal glob matches itself: $literal" "$WORK/literal.json" "$WORK/literal-baseline.json" "1,0"
done
jq -n '[{severity:"high",rule_module:"cicd_rules",type:"banned_language_file",file_pattern:"src/foo.rs"}]' > "$WORK/literal-baseline.json"
for unrelated in 'src/fooXrs' $'src/foo.rs\nother'; do
  jq -n --arg file "$unrelated" '[{severity:"high",rule_module:"cicd_rules",type:"banned_language_file",file:$file}]' > "$WORK/unrelated.json"
  assert_status "literal glob rejects unrelated path" "$WORK/unrelated.json" "$WORK/literal-baseline.json" "0,1"
done

assert_invalid_option() {
  local label="$1" mode="$2" threshold="$3" status=0
  BLOCKING_THRESHOLD="$threshold" bash "$APPLY" "$WORK/findings1.json" "$WORK/empty.json" "$mode" > "$WORK/invalid.out" 2> "$WORK/invalid.err" || status=$?
  if [ "$status" -eq 2 ]; then
    echo "PASS: $label rejected as invalid configuration"
    pass=$((pass + 1))
  else
    echo "FAIL: $label returned $status (expected 2)"
    cat "$WORK/invalid.err"
    fail=$((fail + 1))
  fi
}
assert_invalid_option "invalid mode" bypass high
assert_invalid_option "invalid threshold" blocking nonsense

echo
echo "Total: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
