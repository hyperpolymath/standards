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
# ═══════════════════════════════════════════════════════════════════════
# List-valued `rule_module` (standards#966)
#
# ONE DEFECT CAN BE EMITTED BY TWO RULE MODULES. Hypatia raises
# `invalid_actions_lock` from BOTH `workflow_audit` and `workflow_hardening`
# for a single desynced lockfile. Under the old exact-string equality an
# acknowledgement could only name one of them, so the other stayed
# unsuppressed and went on blocking `main` — while the entry looked correct
# in every visible respect: right file, right severity, right type.
# ═══════════════════════════════════════════════════════════════════════

# The real-world reproduction. Two findings, one defect, ONE entry.
cat > "$WORK/findings-2mod.json" <<'EOF'
[{"severity":"high","rule_module":"workflow_audit","type":"invalid_actions_lock","file":".github/workflows/actions.lock"},
 {"severity":"high","rule_module":"workflow_hardening","type":"invalid_actions_lock","file":".github/workflows/actions.lock"}]
EOF
cat > "$WORK/baseline-2mod.json" <<'EOF'
[{"severity":"high","rule_module":["workflow_audit","workflow_hardening"],"type":"invalid_actions_lock","file_pattern":"**actions.lock"}]
EOF
assert_status "one list entry suppresses BOTH emitting modules" \
  "$WORK/findings-2mod.json" "$WORK/baseline-2mod.json" "2,0"

# Each member individually. A list that only ever matched its first element
# would pass the case above by luck if the findings were ordered kindly.
cat > "$WORK/findings-mod2only.json" <<'EOF'
[{"severity":"high","rule_module":"workflow_hardening","type":"invalid_actions_lock","file":".github/workflows/actions.lock"}]
EOF
assert_status "list matches a NON-FIRST member" \
  "$WORK/findings-mod2only.json" "$WORK/baseline-2mod.json" "1,0"

# The over-match control. A list must not become a wildcard.
cat > "$WORK/findings-3rd.json" <<'EOF'
[{"severity":"high","rule_module":"cicd_rules","type":"invalid_actions_lock","file":".github/workflows/actions.lock"}]
EOF
assert_status "list does NOT suppress a module it omits" \
  "$WORK/findings-3rd.json" "$WORK/baseline-2mod.json" "0,1"

# A one-element list must behave exactly like the bare string.
cat > "$WORK/baseline-1list.json" <<'EOF'
[{"severity":"high","rule_module":["workflow_audit"],"type":"invalid_actions_lock","file_pattern":"**actions.lock"}]
EOF
assert_status "single-element list == the string form (matches)" \
  "$WORK/findings-mod2only.json" "$WORK/baseline-1list.json" "0,1"
cat > "$WORK/findings-mod1only.json" <<'EOF'
[{"severity":"high","rule_module":"workflow_audit","type":"invalid_actions_lock","file":".github/workflows/actions.lock"}]
EOF
assert_status "single-element list == the string form (rejects)" \
  "$WORK/findings-mod1only.json" "$WORK/baseline-1list.json" "1,0"

# The string form must be untouched. This is the compatibility control for
# all 212 existing entries, every one of which uses a bare string.
cat > "$WORK/baseline-str.json" <<'EOF'
[{"severity":"high","rule_module":"workflow_audit","type":"invalid_actions_lock","file_pattern":"**actions.lock"}]
EOF
assert_status "bare string form still matches" \
  "$WORK/findings-mod1only.json" "$WORK/baseline-str.json" "1,0"
assert_status "bare string form still rejects the other module" \
  "$WORK/findings-mod2only.json" "$WORK/baseline-str.json" "0,1"

# ── MUTANT ────────────────────────────────────────────────────────────
# Restore the exact-equality comparison and assert the two-module case
# REGRESSES to half-suppressed. Without this, every assertion above would
# pass identically against an implementation that ignored the list entirely
# and matched on severity+type+file alone.
MUTANT="$WORK/apply-baseline-mutant.sh"
# Mutate the membership test to "first element only". This is the most
# plausible wrong implementation of a list match, and it is invisible to any
# assertion that happens to put the matching module first.
sed 's/| any(\. == \$finding\.rule_module)/| .[0] == $finding.rule_module/' \
  "$APPLY" > "$MUTANT"
chmod +x "$MUTANT"

if ! grep -q '\.\[0\] == \$finding\.rule_module' "$MUTANT"; then
  echo "FAIL: MUTANT was not applied — the sed anchor no longer matches apply-baseline.sh"
  fail=$((fail + 1))
elif ! bash -n "$MUTANT" 2>/dev/null; then
  echo "FAIL: MUTANT is not valid bash; the regression control did not execute"
  fail=$((fail + 1))
else
  # Non-first member must now be MISSED.
  mutant_got=$(bash "$MUTANT" "$WORK/findings-mod2only.json" "$WORK/baseline-2mod.json" advisory \
    | jq -r '"\(.findings_suppressed | length),\(.findings_kept | length)"')
  if [ "$mutant_got" = "0,1" ]; then
    echo "PASS: MUTANT (first element only) misses the non-first module — any() IS load-bearing"
    pass=$((pass + 1))
  else
    echo "FAIL: MUTANT expected 0,1 got $mutant_got — the list assertions do not depend on any()"
    fail=$((fail + 1))
  fi
  # And the two-module case must regress to half-suppressed: exactly the
  # #966 symptom, reproduced on demand.
  mutant_both=$(bash "$MUTANT" "$WORK/findings-2mod.json" "$WORK/baseline-2mod.json" advisory \
    | jq -r '"\(.findings_suppressed | length),\(.findings_kept | length)"')
  if [ "$mutant_both" = "1,1" ]; then
    echo "PASS: MUTANT reproduces the #966 symptom (half-suppressed, entry looks correct)"
    pass=$((pass + 1))
  else
    echo "FAIL: MUTANT expected 1,1 got $mutant_both"
    fail=$((fail + 1))
  fi
fi

# A list member that is not a valid module name must be REJECTED, not
# silently ignored. The validator is a gate, so it needs its own negative.
cat > "$WORK/baseline-badmember.json" <<'EOF'
[{"severity":"high","rule_module":["workflow_audit","Workflow-Hardening"],"type":"invalid_actions_lock","file_pattern":"**actions.lock"}]
EOF
if bash "$APPLY" "$WORK/findings-mod1only.json" "$WORK/baseline-badmember.json" advisory >/dev/null 2>&1; then
  echo "FAIL: a malformed rule_module list member was accepted"
  fail=$((fail + 1))
else
  echo "PASS: malformed rule_module list member rejected"
  pass=$((pass + 1))
fi

# An empty list names no module, so it can match nothing. Accepting it would
# create an entry that silently never applies.
cat > "$WORK/baseline-emptylist.json" <<'EOF'
[{"severity":"high","rule_module":[],"type":"invalid_actions_lock","file_pattern":"**actions.lock"}]
EOF
if bash "$APPLY" "$WORK/findings-mod1only.json" "$WORK/baseline-emptylist.json" advisory >/dev/null 2>&1; then
  echo "FAIL: an empty rule_module list was accepted"
  fail=$((fail + 1))
else
  echo "PASS: empty rule_module list rejected"
  pass=$((pass + 1))
fi

# --- The SHIPPED ledger, not a fixture -------------------------------------
# #966: hypatia emits invalid_actions_lock from BOTH workflow_audit and
# workflow_hardening for one defect. It was acked twice, which meant two
# expiry dates for one decision — the shape in which a half-expired ack
# silently reopens a gate. #971 made rule_module list-valued; this collapses
# the pair into one entry. These assertions run against the real
# .hypatia-baseline.json so that splitting it back, or narrowing the matcher,
# reds this suite instead of quietly un-suppressing a live finding.
SHIPPED="$SCRIPT_DIR/../../.hypatia-baseline.json"
if [ ! -f "$SHIPPED" ]; then
  echo "FAIL: shipped baseline not found at $SHIPPED"
  fail=$((fail + 1))
else
  # Exactly one entry, and it must name BOTH modules.
  got=$(jq -r '[.[] | select(.type == "invalid_actions_lock")] as $e
    | "\($e | length),\($e[0].rule_module | if type == "array" then length else 1 end)"' "$SHIPPED")
  if [ "$got" = "1,2" ]; then
    echo "PASS: shipped ledger holds ONE invalid_actions_lock ack naming TWO modules"
    pass=$((pass + 1))
  else
    echo "FAIL: shipped invalid_actions_lock acks: expected 1 entry / 2 modules, got $got"
    fail=$((fail + 1))
  fi

  # ⚠ The two emissions carry DIFFERENT file values — `actions.lock` from
  # workflow_audit and the full path from WH004 — so the surviving entry must
  # match by file_pattern. An exact `file` key would suppress only one of
  # them and the other would red main. Both, or the collapse is unsound.
  cat > "$WORK/findings-shipped-pair.json" <<'EOF'
[{"severity":"high","rule_module":"workflow_audit","type":"invalid_actions_lock","file":"actions.lock"},
 {"severity":"high","rule_module":"workflow_hardening","type":"invalid_actions_lock","file":".github/workflows/actions.lock"}]
EOF
  assert_status "shipped ledger suppresses BOTH emission paths" \
    "$WORK/findings-shipped-pair.json" "$SHIPPED" "2,0"

  # Over-match control: the collapse must not have turned the ack into a
  # blanket amnesty for anything hypatia says about actions.lock.
  cat > "$WORK/findings-shipped-other.json" <<'EOF'
[{"severity":"high","rule_module":"workflow_audit","type":"shell_download","file":"actions.lock"},
 {"severity":"high","rule_module":"workflow_lint","type":"invalid_actions_lock","file":"actions.lock"}]
EOF
  got=$(bash "$APPLY" "$WORK/findings-shipped-other.json" "$SHIPPED" advisory \
    | jq -r '"\(.findings_suppressed | length),\(.findings_kept | length)"')
  if [ "$got" = "0,2" ]; then
    echo "PASS: shipped ledger does NOT suppress other types or modules"
    pass=$((pass + 1))
  else
    echo "FAIL: shipped ledger over-matches: expected 0,2 got $got"
    fail=$((fail + 1))
  fi
fi

assert_invalid_option "invalid mode" bypass high
assert_invalid_option "invalid threshold" blocking nonsense

echo
echo "Total: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
