#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# verdict-grid-test.sh — the findings-family grid never turns missing or
# unreadable evidence into a clean row, grades the worst finding present,
# keeps explicit scope exclusions grey, and compares revisions only under one
# profile. docs/VERDICT-GRID-SPEC.adoc is the contract under test.
#
# Run: bash scripts/tests/verdict-grid-test.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GRID="$SCRIPT_DIR/../verdict-grid.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

pass=0
fail=0
check() {
  local label="$1" expected="$2" got="$3"
  if [ "$got" = "$expected" ]; then
    echo "PASS: $label ($got)"; pass=$((pass + 1))
  else
    echo "FAIL: $label expected=$expected got=$got"; fail=$((fail + 1))
  fi
}
render() { bash "$GRID" render --out "$1" "${@:2}" >/dev/null; }
state() { jq -r .state "$1/report.json"; }
row() { jq -r --arg id "$2" '.reporters[] | select(.id == $id) | .state' "$1/report.json"; }

hyp_clean="$WORK/hyp-clean.json";    echo '[]' > "$hyp_clean"
hyp_crit="$WORK/hyp-crit.json";      echo '[{"severity":"critical","rule_module":"cicd_rules","type":"banned_language_file","file":"a.ts","reason":"x"},{"severity":"warn","rule_module":"r","type":"t","file":"b","reason":"y"}]' > "$hyp_crit"
hyp_med="$WORK/hyp-med.json";        echo '[{"severity":"medium","rule_module":"r","type":"t","file":"b","reason":"y"}]' > "$hyp_med"
hyp_info="$WORK/hyp-info.json";      echo '[{"severity":"info","rule_module":"r","type":"t","file":"b","reason":"y"}]' > "$hyp_info"
hyp_bad="$WORK/hyp-bad.json";        echo 'not json' > "$hyp_bad"
hyp_odd="$WORK/hyp-odd.json";        echo '[{"severity":"catastrophic","rule_module":"r","type":"t","reason":"y"}]' > "$hyp_odd"
pan_ran="$WORK/pan-ran.json";        echo '{"status":"ran","tool":"panic-attack","ref":"abc","reason":null,"exit_code":0,"report":{"weak_points":[{"category":"ProofDrift","file":"t.jl","severity":"Medium","description":"d"},{"category":"X","file":"s","severity":"Critical","description":"e","suppressed":true}]}}' > "$pan_ran"
pan_not="$WORK/pan-not.json";        echo '{"status":"not_run","tool":"panic-attack","ref":"abc","reason":"cargo is not available","exit_code":null,"counts":null,"report":null}' > "$pan_not"
pan_clean="$WORK/pan-clean.json";    echo '{"status":"ran","tool":"panic-attack","ref":"abc","reason":null,"exit_code":0,"report":{"weak_points":[]}}' > "$pan_clean"
pons_ran="$WORK/pons-ran.json";      echo '{"status":"ran","tool":"pons-asinorum","ref":"def","reason":null,"exit_code":0,"report":{"findings":[{"rule_id":"unreachable-after-jump","tier":"T0","evidence":"HEURISTIC","severity":"WARN","location":{"file":"a.tsx","line_start":4},"message":"m"}]}}' > "$pons_ran"
pons_clean="$WORK/pons-clean.json";  echo '{"status":"ran","tool":"pons-asinorum","ref":"def","reason":null,"exit_code":0,"report":{"findings":[]}}' > "$pons_clean"
pons_hollow="$WORK/pons-hollow.json"; echo '{"status":"ran","tool":"pons-asinorum","ref":"def"}' > "$pons_hollow"

# 1. No evidence at all is orange, never green, and the badge says so.
render "$WORK/o1"
check "no evidence is indeterminate" orange "$(state "$WORK/o1")"
check "badge colour follows the state" orange "$(jq -r .color "$WORK/o1/badge.json")"
check "missing scanner output is an error row" error "$(jq -r '.reporters[] | select(.id == "hypatia") | .status' "$WORK/o1/report.json")"

# 2. A known critical beats every other state, including missing evidence.
render "$WORK/o2" --hypatia "$hyp_crit" --panic "$pan_not"
check "critical finding is purple" purple "$(state "$WORK/o2")"
check "warn counts as medium" 1 "$(jq '.reporters[0].counts.medium' "$WORK/o2/report.json")"
check "not_run envelope is orange" orange "$(row "$WORK/o2" panic-attack)"
check "badge message names the worst grade" critical "$(jq -r .message "$WORK/o2/badge.json")"

# 3. Malformed or unrecognised evidence is orange.
render "$WORK/o3" --hypatia "$hyp_bad" --panic "$pan_clean" --pons "$pons_clean"
check "unparseable findings are indeterminate" orange "$(state "$WORK/o3")"
render "$WORK/o3b" --hypatia "$hyp_odd" --panic "$pan_clean" --pons "$pons_clean"
check "unknown severity is indeterminate" orange "$(row "$WORK/o3b" hypatia)"
render "$WORK/o3c" --hypatia "$hyp_clean" --panic "$pan_clean" --pons "$pons_hollow"
check "a ran envelope without a report is indeterminate" orange "$(row "$WORK/o3c" pons-asinorum)"

# 4. Grades below critical: red, yellow, blue, green; suppressed points are not counted.
render "$WORK/o4" --hypatia "$hyp_med" --panic "$pan_ran" --pons "$pons_ran"
check "medium is yellow" yellow "$(state "$WORK/o4")"
check "suppressed weak points are not counted" 0 "$(jq '.reporters[] | select(.id == "panic-attack") | .counts.critical' "$WORK/o4/report.json")"
check "pons WARN is medium" yellow "$(row "$WORK/o4" pons-asinorum)"
render "$WORK/o5" --hypatia "$hyp_info" --panic "$pan_clean" --pons "$pons_clean"
check "info only is blue" blue "$(state "$WORK/o5")"
render "$WORK/o6" --hypatia "$hyp_clean" --panic "$pan_clean" --pons "$pons_clean"
check "all clean is green" green "$(state "$WORK/o6")"

# 5. Fleet rows: enrolled-without-report is orange, not-enrolled is grey with a reason,
#    a report-back supplies its own row, and grey never masks a finding.
mkdir -p "$WORK/backs"
echo '{"reporter":"rhodibot","title":"rhodibot (RSR)","status":"ran","ref":"r1","findings":[{"rule_id":"RSR-001","severity":"error","file":"README.adoc","message":"missing"}]}' > "$WORK/backs/rhodibot.json"
echo '{"reporters":{"rhodibot":{"enrolled":true},"panicbot":{"title":"panicbot","enrolled":true},"glambot":{"enrolled":false,"reason":"no served HTML"}}}' > "$WORK/enrol.json"
render "$WORK/o7" --hypatia "$hyp_clean" --panic "$pan_clean" --pons "$pons_clean" --report-backs "$WORK/backs" --enrolment "$WORK/enrol.json"
check "fleet error severity is high" red "$(row "$WORK/o7" rhodibot)"
check "enrolled member without a report-back is orange" orange "$(row "$WORK/o7" panicbot)"
check "member not enrolled is grey" grey "$(row "$WORK/o7" glambot)"
check "grey rows do not hide a red one" red "$(state "$WORK/o7")"
check "the grey reason is shown" yes "$(grep -q 'no served HTML' "$WORK/o7/report.md" && echo yes)"
echo 'nonsense' > "$WORK/enrol-bad.json"
render "$WORK/o8" --hypatia "$hyp_clean" --panic "$pan_clean" --pons "$pons_clean" --enrolment "$WORK/enrol-bad.json" 2>/dev/null
check "an unreadable enrolment manifest is missing evidence" orange "$(state "$WORK/o8")"

# 6. Escaping: untrusted text cannot break the table or inject markup.
echo '[{"severity":"high","rule_module":"r","type":"t</details><script>x</script>","file":"a|b","reason":"line one\nline two"}]' > "$WORK/hyp-esc.json"
render "$WORK/o9" --hypatia "$WORK/hyp-esc.json" --panic "$pan_clean" --pons "$pons_clean"
check "markup is escaped" 0 "$(grep -c '<script>' "$WORK/o9/report.md" || true)"
check "pipes are escaped" 1 "$(grep -c 'a&#124;b' "$WORK/o9/report.md")"

# 7. The delta column: same profile and PR, different commit; otherwise no comparison.
render "$WORK/d1" --hypatia "$hyp_crit" --panic "$pan_clean" --pons "$pons_clean" --pr 7 --sha aaaa
render "$WORK/d2" --hypatia "$hyp_clean" --panic "$pan_clean" --pons "$pons_clean" --pr 7 --sha bbbb --previous "$WORK/d1/report.md"
check "fewer blocking findings is an improvement" 1 "$(grep -c '↑ improved (1 fewer blocking)' "$WORK/d2/report.md")"
render "$WORK/d3" --hypatia "$hyp_clean" --panic "$pan_clean" --pons "$pons_clean" --pr 7 --sha bbbb --previous "$WORK/d2/report.md"
check "the same commit is not compared with itself" 0 "$(grep -c 'improved' "$WORK/d3/report.md" || true)"
render "$WORK/d4" --hypatia "$hyp_clean" --panic "$pan_clean" --pons "$pons_clean" --pr 8 --sha cccc --previous "$WORK/d1/report.md"
check "another PR is no baseline" 3 "$(grep -c 'no comparable baseline' "$WORK/d4/report.md")"
render "$WORK/d5" --hypatia "$hyp_clean" --panic "$pan_not" --pons "$pons_clean" --pr 7 --sha dddd --previous "$WORK/d1/report.md"
check "a changed evidence status is not an improvement" 1 "$(grep -c '— evidence changed' "$WORK/d5/report.md")"
check "baseline round-trips" aaaa "$(bash "$GRID" baseline "$WORK/d1/report.md" | jq -r .sha)"
check "a garbled baseline reads as null" null "$(printf '<!-- verdict-grid-baseline:xxxx -->' > "$WORK/garbled.md"; bash "$GRID" baseline "$WORK/garbled.md")"

echo
echo "verdict-grid-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
