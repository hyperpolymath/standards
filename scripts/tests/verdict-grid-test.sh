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
# ⚠ CONTRACT CHANGE (scope + gate separation, 2026-10-24). The lane state used
# to be the worst row of ANY kind, so an advisory fleet report-back could turn
# the lane red while every gating check was green. It is now the worst GATING
# row; the advisory worst is surfaced on the next line and must name the red.
check "an advisory red does not set the gating state" green "$(state "$WORK/o7")"
check "the advisory red is still reported and named" yes \
  "$(grep -q 'Advisory: .*worst .*' "$WORK/o7/report.md" && grep -q '🔴' "$WORK/o7/report.md" && echo yes)"
check "the row itself is still red, and marked advisory" yes \
  "$(grep -q '| rhodibot (RSR) | advisory | 🔴' "$WORK/o7/report.md" && echo yes)"
check "the grey reason is shown" yes "$(grep -q 'no served HTML' "$WORK/o7/report.md" && echo yes)"
echo 'nonsense' > "$WORK/enrol-bad.json"
render "$WORK/o8" --hypatia "$hyp_clean" --panic "$pan_clean" --pons "$pons_clean" --enrolment "$WORK/enrol-bad.json" 2>/dev/null
# The enrolment manifest governs which ADVISORY fleet members should report.
# Breaking it no longer reddens the lane -- the gating row is independent -- but
# it must remain visible as missing advisory evidence, never as clean.
check "an unreadable enrolment manifest still surfaces as missing evidence" yes \
  "$(grep -q 'Advisory: .*🟠 indeterminate' "$WORK/o8/report.md" && echo yes)"

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

# 8. Scope: what the grid is allowed to GRADE.
#    The failures that matter here are the quiet ones -- a delta grid that
#    silently grades everything (a verdict that never moves) and an exclusion
#    that swallows a finding it should not have matched.
printf 'scripts/touched.sh\n' > "$WORK/changed.txt"
cat > "$WORK/hyp-mixed.json" <<'JSON'
[
 {"severity":"medium","rule_module":"cp","type":"t","file":"scripts/touched.sh","reason":"in scope"},
 {"severity":"critical","rule_module":"cp","type":"t","file":"scripts/untouched.sh","reason":"standing"},
 {"severity":"high","rule_module":"cp","type":"t","file":"9-archive/old.adoc","reason":"frozen"}
]
JSON
render "$WORK/s1" --hypatia "$WORK/hyp-mixed.json" --panic "$WORK/pan_clean" --pons "$WORK/pons_clean" \
  --changed "$WORK/changed.txt" --exclude-prefix 9-archive/
check "delta scope grades only the touched file" yellow "$(row "$WORK/s1" hypatia)"
check "the standing critical does not grade" 0 "$(jq -r '.reporters[] | select(.id=="hypatia") | .counts.critical' "$WORK/s1/report.json")"
check "the standing critical IS counted" 1 "$(jq -r '.reporters[] | select(.id=="hypatia") | .counts_standing.critical' "$WORK/s1/report.json")"
check "the excluded finding is neither graded nor standing" 1 "$(jq -r '.reporters[] | select(.id=="hypatia") | .counts_excluded.high' "$WORK/s1/report.json")"
check "the identity line states the scope" yes \
  "$(grep -q 'scope: this change (1 file' "$WORK/s1/report.md" && echo yes)"
check "the identity line names the exclusion" yes \
  "$(grep -q 'excludes .*9-archive/' "$WORK/s1/report.md" && echo yes)"
check "the standing finding is named in the accordion" yes \
  "$(grep -q 'further finding(s) in files this change does not touch' "$WORK/s1/report.md" && echo yes)"

render "$WORK/s2" --hypatia "$WORK/hyp-mixed.json" --panic "$WORK/pan_clean" --pons "$WORK/pons_clean"
check "without --changed the whole repository is graded" purple "$(row "$WORK/s2" hypatia)"
check "whole-repo mode says so" yes "$(grep -q 'scope: whole repository' "$WORK/s2/report.md" && echo yes)"

# A finding with no path cannot be shown to be outside the change: it grades.
printf '[{"severity":"high","rule_module":"cp","type":"t","file":"","reason":"no path"}]' > "$WORK/hyp-nopath.json"
render "$WORK/s3" --hypatia "$WORK/hyp-nopath.json" --panic "$WORK/pan_clean" --pons "$WORK/pons_clean" \
  --changed "$WORK/changed.txt"
check "a location-less finding is graded, not silently standing" red "$(row "$WORK/s3" hypatia)"

# Exclusion applies in whole-repo mode too: frozen history is out of scope
# whether or not the change touched it.
render "$WORK/s4" --hypatia "$WORK/hyp-mixed.json" --panic "$WORK/pan_clean" --pons "$WORK/pons_clean" \
  --exclude-prefix 9-archive/
check "exclusion applies without delta scoping" purple "$(row "$WORK/s4" hypatia)"
check "...and the excluded row is not counted as graded" 0 \
  "$(jq -r '.reporters[] | select(.id=="hypatia") | .counts.high' "$WORK/s4/report.json")"

# An unreadable changed-file list is a usage error, never a silent whole-repo
# run: grading the repository when the caller asked for a delta would colour the
# PR for debt it did not introduce, with nothing to say which question was asked.
# (Under `set -e` the failing call is read with `||`, never bare.)
rc=0
bash "$GRID" render --out "$WORK/s5" --hypatia "$WORK/hyp-mixed.json" \
  --changed "$WORK/does-not-exist.txt" >/dev/null 2>&1 || rc=$?
check "a missing changed-file list exits 2..." 2 "$rc"
check "...and writes no report to be misread" no \
  "$(if [ -f "$WORK/s5/report.json" ]; then echo yes; else echo no; fi)"

# A scope change invalidates the comparison rather than faking one.
render "$WORK/s6" --hypatia "$WORK/hyp-mixed.json" --panic "$WORK/pan_clean" --pons "$WORK/pons_clean" --pr 9 --sha s1
render "$WORK/s7" --hypatia "$WORK/hyp-mixed.json" --panic "$WORK/pan_clean" --pons "$WORK/pons_clean" --pr 9 --sha s2 \
  --changed "$WORK/changed.txt" --previous "$WORK/s6/report.md"
check "a change of scope is not a comparable baseline" 3 "$(grep -c 'no comparable baseline' "$WORK/s7/report.md")"

echo
echo "verdict-grid-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
