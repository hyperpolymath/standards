#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Regression suite for ci-pipeline.yml's `detect` scan step.
#
# ⚠ THIS SUITE EXECUTES THE SHIPPED YAML, NOT A COPY OF IT. The step body is
# extracted from .github/workflows/ci-pipeline.yml at run time. A test that
# re-implements the logic it is testing passes forever after the real thing
# breaks, which is the failure mode this file exists to avoid.
#
# The property under test is NOT "detection works". It is:
#
#     an ecosystem the pipeline cannot CHECK must never lift TOTAL above zero
#
# because TOTAL == 0 is what triggers the refusal. A probe added to the wrong
# accumulator silences the refusal while examining nothing — a vacuous gate,
# which reports success. Fixture 3 and the mutant below are the controls for
# exactly that.
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOW="$ROOT/.github/workflows/ci-pipeline.yml"
PASS=0; FAIL=0
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Extract the `run:` body of the step whose id is `scan`.
#
# No Python: LANGUAGE-POLICY bans it, so the extraction is awk. The block is
# the run-on scalar after `run: |`, i.e. every following line indented deeper
# than the `run:` key itself, plus blank lines. 10 leading spaces are stripped.
extract_scan() {
  awk '
    /^        id: scan$/            { seen=1 }
    seen && /^        run: \|$/     { grab=1; next }
    grab {
      if ($0 ~ /^[[:space:]]*$/) { print ""; next }
      if ($0 !~ /^          /)   { exit }
      sub(/^          /, ""); print
    }
  ' "$WORKFLOW"
}

SCAN="$WORK/scan.sh"
extract_scan > "$SCAN"
[ -s "$SCAN" ] || { echo "FATAL: extracted an EMPTY scan body — the awk anchors no longer match $WORKFLOW"; exit 2; }
grep -q 'N_UNSUPPORTED=0' "$SCAN" || { echo "FATAL: extraction produced a body with no N_UNSUPPORTED accumulator"; exit 2; }

# Run the extracted body inside a throwaway git repo holding $* as tracked
# files. Sets: T (total), U (n_unsupported), RC, and OUT (stderr+stdout).
run_fixture() {
  local fail_on="$1"; shift
  local mutate="$1"; shift
  local repo; repo="$(mktemp -d -p "$WORK")"
  (
    cd "$repo" || exit 9
    git init -q . && git config user.email t@t && git config user.name t
    for f in "$@"; do mkdir -p "$(dirname "$f")"; : > "$f"; done
    [ $# -gt 0 ] && git add -A && git -c commit.gpgsign=false commit -qm x >/dev/null 2>&1
    cp "$SCAN" ./s.sh
    # The workflow expression is not shell; substitute it as Actions would.
    sed -i "s|\${{ inputs.fail_on_no_ecosystem }}|$fail_on|g" ./s.sh
    [ -n "$mutate" ] && sed -i "$mutate" ./s.sh
    GITHUB_OUTPUT=./out GITHUB_STEP_SUMMARY=./sum bash ./s.sh 2>&1
    echo "__RC__=$?"
    cat ./out 2>/dev/null
  ) > "$repo/.captured" 2>&1
  OUT="$(cat "$repo/.captured")"
  RC="$(sed -n 's/^__RC__=//p' <<< "$OUT" | tail -1)"
  T="$(sed -n 's/^total=//p' <<< "$OUT" | tail -1)"
  U="$(sed -n 's/^n_unsupported=//p' <<< "$OUT" | tail -1)"
  # The refusal VERDICT. It used to be RC: `detect` exited 1 and the job went
  # red. That made `detect` a SECOND judge alongside `report`, so the exemption
  # ledger — which can only spare `report`'s own exit — could never actually
  # grant a repository the green it promises. The verdict is now an OUTPUT and
  # the job stays green; `report` is the single judge.
  #
  # ⚠ So RC no longer discriminates a refusal from a pass, and every assertion
  # below that used to read RC now reads this. Leaving them on RC would not
  # have failed — it would have passed VACUOUSLY, asserting 0 == 0 forever.
  R="$(sed -n 's/^refused=//p' <<< "$OUT" | tail -1)"
}

check() {
  local what="$1" got="$2" want="$3"
  if [ "$got" = "$want" ]; then PASS=$((PASS+1)); printf '  ok   %-46s = %s\n' "$what" "$got"
  else FAIL=$((FAIL+1)); printf '  FAIL %-46s = %s (wanted %s)\n' "$what" "$got" "$want"; fi
}

echo "== Fixture 1: empty repo, fail_on_no_ecosystem=true =="
run_fixture true '' 
check "total"                    "$T"   "0"
check "n_unsupported"            "$U"   "0"
check "REFUSES (verdict)"        "$R"   "true"
check "job still exits 0"        "$RC"  "0"
check "names the UNRECOGNISED case" "$(grep -qF 'No known ecosystem detected' <<< "$OUT" && echo y || echo n)" "y"
check "does NOT claim unsupported"   "$(grep -qF 'no lint or format job for any of them' <<< "$OUT" && echo y || echo n)" "n"

echo "== Fixture 2: Cargo.toml only (supported) =="
run_fixture true '' Cargo.toml
check "total"                    "$T"   "1"
check "n_unsupported"            "$U"   "0"
check "exit code (green)"        "$RC"  "0"
check "verdict written FALSE"    "$R"   "false"
check "no refusal text"          "$(grep -qF 'REFUSED' <<< "$OUT" && echo y || echo n)" "n"

echo "== Fixture 3: Project.toml only (DETECTED, UNSUPPORTED) =="
echo "   the load-bearing case: Julia must NOT lift total above zero"
run_fixture true '' Project.toml
check "total STAYS zero"         "$T"   "0"
check "n_unsupported"            "$U"   "1"
check "still REFUSES (verdict)"  "$R"   "true"
check "job still exits 0"        "$RC"  "0"
check "names Julia"              "$(grep -qF 'Julia (1)' <<< "$OUT" && echo y || echo n)" "y"
check "uses the DEBT message"    "$(grep -qF 'no lint or format job for any of them' <<< "$OUT" && echo y || echo n)" "y"
check "cites #967"               "$(grep -qF 'standards#967' <<< "$OUT" && echo y || echo n)" "y"
check "NOT the unrecognised msg" "$(grep -qF 'No known ecosystem detected' <<< "$OUT" && echo y || echo n)" "n"

echo "== Fixture 3b: same, fail_on_no_ecosystem=false =="
run_fixture false '' Project.toml
check "warns, does not fail"     "$RC"  "0"
# The warning-only escape hatch must NOT set the verdict, or `report` would
# block a repository the caller deliberately chose not to fail.
check "verdict FALSE when warn-only" "$R" "false"
check "warning names Julia"      "$(grep -qF '::warning::Detected Julia (1)' <<< "$OUT" && echo y || echo n)" "y"

echo "== Fixture 4: both Cargo.toml and Project.toml =="
run_fixture true '' Cargo.toml Project.toml
check "total counts ONLY rust"   "$T"   "1"
check "unsupported counts julia" "$U"   "1"
check "green (something checked)" "$RC" "0"
check "verdict written FALSE"    "$R"   "false"

echo "== Fixture 5: Bun markers =="
run_fixture true '' package.json bunfig.toml
check "total"                    "$T"   "0"
check "n_unsupported"            "$U"   "2"
check "names Bun"                "$(grep -qF 'Bun (2)' <<< "$OUT" && echo y || echo n)" "y"
check "refuses (verdict)"        "$R"   "true"
check "job still exits 0"        "$RC"  "0"

echo "== Fixture 6: deno.json still trips the BAN probe =="
run_fixture true '' deno.json
check "has_deno true"            "$(grep -qF 'has_deno=true' <<< "$OUT" && echo y || echo n)" "y"
check "n_deno"                   "$(sed -n 's/^n_deno=//p' <<< "$OUT" | tail -1)" "1"
check "counts toward total"      "$T"   "1"

echo "== MUTANT: delete the Julia probe. Fixture 3 must change its answer. =="
echo "   A suite that only ever goes green proves nothing."
run_fixture true "/probe 'Julia'/d" Project.toml
check "mutant: unsupported now 0"   "$U"  "0"
check "mutant: falls back to the UNRECOGNISED message" \
      "$(grep -qF 'No known ecosystem detected' <<< "$OUT" && echo y || echo n)" "y"
check "mutant: no longer names Julia" \
      "$(grep -qF 'Julia' <<< "$OUT" && echo y || echo n)" "n"

echo "== MUTANT 2: fold unsupported into TOTAL. The refusal must go SILENT. =="
echo "   This is the vacuous gate this design exists to prevent; if the"
echo "   mutant still refuses, the separation is not what makes it refuse."
# Folded AFTER the probes run, which is the only place the fold could
# plausibly be written by someone "tidying up" the two accumulators into one.
run_fixture true 's|^bool() {|TOTAL=$(( TOTAL + N_UNSUPPORTED ))\nbool() {|' Project.toml
check "mutant2: total wrongly non-zero" "$T"  "1"
check "mutant2: refusal SILENCED"       "$R"  "false"

echo ""
echo "detect-ecosystem-test: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
