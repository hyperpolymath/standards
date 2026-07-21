#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail

# Regression test for check-workflow-staleness.sh (recency-window model).
#
# Builds a hermetic fixture "standards" repo with a chain of commits at
# controlled ages, then points the gate at it via STALENESS_STANDARDS_DIR and
# checks that each pin classification (fresh / ahead / in-window-by-commits /
# in-window-by-days / out-of-window / forged) yields the expected exit code,
# alongside the structural rules (retired enforcer, SARIF code-scanning).

TEST_DIR=$(mktemp -d)
trap 'rm -rf "$TEST_DIR"' EXIT

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECK_SCRIPT="$SCRIPT_DIR/../check-workflow-staleness.sh"

# ── Build the fixture standards history ─────────────────────────────────────
FIX="$TEST_DIR/standards-fixture"
mkdir -p "$FIX"
git -C "$FIX" init -q
git -C "$FIX" config user.email "test@example.com"
git -C "$FIX" config user.name "Test"

commit_at() { # repo days_ago msg
  local repo="$1" days_ago="$2" msg="$3" ts
  ts=$(( $(date -u +%s) - days_ago * 86400 ))
  GIT_AUTHOR_DATE="@$ts +0000" GIT_COMMITTER_DATE="@$ts +0000" \
    git -C "$repo" commit --allow-empty -q -m "$msg"
  git -C "$repo" rev-parse HEAD
}

C0=$(commit_at "$FIX" 400 "C0 ancient")
C1=$(commit_at "$FIX" 300 "C1")
# shellcheck disable=SC2034
C2=$(commit_at "$FIX" 20 "C2")
C3=$(commit_at "$FIX" 10 "C3")
C4=$(commit_at "$FIX" 2 "C4")
C5=$(commit_at "$FIX" 0 "C5 head")   # HEAD

FORGED="deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"

mk_repo() { mkdir -p "$1/.github/workflows"; }

# Write a consumer workflow that pins one standards reusable at $sha.
write_pin() { # file reusable sha
  cat > "$1" <<EOF
name: Consumer
on: push
jobs:
  job:
    uses: hyperpolymath/standards/.github/workflows/$2@$3
EOF
}

PASS=0
TOTAL=0

# run_case desc expected_exit repo [VAR=VAL ...]
run_case() {
  local desc="$1" expected="$2" repo="$3"; shift 3
  TOTAL=$((TOTAL + 1))
  set +e
  env "$@" \
    GITHUB_REPOSITORY="hyperpolymath/test-repo" \
    STALENESS_STANDARDS_DIR="$FIX" \
    bash "$CHECK_SCRIPT" "$repo" >/dev/null 2>&1
  local rc=$?
  set -e
  if [ "$rc" -eq "$expected" ]; then
    echo "PASS: $desc"
    PASS=$((PASS + 1))
  else
    echo "FAIL: $desc (expected exit $expected, got $rc)"
  fi
}

# run_case_out desc expected_exit expected_output_regex repo [VAR=VAL ...]
# As run_case, but also asserts the gate said the right thing — an exit code
# alone cannot tell "passed silently" from "passed with the advisory notice
# the propagation path depends on".
run_case_out() {
  local desc="$1" expected="$2" regex="$3" repo="$4"; shift 4
  TOTAL=$((TOTAL + 1))
  local out rc
  set +e
  out=$(env "$@" \
    GITHUB_REPOSITORY="hyperpolymath/test-repo" \
    STALENESS_STANDARDS_DIR="$FIX" \
    bash "$CHECK_SCRIPT" "$repo" 2>&1)
  rc=$?
  set -e
  if [ "$rc" -eq "$expected" ] && printf '%s' "$out" | grep -qE "$regex"; then
    echo "PASS: $desc"
    PASS=$((PASS + 1))
  else
    echo "FAIL: $desc (expected exit $expected + /$regex/, got exit $rc)"
  fi
}

# ── 1. FRESH: pin == HEAD ───────────────────────────────────────────────────
R="$TEST_DIR/fresh"; mk_repo "$R"
write_pin "$R/.github/workflows/governance.yml" "governance-reusable.yml" "$C5"
run_case "fresh pin (== HEAD) passes" 0 "$R"

# ── 2. IN_WINDOW recent (default window) ────────────────────────────────────
R="$TEST_DIR/recent"; mk_repo "$R"
write_pin "$R/.github/workflows/governance.yml" "governance-reusable.yml" "$C4"
run_case "recent pin (2d, 1 behind) passes under default window" 0 "$R"

# ── 3. IN_WINDOW via the DAYS axis (commits axis disabled) ──────────────────
R="$TEST_DIR/days"; mk_repo "$R"
write_pin "$R/.github/workflows/hypatia.yml" "hypatia-scan-reusable.yml" "$C3"
run_case "10d/2-behind pin passes on days axis (window 1 commit / 14 days)" 0 "$R" \
  STALENESS_WINDOW_COMMITS=1 STALENESS_WINDOW_DAYS=14

# ── 4. IN_WINDOW via the COMMITS axis (days axis disabled) ──────────────────
R="$TEST_DIR/commits"; mk_repo "$R"
write_pin "$R/.github/workflows/scorecard.yml" "scorecard-reusable.yml" "$C1"
run_case "300d/4-behind pin passes on commits axis (window 5 commits / 1 day)" 0 "$R" \
  STALENESS_WINDOW_COMMITS=5 STALENESS_WINDOW_DAYS=1

# ── 5. OUT_OF_WINDOW: outside both axes -> ADVISORY, not a failure ──────────
# Contract change (2026-07-21): age alone no longer fails. Making it fail turned
# the gate into a clock that reddened the whole fleet ~14 days after every
# propagation (294 of 350 consumers, one pin, nothing wrong with any of them).
# The pin must still be *reported* so the propagation path has something to act
# on — hence the output assertion, not just the exit code.
R="$TEST_DIR/stale"; mk_repo "$R"
write_pin "$R/.github/workflows/governance.yml" "governance-reusable.yml" "$C0"
run_case_out "ancient pin (400d, 5 behind) passes with an advisory notice" 0 \
  '::notice.*outside the recency window' "$R" \
  STALENESS_WINDOW_COMMITS=2 STALENESS_WINDOW_DAYS=14

# ── 5b. KNOWN-BAD: pin predates a named defect fix -> hard fail at any age ───
# The deny-list is what the window was only ever proxying for. Strictly better
# in both directions: it catches this, and it does not punish 5 above.
R="$TEST_DIR/knownbad"; mk_repo "$R"
write_pin "$R/.github/workflows/governance.yml" "governance-reusable.yml" "$C1"
run_case_out "pin predating a known-bad fix fails regardless of window" 1 \
  'predates.*carries the' "$R" \
  STALENESS_WINDOW_COMMITS=9999 STALENESS_WINDOW_DAYS=9999 \
  STALENESS_KNOWN_BAD_BEFORE="governance-reusable.yml:$C3"

# ── 5c. KNOWN-BAD outranks freshness: an IN-WINDOW pin is still rejected ─────
R="$TEST_DIR/knownbad-fresh"; mk_repo "$R"
write_pin "$R/.github/workflows/governance.yml" "governance-reusable.yml" "$C4"
run_case "in-window pin carrying a known defect still fails" 1 "$R" \
  STALENESS_KNOWN_BAD_BEFORE="governance-reusable.yml:$C5"

# ── 5d. The fix commit itself is NOT denied (boundary is strict) ─────────────
R="$TEST_DIR/knownbad-boundary"; mk_repo "$R"
write_pin "$R/.github/workflows/governance.yml" "governance-reusable.yml" "$C3"
run_case "the fixing commit itself is not denied" 0 "$R" \
  STALENESS_WINDOW_COMMITS=9999 STALENESS_WINDOW_DAYS=9999 \
  STALENESS_KNOWN_BAD_BEFORE="governance-reusable.yml:$C3"

# ── 5e. The deny-list is scoped per reusable, not applied globally ───────────
R="$TEST_DIR/knownbad-scope"; mk_repo "$R"
write_pin "$R/.github/workflows/scorecard.yml" "scorecard-reusable.yml" "$C1"
run_case "a deny-list entry for another reusable does not fail this one" 0 "$R" \
  STALENESS_WINDOW_COMMITS=9999 STALENESS_WINDOW_DAYS=9999 \
  STALENESS_KNOWN_BAD_BEFORE="governance-reusable.yml:$C5"

# ── 6. UNKNOWN / forged SHA -> fail ─────────────────────────────────────────
R="$TEST_DIR/forged"; mk_repo "$R"
write_pin "$R/.github/workflows/governance.yml" "governance-reusable.yml" "$FORGED"
run_case "forged pin (not a standards commit) fails" 1 "$R"

# ── 7. AHEAD: consumer pins a descendant of the gate's HEAD view -> pass ────
R="$TEST_DIR/ahead"; mk_repo "$R"
write_pin "$R/.github/workflows/governance.yml" "governance-reusable.yml" "$C5"
run_case "consumer ahead of gate HEAD passes" 0 "$R" STALENESS_EXPECTED_SHA="$C4"

# ── 8. Retired scorecard-enforcer.yml in a consumer -> fail ─────────────────
R="$TEST_DIR/enforcer"; mk_repo "$R"
touch "$R/.github/workflows/scorecard-enforcer.yml"
run_case "retired scorecard-enforcer.yml fails" 1 "$R"

# ── 9. Direct Scorecard SARIF upload -> fail ────────────────────────────────
R="$TEST_DIR/sarif"; mk_repo "$R"
cat > "$R/.github/workflows/scorecard.yml" <<'EOF'
name: Scorecard
on: push
jobs:
  scorecard:
    steps:
      - uses: ossf/scorecard-action@abc
      - uses: github/codeql-action/upload-sarif@xyz
EOF
run_case "direct Scorecard SARIF upload fails" 1 "$R"

# ── 10. No workflows dir -> pass ────────────────────────────────────────────
R="$TEST_DIR/empty"; mkdir -p "$R"
run_case "no .github/workflows passes" 0 "$R"

# ── 11. Commented-out pin is ignored (ancient SHA, but in a comment) ────────
R="$TEST_DIR/commented"; mk_repo "$R"
cat > "$R/.github/workflows/governance.yml" <<EOF
name: Consumer
on: push
jobs:
  job:
    # example: uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@$C0
    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@$C5
EOF
run_case "commented-out ancient pin is ignored; active fresh pin passes" 0 "$R"

# ── 12. Clean multi-reusable repo (all three at a recent SHA) -> pass ───────
R="$TEST_DIR/multi"; mk_repo "$R"
write_pin "$R/.github/workflows/governance.yml" "governance-reusable.yml" "$C4"
write_pin "$R/.github/workflows/hypatia.yml" "hypatia-scan-reusable.yml" "$C4"
write_pin "$R/.github/workflows/scorecard.yml" "scorecard-reusable.yml" "$C4"
run_case "clean multi-reusable repo passes" 0 "$R"

echo "----------------------------------------"
echo "$PASS/$TOTAL test cases passed."
[ "$PASS" -eq "$TOTAL" ] || { echo "Some regression tests FAILED."; exit 1; }
echo "All regression tests passed!"
exit 0
