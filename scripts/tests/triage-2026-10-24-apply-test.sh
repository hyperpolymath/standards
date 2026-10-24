#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# triage-2026-10-24-apply-test.sh — regression tests for
# scripts/triage-2026-10-24-apply.sh and the 2026-10-24 backlog classification.
#
# The script closes GitHub issues, so every test runs against a `gh` shim on
# PATH: nothing here touches the network, and a test failure cannot close an
# issue. What is asserted is exactly the contract the document claims:
#   * the CSV is complete (one row per open issue, unique, valid tiers);
#   * --report needs no network;
#   * dry-run writes nothing;
#   * --apply comments then closes every tier-C issue, and nothing else;
#   * an already-closed issue is skipped rather than re-commented.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
SUT="$SCRIPT_DIR/../triage-2026-10-24-apply.sh"
CSV="$ROOT/docs/triage/2026-10-24-issue-classification.csv"
DOC="$ROOT/docs/triage/2026-10-24-issue-classification.adoc"

fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
pass() { printf 'PASS: %s\n' "$*"; }

# 1. The subject exists and parses.
[ -f "$SUT" ] || fail "$SUT does not exist"
bash -n "$SUT" || fail "$SUT does not parse"
pass "script exists and parses"

# 2. The classification artifacts exist and agree with each other.
[ -f "$CSV" ] || fail "$CSV does not exist"
[ -f "$DOC" ] || fail "$DOC does not exist"

python3 - "$CSV" "$DOC" <<'PY' || exit 1
import csv, sys, collections
rows = list(csv.DictReader(open(sys.argv[1])))
assert rows, "CSV has no rows"
nums = [r["number"] for r in rows]
assert len(nums) == len(set(nums)), "duplicate issue numbers in CSV"
tiers = collections.Counter(r["tier"] for r in rows)
assert set(tiers) <= set("CQSEDRU"), f"unknown tier codes: {set(tiers) - set('CQSEDRU')}"
notes = [r for r in rows if not r["note"].strip()]
assert not notes, f"{len(notes)} rows carry no note"
doc = open(sys.argv[2]).read()
for t in "CQSEDRU":
    assert f"== Tier {t} " in doc, f"document has no Tier {t} section"
total = len(rows)
assert f"{total}" in doc, "document does not state the total"
print(f"PASS: {total} classified issues ({', '.join(f'{t}={tiers[t]}' for t in 'CQSEDRU')}), "
      "unique numbers, every row annotated, every tier has a section")
PY

# 3. --report must not need the network at all: run it with an empty PATH for gh.
REPORT_OUT="$(PATH="/usr/bin:/bin" bash "$SUT" --report)" || fail "--report failed without gh on PATH"
printf '%s' "$REPORT_OUT" | grep -q 'total' || fail "--report printed no total"
printf '%s' "$REPORT_OUT" | grep -qE 'C +close now' || fail "--report did not summarise tier C"
pass "--report works with no gh on PATH"

# 4. Everything below uses a shim gh.
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
CALLS="$WORK/calls.log"
: > "$CALLS"

cat > "$WORK/gh" <<'SHIM'
#!/usr/bin/env bash
# Records every invocation, then behaves just enough like gh for the SUT.
printf '%s\n' "$*" >> "$SHIM_CALLS"
case "${1:-} ${2:-}" in
  "issue view") printf '%s\n' "${SHIM_STATE:-OPEN}" ;;
  "issue comment") : ;;
  "issue close") : ;;
  *) printf 'unexpected gh invocation: %s\n' "$*" >&2; exit 1 ;;
esac
SHIM
chmod +x "$WORK/gh"

run_sut() { SHIM_CALLS="$CALLS" PATH="$WORK:$PATH" bash "$SUT" "$@"; }

C_TIER="$(python3 -c "
import csv,sys
print(len([r for r in csv.DictReader(open('$CSV')) if r['tier']=='C']))")"
[ "$C_TIER" -gt 0 ] || fail "tier C is empty, nothing to apply"

# 4a. Dry-run: reads state, writes nothing.
run_sut > /dev/null || fail "dry-run failed"
grep -q 'issue view' "$CALLS" || fail "dry-run did not read issue state"
if grep -qE '^issue (close|comment)' "$CALLS"; then
  fail "dry-run issued a write ($(grep -cE '^issue (close|comment)' "$CALLS") writes)"
fi
pass "dry-run reads state and writes nothing"

# 4b. Apply: one comment and one close per tier-C issue, and nothing else.
: > "$CALLS"
run_sut --apply > /dev/null || fail "--apply failed"
closes="$(grep -cE '^issue close ' "$CALLS" || true)"
comments="$(grep -cE '^issue comment ' "$CALLS" || true)"
[ "$closes" = "$C_TIER" ] || fail "--apply closed $closes issues, expected $C_TIER"
[ "$comments" = "$C_TIER" ] || fail "--apply commented $comments times, expected $C_TIER"

# Every close names an issue number; every one of them is a tier-C number.
python3 - "$CSV" "$CALLS" <<'PY' || exit 1
import csv, sys, re
tier_c = {r["number"]: r["note"] for r in csv.DictReader(open(sys.argv[1])) if r["tier"] == "C"}
closed = []
commented = []
for line in open(sys.argv[2]):
    parts = line.split()
    if parts[:2] == ["issue", "close"]:
        closed.append(parts[2])
    elif parts[:2] == ["issue", "comment"]:
        commented.append(parts[2])
bad = [n for n in closed if n not in tier_c]
assert not bad, f"--apply closed non-tier-C issues: {bad}"
assert sorted(closed) == sorted(commented), "a close was not preceded by a comment"
assert len(closed) == len(set(closed)), "an issue was closed twice"
print(f"PASS: --apply commented and closed exactly the {len(closed)} tier-C issues, nothing else")
PY

# 4c. Idempotence: an already-closed issue is skipped, never re-commented.
: > "$CALLS"
SHIM_STATE=CLOSED run_sut --apply > /dev/null || fail "--apply failed when issues were already closed"
if grep -qE '^issue (close|comment)' "$CALLS"; then
  fail "already-closed issues were written to again"
fi
pass "already-closed issues are skipped"

printf '\nall triage-2026-10-24 assertions passed\n'
