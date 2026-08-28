#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -uo pipefail
#
# Wave-0 "kill the false green" regression test.
#
# Every validator fixed in Wave 0 must be able to FAIL — a check that cannot
# fail is not a check. This exercises each one's success AND failure path so a
# future regression that reintroduces a vacuous pass is caught here.
#
# Covers:
#   * a2ml/scripts/check-6scm.sh          (obsolete no-op / orphan drift / out-of-sync)
#   * scripts/check-mustfile-structure.sh (valid Mustfile / hollow check)
#   * rhodium-standard-repositories/rsr-audit.sh (bad format exits 4 / --format json works)
#   * audit-contractiles.sh               (retired; Hypatia owns this audit)

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

pass=0 fail=0
ok()   { echo "  ✅ $1"; pass=$((pass + 1)); }
bad()  { echo "  ❌ $1"; fail=$((fail + 1)); }
# expect <wanted-code> <actual-code> <label>
expect() { if [ "$2" -eq "$1" ]; then ok "$3 (exit $2)"; else bad "$3 (wanted $1, got $2)"; fi; }

# NOTE: the check-6scm.sh section was removed on 2026-08-28 when a2ml/ was
# evicted to hyperpolymath/a2ml (standards#490). The script it exercised now
# lives there, so these four cases belong in that repo's test suite, not this
# one. Leaving them here would make a REQUIRED check fail for a file this
# repository no longer contains.

echo "== check-mustfile-structure.sh =="
MS="$ROOT/scripts/check-mustfile-structure.sh"
# valid: the repo's real Mustfile -> exit 0
bash "$MS" "$ROOT/.machine_readable/contractiles/must/Mustfile.a2ml" >/dev/null 2>&1
expect 0 $? "real Mustfile is structurally valid"
# hollow: a check with neither run nor verification -> exit 1
hf="$TMP/hollow.a2ml"; printf '### hollow\n- description: nothing runnable\n- severity: high\n' > "$hf"
bash "$MS" "$hf" >/dev/null 2>&1; expect 1 $? "hollow check fails"
# governance: a check with verification (no run) is accepted -> exit 0
gf="$TMP/gov.a2ml"; printf '### gov\n- description: manual\n- verification: reviewed by owner\n- severity: high\n' > "$gf"
bash "$MS" "$gf" >/dev/null 2>&1; expect 0 $? "verification-only check accepted"
# missing file -> exit 2
bash "$MS" "$TMP/nope.a2ml" >/dev/null 2>&1; expect 2 $? "missing Mustfile errors"

echo "== rsr-audit.sh (standards#387 arg parsing) =="
RSR="$ROOT/rhodium-standard-repositories/rsr-audit.sh"
# bad format -> exit 4 (no longer silently defaults to text)
bash "$RSR" . --format xml >/dev/null 2>&1; expect 4 $? "invalid --format errors loudly"
# documented --format json now produces JSON. Capture first: rsr-audit's exit
# code encodes the grade (non-zero for < Gold), so piping into grep under
# `pipefail` would mask a successful match — assert on the captured text.
json_out="$(bash "$RSR" . --format json 2>/dev/null || true)"
case "$json_out" in
  *'"compliance_level"'*) ok "--format json emits JSON report" ;;
  *) bad "--format json did not emit JSON" ;;
esac
# bare positional 'text' (Justfile backward-compat) still works: grade code 0-3
bash "$RSR" . text >/dev/null 2>&1; rc=$?
if [ "$rc" -ge 0 ] && [ "$rc" -le 3 ]; then ok "bare positional 'text' returns a grade code ($rc)"; else bad "bare positional 'text' returned $rc"; fi

echo "== retired audit-contractiles.sh =="
if [ -e "$ROOT/audit-contractiles.sh" ]; then
  bad "retired personal-machine validator was reintroduced"
else
  ok "retired personal-machine validator remains absent"
fi

echo
echo "Wave-0 false-green regression: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
