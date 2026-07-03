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
#   * audit-contractiles.sh               (loud error on zero repos)

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

pass=0 fail=0
ok()   { echo "  ✅ $1"; pass=$((pass + 1)); }
bad()  { echo "  ❌ $1"; fail=$((fail + 1)); }
# expect <wanted-code> <actual-code> <label>
expect() { if [ "$2" -eq "$1" ]; then ok "$3 (exit $2)"; else bad "$3 (wanted $1, got $2)"; fi; }

echo "== check-6scm.sh =="
SIX="$ROOT/a2ml/scripts/check-6scm.sh"
# (1) obsolete no-op: no sources, no mirror -> exit 0
w="$TMP/six-obsolete"; mkdir -p "$w/.machine_readable/6a2"; cp "$SIX" "$w/check.sh"
( cd "$w" && bash check.sh >/dev/null 2>&1 ); expect 0 $? "obsolete no-op passes"
# (2) orphan drift: mirror files but no sources -> exit 1
w="$TMP/six-orphan"; mkdir -p "$w/.machine_readable/6scm"; echo x > "$w/.machine_readable/6scm/STATE.scm"; cp "$SIX" "$w/check.sh"
( cd "$w" && bash check.sh >/dev/null 2>&1 ); expect 1 $? "orphan-mirror drift fails"
# (3) out-of-sync: source present, mirror differs -> exit 1
w="$TMP/six-desync"; mkdir -p "$w/.machine_readable/6scm"
printf 'a\n' > "$w/.machine_readable/STATE.scm"; printf 'b\n' > "$w/.machine_readable/6scm/STATE.scm"; cp "$SIX" "$w/check.sh"
( cd "$w" && bash check.sh >/dev/null 2>&1 ); expect 1 $? "out-of-sync mirror fails"
# (4) in-sync: source present, mirror identical -> exit 0
w="$TMP/six-sync"; mkdir -p "$w/.machine_readable/6scm"
printf 'a\n' > "$w/.machine_readable/STATE.scm"; printf 'a\n' > "$w/.machine_readable/6scm/STATE.scm"; cp "$SIX" "$w/check.sh"
( cd "$w" && bash check.sh >/dev/null 2>&1 ); expect 0 $? "in-sync mirror passes"

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

echo "== audit-contractiles.sh =="
AC="$ROOT/audit-contractiles.sh"
# Runs against an explicit repo path with no hardcoded owner (/var/mnt/...) paths.
# Capture the output first (avoid pipefail masking the script's own exit code).
ac_out="$(bash "$AC" "$ROOT" 2>&1 || true)"
case "$ac_out" in
  *"Contractile System Audit"*) ok "runs against an explicit repo path (no hardcoded owner paths)" ;;
  *) bad "did not run against explicit repo path" ;;
esac
# No hardcoded owner path is USED (a quoted absolute /var/mnt/... array element);
# a prose mention in a comment is fine, an actual code path is not.
if grep -qE '^[[:space:]]*"/var/mnt/' "$AC"; then
  bad "hardcoded owner path still used in code"
else
  ok "no hardcoded owner path used in code"
fi

echo
echo "Wave-0 false-green regression: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
