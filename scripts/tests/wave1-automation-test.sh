#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -uo pipefail
#
# Wave-1 "make the automation actually run" regression test.
#
# Covers the runners that Wave 1 wired into CI/hooks so they can no longer be
# declared-but-never-run:
#   * scripts/run-mustfile.sh  — executes the Mustfile invariants (real gate)
#   * hooks/install.sh         — installs the pre-commit guard into .git/hooks

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

pass=0 fail=0
ok()  { echo "  ✅ $1"; pass=$((pass + 1)); }
bad() { echo "  ❌ $1"; fail=$((fail + 1)); }
expect() { if [ "$2" -eq "$1" ]; then ok "$3 (exit $2)"; else bad "$3 (wanted $1, got $2)"; fi; }

echo "== run-mustfile.sh =="
RM="$ROOT/scripts/run-mustfile.sh"
# The repo's real Mustfile must pass (all critical/high green) — this is the
# ground truth that lets the CI job block honestly.
bash "$RM" "$ROOT/.machine_readable/contractiles/must/Mustfile.a2ml" >/dev/null 2>&1
expect 0 $? "real Mustfile passes enforcement"
# A failing critical check blocks.
cf="$TMP/crit.a2ml"; printf '### x\n- run: test -f /nonexistent-xyzzy\n- severity: critical\n' > "$cf"
bash "$RM" "$cf" >/dev/null 2>&1; expect 1 $? "failing critical check blocks"
# A failing high check blocks.
hf="$TMP/high.a2ml"; printf '### x\n- run: test -f /nonexistent-xyzzy\n- severity: high\n' > "$hf"
bash "$RM" "$hf" >/dev/null 2>&1; expect 1 $? "failing high check blocks"
# A failing warning check is advisory (non-blocking).
wf="$TMP/warn.a2ml"; printf '### x\n- run: test -f /nonexistent-xyzzy\n- severity: warning\n' > "$wf"
bash "$RM" "$wf" >/dev/null 2>&1; expect 0 $? "failing warning check is non-blocking"
# Missing file errors.
bash "$RM" "$TMP/nope.a2ml" >/dev/null 2>&1; expect 2 $? "missing Mustfile errors"

echo "== hooks/install.sh =="
HR="$TMP/hookrepo"; mkdir -p "$HR/hooks"; git -C "$HR" init -q
cp "$ROOT/hooks/pre-commit" "$HR/hooks/"; cp "$ROOT/hooks/install.sh" "$HR/hooks/"
( cd "$HR" && bash hooks/install.sh >/dev/null 2>&1 ); expect 0 $? "installer runs"
if [ -x "$HR/.git/hooks/pre-commit" ]; then ok "pre-commit installed + executable"; else bad "pre-commit not installed"; fi
if grep -q 'exec .*hooks/pre-commit' "$HR/.git/hooks/pre-commit" 2>/dev/null; then
  ok "installed hook execs the tracked work-tree copy (single source of truth)"
else
  bad "installed hook does not delegate to tracked copy"
fi
# Idempotent: re-running is safe.
( cd "$HR" && bash hooks/install.sh >/dev/null 2>&1 ); expect 0 $? "installer is idempotent"

echo
echo "Wave-1 automation regression: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
