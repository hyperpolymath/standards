#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -uo pipefail
#
# Wave-6 regression: the canonical-names reintroduction guard must block a NEW
# deprecated token while leaving grandfathered existing occurrences alone.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CHK="$ROOT/scripts/check-canonical-names.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

pass=0 fail=0
ok()  { echo "  ✅ $1"; pass=$((pass + 1)); }
bad() { echo "  ❌ $1"; fail=$((fail + 1)); }

# Exercise the guard in a private repository so unrelated changes in the caller's
# worktree cannot turn the clean-name case into a false failure.
mkdir -p "$TMP/repo/scripts"
cp "$CHK" "$TMP/repo/scripts/check-canonical-names.sh"
cd "$TMP/repo" || exit
git init -q
git config user.name "Standards regression test"
git config user.email "standards-regression@example.invalid"
git add scripts/check-canonical-names.sh
git commit -qm "baseline"
CHK="$TMP/repo/scripts/check-canonical-names.sh"

echo "== guard blocks a newly-added deprecated token =="
f="wave6_guard_probe.txt"
printf 'this file uses the 6a2 layout\n' > "$f"
git add "$f" 2>/dev/null
if bash "$CHK" HEAD >/dev/null 2>&1; then bad "new '6a2' token not blocked"; else ok "new '6a2' token blocked"; fi
# agent_instructions too
printf 'agent_instructions live here\n' > "$f"
git add "$f" 2>/dev/null
if bash "$CHK" HEAD >/dev/null 2>&1; then bad "new 'agent_instructions' not blocked"; else ok "new 'agent_instructions' blocked"; fi
git reset -q "$f" 2>/dev/null; rm -f "$f"

echo "== guard blocks a newly-added deprecated directory =="
legacy_dir=".machine_readable/6a2"
mkdir -p "$legacy_dir"
printf '[metadata]\nstatus = "probe"\n' > "$legacy_dir/STATE.a2ml"
git add "$legacy_dir/STATE.a2ml" 2>/dev/null
if bash "$CHK" HEAD >/dev/null 2>&1; then bad "new deprecated directory not blocked"; else ok "new deprecated directory blocked"; fi
git reset -q "$legacy_dir/STATE.a2ml" 2>/dev/null
rm -rf "$legacy_dir"

echo "== guard passes with no offending additions =="
printf 'a perfectly canonical descriptiles + bot_directives line\n' > "$f"
git add "$f" 2>/dev/null
bash "$CHK" HEAD >/dev/null 2>&1 && ok "canonical names pass" || bad "canonical names wrongly blocked"
git reset -q "$f" 2>/dev/null; rm -f "$f"

echo "== the guard excludes CANONICAL-NAMES.adoc itself =="
grep -q 'CANONICAL-NAMES.adoc' "$CHK" && ok "mandate doc is excluded from the guard" || bad "mandate doc not excluded"

echo
echo "Wave-6 canonical-names regression: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
