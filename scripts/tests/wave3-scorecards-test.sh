#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -uo pipefail
#
# Wave-3 scorecard/dashboard regression test.
#
# The generator must be honest and deterministic:
#   * a `pass` without evidence is rejected,
#   * an orphan scorecard (spec_id not in the registry) is rejected,
#   * `aspirational` never counts as a MUST pass,
#   * regeneration is idempotent, and --check detects drift.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
GEN="$ROOT/scripts/build-scorecards.sh"
SCDIR="$ROOT/.machine_readable/scorecards"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"; rm -f "$SCDIR"/zzz-*.scorecard.a2ml' EXIT

pass=0 fail=0
ok()  { echo "  ✅ $1"; pass=$((pass + 1)); }
bad() { echo "  ❌ $1"; fail=$((fail + 1)); }

echo "== generator honesty =="
# pass without evidence -> rejected
cat > "$SCDIR/zzz-a.scorecard.a2ml" <<'EOF'
[scorecard]
spec_id = "a2ml"
version = "1.0.0"
assessed_date = "2026-07-03"
assessor = "x"

[[must]]
id = "M1"
text = "x"
system = "none"
status = "pass"
effects = "y"
EOF
if bash "$GEN" >/dev/null 2>&1; then bad "pass-without-evidence was NOT rejected"; else ok "pass-without-evidence rejected"; fi
rm -f "$SCDIR/zzz-a.scorecard.a2ml"

# orphan spec_id -> rejected
cat > "$SCDIR/zzz-orphan.scorecard.a2ml" <<'EOF'
[scorecard]
spec_id = "definitely-not-a-registered-spec"
version = "1.0.0"
assessed_date = "2026-07-03"
assessor = "x"

[[must]]
id = "M1"
text = "x"
system = "none"
status = "fail"
effects = "y"
EOF
if bash "$GEN" >/dev/null 2>&1; then bad "orphan scorecard was NOT rejected"; else ok "orphan scorecard rejected"; fi
rm -f "$SCDIR/zzz-orphan.scorecard.a2ml"

echo "== determinism + drift =="
# regenerate twice -> identical
bash "$GEN" >/dev/null 2>&1
h1="$(sha256sum "$ROOT/COMPLIANCE-DASHBOARD.md" | cut -d' ' -f1)"
bash "$GEN" >/dev/null 2>&1
h2="$(sha256sum "$ROOT/COMPLIANCE-DASHBOARD.md" | cut -d' ' -f1)"
[ "$h1" = "$h2" ] && ok "regeneration is deterministic" || bad "regeneration not deterministic"
# --check green when in sync
bash "$GEN" --check >/dev/null 2>&1 && ok "--check passes when in sync" || bad "--check failed when in sync"
# --check red when dashboard mutated
printf '\n<!-- drift -->\n' >> "$ROOT/COMPLIANCE-DASHBOARD.md"
if bash "$GEN" --check >/dev/null 2>&1; then bad "--check missed injected drift"; else ok "--check detects injected drift"; fi
bash "$GEN" >/dev/null 2>&1  # restore

echo "== --verify (executable grounding; Wave 7) =="
# pick any real scorecard to mutate, restore after
TGT="$(ls "$SCDIR"/*.scorecard.a2ml | head -1)"
cp "$TGT" "$TMP/orig.a2ml"
# (a) a pass row with a FAILING check must fail --verify (broken pass).
# Pass rows are now fully grounded (each already carries a check), so REPLACE
# every existing check with `false` rather than inserting a duplicate line
# (the parser takes the last check line in a block).
sed 's/^check = ".*"$/check = "false"/' "$TMP/orig.a2ml" > "$TGT"
if bash "$GEN" --verify >/dev/null 2>&1; then bad "--verify missed a broken pass-check"; else ok "--verify fails on a broken pass-check"; fi
cp "$TMP/orig.a2ml" "$TGT"
# (b) pass rows with HOLDING checks keep --verify green (replace all with `true`)
sed 's/^check = ".*"$/check = "true"/' "$TMP/orig.a2ml" > "$TGT"
if bash "$GEN" --verify >/dev/null 2>&1; then ok "--verify green with holding pass-checks"; else bad "--verify failed on holding checks"; fi
# (c) grounded count appears in verify output (capture first: `grep -q` closing
# the pipe early can SIGPIPE the still-writing generator under pipefail)
cout="$(bash "$GEN" --verify 2>&1 || true)"
if grep -qE '[1-9][0-9]* grounded pass' <<< "$cout"; then ok "grounded passes counted"; else bad "grounded count missing"; fi
cp "$TMP/orig.a2ml" "$TGT"
# (d) a FAIL row whose check passes is a stale-fail ADVISORY (not fatal)
awk '1; /^status = "fail"$/ && !done {print "check = \"true\""; done=1}' "$TMP/orig.a2ml" > "$TGT"
vout="$(bash "$GEN" --verify 2>&1)"; vrc=$?
if [ "$vrc" -eq 0 ] && grep -q 'stale-fail' <<< "$vout"; then ok "stale-fail is advisory, reported, non-fatal"; else
  # some scorecards may have no fail rows; treat absence of any fail row as skip-ok
  grep -q '^status = "fail"$' "$TMP/orig.a2ml" && bad "stale-fail not handled (rc=$vrc)" || ok "no fail rows in fixture (skip)"
fi
cp "$TMP/orig.a2ml" "$TGT"
bash "$GEN" >/dev/null 2>&1  # restore dashboard

echo
echo "Wave-3 scorecard regression: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
