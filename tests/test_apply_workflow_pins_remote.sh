#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# test_apply_workflow_pins_remote.sh — regression suite for the pin applier.
#
# This suite is MUTATION-BASED on purpose. A green run of the applier's own
# controls proves only that the controls agree with the code; it does not prove
# the controls can DETECT anything. So every mutant below reintroduces a real
# defect verbatim and asserts the suite turns red. A mutant that stays green is
# a control that was never testing what its name claims.
#
# Trap already paid for once: a syntactically INVALID mutant fails for the wrong
# reason and every control "fails" on a parse error, which reads as success.
# Each mutant is therefore `bash -n`-checked BEFORE its redness is believed.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APPLIER="${SCRIPT_DIR}/../scripts/apply-workflow-pins-remote.sh"

TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
rc=0
pass() { echo "  PASS $*"; }
fail() { echo "  FAIL $*" >&2; rc=1; }

# --- 0. the script must exist, parse, and be committed executable ------------
echo "== 0. shape =="
[ -f "$APPLIER" ] || { echo "FATAL: applier not found at $APPLIER" >&2; exit 1; }
bash -n "$APPLIER" && pass "applier parses" || fail "applier does not parse"

# A suite committed 100644 passes every local run (`bash script` ignores the
# mode) and dies in CI at exit 126 before a single control runs.
if git -C "${SCRIPT_DIR}/.." ls-files -s -- tests/test_apply_workflow_pins_remote.sh 2>/dev/null | grep -q '^100755'; then
  pass "this test is committed executable (100755)"
elif ! git -C "${SCRIPT_DIR}/.." rev-parse --git-dir >/dev/null 2>&1; then
  pass "not a git checkout; mode check skipped"
else
  # Not yet staged is acceptable while authoring; a wrong mode is not.
  if git -C "${SCRIPT_DIR}/.." ls-files -- tests/test_apply_workflow_pins_remote.sh 2>/dev/null | grep -q .; then
    fail "test is tracked but NOT 100755 — it will exit 126 in CI"
  else
    pass "test not yet tracked; mode will be checked once added"
  fi
fi
if git -C "${SCRIPT_DIR}/.." ls-files -s -- scripts/apply-workflow-pins-remote.sh 2>/dev/null | grep -q '^100644'; then
  fail "applier is tracked 100644 — it must be executable"
else
  pass "applier mode acceptable"
fi

# --- 1. the applier's own controls must pass unmutated -----------------------
echo "== 1. baseline: unmutated controls =="
if bash "$APPLIER" --self-test >"$TMP/base.out" 2>&1; then
  pass "baseline controls green"
else
  fail "baseline controls RED — fix the applier before reading any mutant"
  sed 's/^/    /' "$TMP/base.out" >&2
fi
for want in "fresh.yml" "behind.yml" "illegal.yml" "tracking.yml" "none.yml" "short.yml"; do
  grep -q "PASS $want" "$TMP/base.out" && pass "control present: $want" \
    || fail "control MISSING from baseline: $want (a control that never runs is not a control)"
done

# --- 2. mutants --------------------------------------------------------------
# kill <name> <sed-expr> <expected-substring-in-red-output>
kill_mutant() {
  # NOTE: `local a="$1" m="${a}"` does NOT work — bash expands every word of the
  # `local` builtin's argument list BEFORE assigning any of them, so `${a}` is
  # unset there. Assign on separate lines. (Cost one red herring to find.)
  local name="$1" expr="$2" want="$3"
  local m="$TMP/mutant_${name}.sh"
  cp "$APPLIER" "$m"
  sed -E -i "$expr" "$m"

  if cmp -s "$APPLIER" "$m"; then
    fail "mutant '$name' changed NOTHING — the sed did not match, so nothing was tested"
    return
  fi
  if ! bash -n "$m" 2>"$TMP/${name}.parse"; then
    fail "mutant '$name' is SYNTACTICALLY INVALID — its redness would be meaningless"
    sed 's/^/    /' "$TMP/${name}.parse" >&2
    return
  fi

  if bash "$m" --self-test >"$TMP/${name}.out" 2>&1; then
    fail "mutant '$name' stayed GREEN — no control detects this defect"
  elif grep -qF "$want" "$TMP/${name}.out"; then
    pass "mutant '$name' killed by the right control"
  else
    fail "mutant '$name' died, but not at '$want' — the wrong control fired"
    sed 's/^/    /' "$TMP/${name}.out" >&2
  fi
}

echo "== 2. mutation kills =="

# M1 — classify by SHA pins only. This is the exact blind spot the older
# scripts/propagate-workflow-pins.sh still has: its PIN_RE cannot see an
# unparseable `uses: ../../`, so a repo full of them audits as clean. That
# population is issue #808.
kill_mutant illegal_blind \
  's@^  if \[ -n "\$\(illegal_uses "\$f"\)" \]; then@  if false; then@' \
  "FAIL illegal.yml"

# M2 — compare a pin to the target by exact string instead of by prefix. A
# legitimately short pin then reads BEHIND forever: the applier rewrites it, the
# rewrite is a no-op, and the next run finds it BEHIND again. An applier that
# never converges is just a sweep on a cron.
kill_mutant short_pin_never_converges \
  's@if \[ "\$\{target:0:\$\{#sha\}\}" = "\$sha" \]@if [ "$target" = "$sha" ]@' \
  "FAIL short.yml"

# M3 — drop branch/tag tracking detection. `affinescript` tracks `@main`;
# dropping TRACKING from the census makes the estate's one unpinned caller
# invisible to the policy that forbids unpinned callers.
kill_mutant tracking_blind \
  's@^  if \[ -n "\$\(tracking_refs "\$f"\)" \]; then@  if false; then@' \
  "FAIL tracking.yml"

# M4 — anchor the rewrite on the SHA rather than on the standards reusable PATH.
# It then re-points EVERY 40-hex pin in the file, including actions/checkout, at
# a standards commit. Catastrophic, and both the idempotency control and the
# hit-the-target control stay green through it — which is precisely why the
# third-party control had to be added.
kill_mutant rewrite_overreaches \
  's|s#\(hyperpolymath/standards/\\\.github/workflows/\[A-Za-z0-9\._-\]\+\\\.ya\?ml@\)|s#()|' \
  "FAIL rewrite overreached"

# M5 — the illegal repair emits a ref with no SHA. A "repair" that leaves the
# workflow still unparseable converts a visible failure into a repaired-looking
# one, which is strictly worse than not repairing it.
kill_mutant illegal_repair_still_illegal \
  's@\@\$\{target\}@\@@g' \
  "FAIL illegal repair"

echo
if [ "$rc" -ne 0 ]; then echo "RESULT: FAILED" >&2; else echo "RESULT: all checks passed"; fi
exit $rc
