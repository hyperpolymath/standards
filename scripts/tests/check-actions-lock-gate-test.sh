#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# check-actions-lock-gate-test.sh — fixture suite for scripts/check-actions-lock-gate.sh,
# the `actions-lock-verify` GATE. Every branch is driven on both sides of the
# grace cutoff. The "lockfile present" branches use a stub verifier so the
# planted positive (a verifier that says the lock is bad → gate RED) runs
# without `gh actions-lock` installed.
#
# Run: bash scripts/tests/check-actions-lock-gate-test.sh
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GATE="$SCRIPT_DIR/../check-actions-lock-gate.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
pass=0; fail=0

assert() {
  local label="$1" want="$2" needle="$3"; shift 3
  local out status
  out="$("$@" 2>&1)"; status=$?
  if [ "$status" != "$want" ]; then
    echo "FAIL: $label — expected exit $want, got $status"
    echo "      output: $(printf '%s' "$out" | head -3 | tr '\n' '|')"
    fail=$((fail + 1)); return
  fi
  if [ "$needle" != "-" ] && ! printf '%s' "$out" | grep -qF "$needle"; then
    echo "FAIL: $label — exit $status correct, but output lacked '$needle'"
    echo "      output: $(printf '%s' "$out" | head -3 | tr '\n' '|')"
    fail=$((fail + 1)); return
  fi
  echo "PASS: $label"; pass=$((pass + 1))
}

# mkwf <name> <pinned|unpinned> [lock]
mkwf() {
  local d="$WORK/$1/.github/workflows"; mkdir -p "$d"
  if [ "$2" = pinned ]; then
    printf 'jobs:\n  a:\n    steps:\n      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7\n      - uses: ./local\n      - uses: hyperpolymath/standards/.github/workflows/x.yml@main\n' > "$d/ci.yml"
  else
    printf 'jobs:\n  a:\n    steps:\n      - uses: actions/checkout@v4\n' > "$d/ci.yml"
  fi
  [ "${3:-}" = lock ] && : > "$d/actions.lock"
  printf '%s' "$d"
}
OK_VERIFIER="$WORK/verifier-ok.sh";  printf '#!/usr/bin/env bash\necho stub-verifier-ok; exit 0\n' > "$OK_VERIFIER"
BAD_VERIFIER="$WORK/verifier-bad.sh"; printf '#!/usr/bin/env bash\necho stub-verifier-BAD; exit 1\n' > "$BAD_VERIFIER"
BEFORE="2026-09-15"; AFTER="2026-10-01"

echo "=== lockfile present ==="
d=$(mkwf a pinned lock)
assert "lock + verifier OK → 0"          0 "coverage verified"        env ACTIONS_LOCK_VERIFIER="$OK_VERIFIER"  bash "$GATE" "$d"
assert "lock + verifier BAD → 1 (planted positive)" 1 "verification FAILED" env ACTIONS_LOCK_VERIFIER="$BAD_VERIFIER" bash "$GATE" "$d"
assert "lock + verifier missing → 2"     2 "verifier not found"       env ACTIONS_LOCK_VERIFIER="$WORK/nope.sh"  bash "$GATE" "$d"
assert "lock + unpinned refs: verifier decides, not the grep" 0 "stub-verifier-ok" env ACTIONS_LOCK_VERIFIER="$OK_VERIFIER" bash "$GATE" "$(mkwf b unpinned lock)"

echo "=== no lockfile ==="
d=$(mkwf c pinned)
assert "no lock, pinned, before cutoff → 0 NOT YET ENFORCED" 0 "NOT YET ENFORCED" env LOCK_TODAY="$BEFORE" bash "$GATE" "$d"
assert "no lock, pinned, before cutoff emits ::warning"      0 "::warning::"      env LOCK_TODAY="$BEFORE" bash "$GATE" "$d"
assert "no lock, pinned, on cutoff → 1"                      1 "grace window closed" env LOCK_TODAY="$AFTER" bash "$GATE" "$d"
assert "no lock, pinned, custom cutoff honoured → 1"         1 "::error::"        env LOCK_TODAY="2026-09-03" ENFORCE_ACTIONS_LOCK_FROM="2026-09-02" bash "$GATE" "$d"
d=$(mkwf e unpinned)
assert "no lock, unpinned, before cutoff → 1 (no grace for unpinned)" 1 "not SHA-pinned" env LOCK_TODAY="$BEFORE" bash "$GATE" "$d"
assert "no lock, unpinned, after cutoff → 1"                          1 "not SHA-pinned" env LOCK_TODAY="$AFTER"  bash "$GATE" "$d"
assert "missing workflows dir → 2" 2 "not found" bash "$GATE" "$WORK/does-not-exist/.github/workflows"

echo; echo "passed=$pass failed=$fail"
[ "$fail" -eq 0 ]
