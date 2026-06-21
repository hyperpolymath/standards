#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
set -euo pipefail

# Regression test for propagate-workflow-pins.sh.

TEST_DIR=$(mktemp -d)
trap 'rm -rf "$TEST_DIR"' EXIT

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROP="$SCRIPT_DIR/../propagate-workflow-pins.sh"

OLD="aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
TARGET="bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

PASS=0; TOTAL=0

# try DESC CMD [ARGS...] — runs CMD as a real command (no eval); PASS on exit 0.
try() {
  local desc="$1"; shift
  TOTAL=$((TOTAL + 1))
  if "$@"; then echo "PASS: $desc"; PASS=$((PASS + 1)); else echo "FAIL: $desc"; fi
}

# Small eval-free predicates usable as `try` commands.
contains()      { case "$2" in *"$1"*) return 0 ;; *) return 1 ;; esac; }
not_contains()  { case "$2" in *"$1"*) return 1 ;; *) return 0 ;; esac; }
fresh_only()    { contains FRESH "$1" && not_contains BEHIND "$1"; }
file_has()      { grep -q "$2" "$1"; }
staged_has()    { git -C "$1" diff --cached --name-only | grep -q "$2"; }

mk_consumer() { # dir
  mkdir -p "$1/.github/workflows"
  cat > "$1/.github/workflows/governance.yml" <<EOF
name: Governance
on: push
jobs:
  governance:
    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@${OLD}
EOF
  cat > "$1/.github/workflows/scorecard.yml" <<EOF
name: Scorecard
on: push
jobs:
  scorecard:
    # a non-standards action pin that MUST be left untouched
    steps:
      - uses: actions/checkout@${OLD}
    uses: hyperpolymath/standards/.github/workflows/scorecard-reusable.yml@${OLD}
EOF
}

# ── 1. AUDIT mode reports BEHIND and does not modify files ──────────────────
R="$TEST_DIR/audit"; mk_consumer "$R"
OUT=$(bash "$PROP" --to "$TARGET" "$R")
try "audit reports BEHIND" contains BEHIND "$OUT"
try "audit leaves file unchanged" file_has "$R/.github/workflows/governance.yml" "governance-reusable.yml@${OLD}"

# ── 2. FIX rewrites the standards pins to the target ────────────────────────
R="$TEST_DIR/fix"; mk_consumer "$R"
bash "$PROP" --fix --to "$TARGET" "$R" >/dev/null
try "fix bumps governance pin" file_has "$R/.github/workflows/governance.yml" "governance-reusable.yml@${TARGET}"
try "fix bumps scorecard pin"  file_has "$R/.github/workflows/scorecard.yml"  "scorecard-reusable.yml@${TARGET}"

# ── 3. Non-standards action pins are NOT touched ────────────────────────────
try "actions/checkout pin untouched" file_has "$R/.github/workflows/scorecard.yml" "actions/checkout@${OLD}"

# ── 4. After fix, audit reports FRESH (idempotent) ──────────────────────────
OUT=$(bash "$PROP" --to "$TARGET" "$R")
try "post-fix audit reports FRESH" fresh_only "$OUT"

# ── 5. Re-running fix is a no-op (idempotent) ───────────────────────────────
BEFORE=$(cat "$R/.github/workflows/governance.yml")
bash "$PROP" --fix --to "$TARGET" "$R" >/dev/null
try "fix is idempotent" test "$(cat "$R/.github/workflows/governance.yml")" = "$BEFORE"

# ── 6. --fix in a git repo stages a bump branch (no commit/push) ────────────
R="$TEST_DIR/gitrepo"; mk_consumer "$R"
git -C "$R" init -q
git -C "$R" config user.email t@t; git -C "$R" config user.name t
git -C "$R" add -A; git -C "$R" commit -q -m init
bash "$PROP" --fix --to "$TARGET" "$R" >/dev/null 2>&1
try "fix creates bump branch" test "$(git -C "$R" branch --show-current)" = "chore/bump-standards-pins"
try "fix stages the change (no commit)" staged_has "$R" governance.yml

# ── 7. Parent-dir mode processes multiple repos ─────────────────────────────
ROOT="$TEST_DIR/many"; mk_consumer "$ROOT/repoA"; mk_consumer "$ROOT/repoB"
OUT=$(bash "$PROP" --to "$TARGET" "$ROOT")
try "parent-dir mode sees repoA" contains repoA "$OUT"
try "parent-dir mode sees repoB" contains repoB "$OUT"

echo "----------------------------------------"
echo "$PASS/$TOTAL test cases passed."
[ "$PASS" -eq "$TOTAL" ] || { echo "Some propagation tests FAILED."; exit 1; }
echo "All propagation tests passed!"
exit 0
