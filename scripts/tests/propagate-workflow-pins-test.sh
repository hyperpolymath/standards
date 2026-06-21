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
ok() { TOTAL=$((TOTAL + 1)); if eval "$2"; then echo "PASS: $1"; PASS=$((PASS + 1)); else echo "FAIL: $1"; fi; }

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
ok "audit reports BEHIND" '[[ "$OUT" == *BEHIND* ]]'
ok "audit leaves file unchanged" 'grep -q "governance-reusable.yml@${OLD}" "$R/.github/workflows/governance.yml"'

# ── 2. FIX rewrites the standards pins to the target ────────────────────────
R="$TEST_DIR/fix"; mk_consumer "$R"
bash "$PROP" --fix --to "$TARGET" "$R" >/dev/null
ok "fix bumps governance pin" 'grep -q "governance-reusable.yml@${TARGET}" "$R/.github/workflows/governance.yml"'
ok "fix bumps scorecard pin"  'grep -q "scorecard-reusable.yml@${TARGET}" "$R/.github/workflows/scorecard.yml"'

# ── 3. Non-standards action pins are NOT touched ────────────────────────────
ok "actions/checkout pin untouched" 'grep -q "actions/checkout@${OLD}" "$R/.github/workflows/scorecard.yml"'

# ── 4. After fix, audit reports FRESH (idempotent) ──────────────────────────
OUT=$(bash "$PROP" --to "$TARGET" "$R")
ok "post-fix audit reports FRESH" '[[ "$OUT" == *FRESH* && "$OUT" != *BEHIND* ]]'

# ── 5. Re-running fix is a no-op (idempotent) ───────────────────────────────
BEFORE=$(cat "$R/.github/workflows/governance.yml")
bash "$PROP" --fix --to "$TARGET" "$R" >/dev/null
ok "fix is idempotent" '[[ "$(cat "$R/.github/workflows/governance.yml")" == "$BEFORE" ]]'

# ── 6. --fix in a git repo stages a bump branch (no commit/push) ────────────
R="$TEST_DIR/gitrepo"; mk_consumer "$R"
git -C "$R" init -q
git -C "$R" config user.email t@t; git -C "$R" config user.name t
git -C "$R" add -A; git -C "$R" commit -q -m init
bash "$PROP" --fix --to "$TARGET" "$R" >/dev/null 2>&1
ok "fix creates bump branch" '[[ "$(git -C "$R" branch --show-current)" == "chore/bump-standards-pins" ]]'
ok "fix stages the change (no commit)" 'git -C "$R" diff --cached --name-only | grep -q governance.yml'

# ── 7. Parent-dir mode processes multiple repos ─────────────────────────────
ROOT="$TEST_DIR/many"; mk_consumer "$ROOT/repoA"; mk_consumer "$ROOT/repoB"
OUT=$(bash "$PROP" --to "$TARGET" "$ROOT")
ok "parent-dir mode sees repoA" '[[ "$OUT" == *repoA* ]]'
ok "parent-dir mode sees repoB" '[[ "$OUT" == *repoB* ]]'

echo "----------------------------------------"
echo "$PASS/$TOTAL test cases passed."
[ "$PASS" -eq "$TOTAL" ] || { echo "Some propagation tests FAILED."; exit 1; }
echo "All propagation tests passed!"
exit 0
