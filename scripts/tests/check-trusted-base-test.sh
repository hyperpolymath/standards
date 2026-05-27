#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Regression test for scripts/check-trusted-base.sh — exercises the
# `.trusted-base-ignore` path-fragment exemption mechanism plus the
# existing inline-annotation and proof-debt enumeration paths.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT="$SCRIPT_DIR/../check-trusted-base.sh"

if [[ ! -x "$SCRIPT" ]]; then
  echo "FATAL: cannot locate $SCRIPT" >&2
  exit 2
fi

PASS=0
FAIL=0

run_case() {
  local name="$1"; shift
  local expected_exit="$1"; shift
  local expected_substr="$1"; shift
  local setup_fn="$1"; shift

  local tmp
  tmp="$(mktemp -d)"
  (
    cd "$tmp"
    "$setup_fn"
  )
  set +e
  local out
  out="$(cd "$tmp" && bash "$SCRIPT" . 2>&1)"
  local actual_exit=$?
  set -e

  local ok=true
  if [[ "$actual_exit" -ne "$expected_exit" ]]; then ok=false; fi
  if [[ -n "$expected_substr" && "$out" != *"$expected_substr"* ]]; then ok=false; fi

  if $ok; then
    echo "ok   $name"
    PASS=$((PASS+1))
  else
    echo "FAIL $name (exit=$actual_exit, expected=$expected_exit)"
    echo "---- output ----"
    echo "$out"
    echo "----------------"
    FAIL=$((FAIL+1))
  fi
  rm -rf "$tmp"
}

# Each setup creates a fresh fixture tree at $PWD (the tmpdir).

setup_marker_no_docs() {
  mkdir -p src
  cat > src/Foo.idr <<'EOF'
module Foo
%default total
partial
foo : Int -> Int
foo x = x
EOF
}

setup_marker_with_inline_trusted() {
  mkdir -p src docs
  cat > src/Foo.idr <<'EOF'
module Foo
%default total
-- TRUSTED: documented exception per local policy.
partial
foo : Int -> Int
foo x = x
EOF
  # Inline annotations still require a proof-debt index per the original
  # script's contract; the empty doc satisfies that.
  cat > docs/proof-debt.md <<'EOF'
# Proof Debt
(Inline annotations only.)
EOF
}

setup_marker_with_proof_debt() {
  mkdir -p src docs
  cat > src/Foo.idr <<'EOF'
module Foo
%default total
partial
foo : Int -> Int
foo x = x
EOF
  cat > docs/proof-debt.md <<'EOF'
# Proof Debt
See src/Foo.idr:3 — documented partial marker.
EOF
}

setup_marker_with_ignore_exact() {
  mkdir -p test/fixtures
  cat > test/fixtures/admitted.v <<'EOF'
Theorem foo : True.
Admitted.
EOF
  cat > .trusted-base-ignore <<'EOF'
# Test fixtures: deliberately contain the patterns under test.
test/fixtures/
EOF
}

setup_marker_with_ignore_substring() {
  mkdir -p test/soundness/fixtures/code_safety
  cat > test/soundness/fixtures/code_safety/admitted.v <<'EOF'
Theorem foo : True.
Admitted.
EOF
  cat > .trusted-base-ignore <<'EOF'
# Scanner fixtures - intentional.
fixtures/code_safety
EOF
}

setup_ignore_comments_only_still_fails() {
  mkdir -p src docs
  cat > src/Foo.idr <<'EOF'
module Foo
%default total
partial
foo : Int -> Int
foo x = x
EOF
  cat > docs/proof-debt.md <<'EOF'
# Proof Debt
(Empty.)
EOF
  cat > .trusted-base-ignore <<'EOF'
# only comments here
# nothing real
EOF
}

setup_ignore_does_not_match() {
  mkdir -p src docs
  cat > src/Foo.idr <<'EOF'
module Foo
%default total
partial
foo : Int -> Int
foo x = x
EOF
  cat > docs/proof-debt.md <<'EOF'
# Proof Debt
(Empty.)
EOF
  cat > .trusted-base-ignore <<'EOF'
# Pattern that does not match.
test/fixtures/
EOF
}

setup_mixed_exempt_and_documented() {
  mkdir -p src test/fixtures docs
  cat > src/Foo.idr <<'EOF'
module Foo
%default total
partial
foo : Int -> Int
foo x = x
EOF
  cat > test/fixtures/bar.v <<'EOF'
Theorem bar : True.
Admitted.
EOF
  cat > docs/proof-debt.md <<'EOF'
# Proof Debt
src/Foo.idr:3 — documented.
EOF
  cat > .trusted-base-ignore <<'EOF'
# Exempt fixtures.
test/fixtures/
EOF
}

# Existing behaviour must still work.
run_case "marker with no docs and no ignore fails (early exit)" \
  1 "No docs/proof-debt.md (or equivalent) found" setup_marker_no_docs

run_case "inline TRUSTED comment passes (regression)" \
  0 "All 1 escape hatch(es) are documented" setup_marker_with_inline_trusted

run_case "entry in proof-debt.md passes (regression)" \
  0 "All 1 escape hatch(es) are documented" setup_marker_with_proof_debt

# New .trusted-base-ignore behaviour.
run_case "single path-fragment exempts marker (no proof-debt doc)" \
  0 "1 marker(s) exempted via .trusted-base-ignore" setup_marker_with_ignore_exact

run_case "substring-match exemption picks up deeper paths" \
  0 "1 marker(s) exempted via .trusted-base-ignore" setup_marker_with_ignore_substring

run_case "ignore file with only comments behaves as no exemptions" \
  1 "1/1 escape hatch(es) are undocumented" setup_ignore_comments_only_still_fails

run_case "ignore pattern that does not match still fails" \
  1 "1/1 escape hatch(es) are undocumented" setup_ignore_does_not_match

run_case "mixed: one marker exempted, one documented in proof-debt" \
  0 "1 marker(s) exempted via .trusted-base-ignore" setup_mixed_exempt_and_documented

echo
echo "PASS=$PASS FAIL=$FAIL"
if [[ "$FAIL" -gt 0 ]]; then
  exit 1
fi
exit 0
