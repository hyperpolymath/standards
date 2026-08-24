#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# governance-gates-505-test.sh — fixture suite for the two governance gates
# promoted from theatre to real checks in standards#505:
#
#   scripts/check-docs-presence.sh   (was: `::warning::` only)
#   scripts/check-package-policy.sh  (was: unconditional `✅ ... passed`)
#
# Issue #505 requires each change be proved with BOTH a pass and a fail fixture
# before merge, and notes that `standards` CI does not exercise the reusable
# (callers pin a SHA) — so watching standards go green proves nothing. This
# suite is the proof: it drives every branch of both gates, on both sides of the
# grace cutoff, via the DOCS_TODAY / PKG_TODAY test seams.
#
# Run: bash scripts/tests/governance-gates-505-test.sh

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DOCS="$SCRIPT_DIR/../check-docs-presence.sh"
PKG="$SCRIPT_DIR/../check-package-policy.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

pass=0
fail=0

# assert <label> <expected-status> <expected-substring|-> <command...>
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
  echo "PASS: $label"
  pass=$((pass + 1))
}

mkrepo() {
  local d="$WORK/$1"; shift
  rm -rf "$d"; mkdir -p "$d"
  local f
  for f in "$@"; do mkdir -p "$d/$(dirname "$f")"; : > "$d/$f"; done
  printf '%s' "$d"
}

BEFORE="2026-08-01"   # inside the grace window (cutoff 2026-08-21)
AFTER="2026-09-01"    # past the cutoff

echo "=== check-docs-presence.sh ==="

r=$(mkrepo docs-ok README.adoc LICENSE CONTRIBUTING.md)
assert "all docs present (pre-cutoff) passes" 0 "✅ Core documentation present" \
  env DOCS_TODAY="$BEFORE" "$DOCS" "$r"
assert "all docs present (post-cutoff) passes" 0 "✅ Core documentation present" \
  env DOCS_TODAY="$AFTER" "$DOCS" "$r"

r=$(mkrepo docs-md README.md LICENSE.txt CONTRIBUTING.adoc)
assert "alternate extensions accepted" 0 "✅ Core documentation present" \
  env DOCS_TODAY="$AFTER" "$DOCS" "$r"

# README/LICENSE are BLOCKING NOW — the grace window must not shelter them.
r=$(mkrepo docs-no-readme LICENSE CONTRIBUTING.md)
assert "missing README fails even pre-cutoff" 1 "Missing required documentation: README" \
  env DOCS_TODAY="$BEFORE" "$DOCS" "$r"

r=$(mkrepo docs-no-licence README.adoc CONTRIBUTING.md)
assert "missing LICENSE fails even pre-cutoff" 1 "Missing required documentation: LICENSE" \
  env DOCS_TODAY="$BEFORE" "$DOCS" "$r"

# CONTRIBUTING is the grace-windowed one: the SAME repo must pass before the
# cutoff and fail after it. This pair is the proof the date actually flips.
r=$(mkrepo docs-no-contrib README.adoc LICENSE)
assert "missing CONTRIBUTING warns pre-cutoff (no pass claimed)" 0 "NOT YET ENFORCED" \
  env DOCS_TODAY="$BEFORE" "$DOCS" "$r"
assert "missing CONTRIBUTING BLOCKS post-cutoff" 1 "Missing required documentation: CONTRIBUTING" \
  env DOCS_TODAY="$AFTER" "$DOCS" "$r"

# The cutoff is inclusive: enforcement begins ON the date.
assert "cutoff date itself enforces" 1 "Missing required documentation" \
  env DOCS_TODAY="2026-08-21" "$DOCS" "$r"
assert "day before cutoff still in grace" 0 "NOT YET ENFORCED" \
  env DOCS_TODAY="2026-08-20" "$DOCS" "$r"

# Anti-disarm: a malformed cutoff must refuse to run, not silently grace.
assert "malformed cutoff refuses to run" 1 "is not YYYY-MM-DD" \
  env ENFORCE_CONTRIBUTING_FROM="soon" "$DOCS" "$r"
assert "missing repo root errors" 1 "is not a directory" \
  env DOCS_TODAY="$AFTER" "$DOCS" "$WORK/does-not-exist"

echo
echo "=== check-package-policy.sh ==="

r=$(mkrepo pkg-guix guix.scm)
assert "guix.scm passes" 0 "Guix package management detected" \
  env PKG_TODAY="$AFTER" "$PKG" "$r"

r=$(mkrepo pkg-manifest manifest.scm)
assert "manifest.scm passes" 0 "Guix package management detected" \
  env PKG_TODAY="$AFTER" "$PKG" "$r"

r=$(mkrepo pkg-nix flake.nix)
assert "Nix-only packaging BLOCKS after retirement" 1 "Nix-only packaging is not compliant" \
  env PKG_TODAY="$AFTER" "$PKG" "$r"

# Preserve the historical grace seam without reviving the retired policy: a
# pre-retirement Nix-only repo warns and makes no pass claim.
assert "Nix-only packaging warns before retirement" 0 "NOT YET ENFORCED" \
  env PKG_TODAY="2026-05-31" "$PKG" "$r"

# Same repo, both sides of the cutoff — the self-flipping proof.
r=$(mkrepo pkg-none README.adoc)
assert "no packaging warns pre-cutoff (no pass claimed)" 0 "NOT YET ENFORCED" \
  env PKG_TODAY="$BEFORE" "$PKG" "$r"
assert "no packaging BLOCKS post-cutoff" 1 "Package policy violation" \
  env PKG_TODAY="$AFTER" "$PKG" "$r"

# Tightened predicate: a stray Guile file is not packaging. The replaced step
# accepted this via `find . -name "*.scm"`.
r=$(mkrepo pkg-stray-scm src/helpers.scm)
assert "stray .scm does NOT satisfy the policy" 1 "Package policy violation" \
  env PKG_TODAY="$AFTER" "$PKG" "$r"

# Vendored trees must not satisfy the policy on the repo's behalf.
r=$(mkrepo pkg-vendored node_modules/foo/guix.scm)
assert "guix.scm in node_modules does not count" 1 "Package policy violation" \
  env PKG_TODAY="$AFTER" "$PKG" "$r"

r=$(mkrepo pkg-deps deps/bar/flake.nix)
assert "flake.nix in deps/ does not count" 1 "Package policy violation" \
  env PKG_TODAY="$AFTER" "$PKG" "$r"

assert "malformed cutoff refuses to run" 1 "is not YYYY-MM-DD" \
  env ENFORCE_PACKAGE_POLICY_FROM="2026-8-21" "$PKG" "$r"

# Regression: `find | head -1` under `set -o pipefail` takes SIGPIPE on trees
# big enough that find is still writing when head exits, which aborted the
# script under `set -e` and red the repo for no policy reason. Small fixtures
# cannot reproduce it — this one is deliberately large, and repeated, because
# the race is nondeterministic.
# The race needs find to still be WRITING when head exits, so the fixture needs
# many *matching* paths, not merely many files: a single match emits one line
# and head never closes the pipe early.
r=$(mkrepo pkg-large guix.scm)
mkdir -p "$r/deep"
for i in $(seq 1 4000); do
  mkdir -p "$r/deep/d$i"; : > "$r/deep/d$i/manifest.scm"
done
race_fail=0
for _ in $(seq 1 15); do
  PKG_TODAY="$AFTER" "$PKG" "$r" >/dev/null 2>&1 || race_fail=1
done
if [ "$race_fail" -eq 0 ]; then
  echo "PASS: large tree does not SIGPIPE-abort (15 runs)"
  pass=$((pass + 1))
else
  echo "FAIL: large tree SIGPIPE-aborted — pipefail race regression"
  fail=$((fail + 1))
fi

echo
echo "=== summary: $pass passed, $fail failed ==="
[ "$fail" -eq 0 ] || exit 1
