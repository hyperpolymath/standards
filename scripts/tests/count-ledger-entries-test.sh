#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Tests for count-ledger-entries.sh.
#
# ⚠ THE FOURTH TEST IS THE REASON THIS FILE EXISTS. The implementation this
# replaced matched an array with a non-greedy `\[(.*?)\]`, so a `]` inside a
# string ended the array early. The estate's own `.gitleaks.toml` contains
# exactly that — regex entries with character classes — so the counter returned
# 0 for a ledger holding 16 entries. The ratchet then compared 0 against 0 and
# reported OK while measuring nothing, which is the precise failure the counter
# was written to prevent. Any future rewrite must keep this test passing.
set -uo pipefail

COUNTER="$(dirname "$0")/../count-ledger-entries.sh"
pass=0
fail=0

check() {
  local name="$1" expected="$2" input="$3" got
  got="$(printf '%s' "$input" | "$COUNTER")"
  if [ "$got" = "$expected" ]; then
    printf '  ok    %s\n' "$name"
    pass=$((pass + 1))
  else
    printf '  FAIL  %s (expected %s, got %s)\n' "$name" "$expected" "$got"
    fail=$((fail + 1))
  fi
}

SQ=\'

check "basic strings across lines" 2 \
  "paths = [
  \"a\",
  \"b\",
]"

check "single-line array" 3 'paths = ["a", "b", "c"]'

check "literal and basic forms both count" 2 \
  "paths = [\"a\", ${SQ}b${SQ}]"

check "a ] inside a string does not end the array" 2 \
  "paths = [
  \"has ] a bracket\",
  \"b\",
]"

check "trailing comment is not an entry" 2 \
  "paths = [
  \"a\",  # a comment mentioning ${SQ}quotes${SQ} and ]
  \"b\",
]"

check "a commented-out array is not counted" 1 \
  "# paths = [\"not\", \"counted\"]
paths = [\"real\"]"

check "empty arrays count zero" 0 \
  'paths = []
regexes = []'

check "unrelated keys are ignored" 1 \
  'other = ["x", "y"]
paths = ["a"]'

check "regexes counts alongside paths" 3 \
  'paths=["a","b"]
regexes=["c"]'

check "literal multiline string is one entry" 2 \
  "regexes = [
  ${SQ}${SQ}${SQ}spans
lines${SQ}${SQ}${SQ},
  \"x\",
]"

check "no ledger arrays at all" 0 'name = "something"'

printf '\n  %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
