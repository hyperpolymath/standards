#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Verify that a merged AFFIRMATION PR is anchored, per
# docs/AFFIRMATION-STANDARD.adoc <<linear-history>>.
#
#   scripts/verify-affirmation-anchor.sh <owner/repo> <PR> [ANCHOR_SHA]
#
# A = anchor (read from the affirmation file's "Commit (HEAD)" row unless
# given), S = the PR head (the owner's signed commit), M = the commit the PR
# landed as. Checks:
#   1. S is verified-signed, authored by OWNER (default hyperpolymath), parent A
#   2. M is on the default branch and its first parent is A
#   3. tree(M) == tree(S)
#   4. refs/pull/<N>/head still resolves to S
# A merge commit (M has S as a parent) satisfies 2 and 3 trivially.
# Exits 0 when all hold, 1 when any fails, 2 on usage or API error.
set -uo pipefail

OWNER=${OWNER:-hyperpolymath}
[ $# -ge 2 ] || { echo "usage: $0 <owner/repo> <PR> [ANCHOR_SHA]" >&2; exit 2; }
R=$1 N=$2 A=${3:-}
fail=0

# api PATH JQ -- one gh api call; exits 2 on an API error, so an error body
# can never be mistaken for a value.
api() {
  local out
  out=$(gh api "repos/$R/$1" -q "$2" 2>&1) || { echo "API error on $1: $out" >&2; exit 2; }
  printf '%s' "$out"
}

# is_sha VALUE -- true when VALUE is a full 40-hex SHA.
is_sha() { [[ $1 =~ ^[0-9a-f]{40}$ ]]; }

# check LABEL CONDITION... -- print PASS/FAIL for one condition, record a fail.
check() {
  local label=$1; shift
  if "$@"; then echo "PASS  $label"; else echo "FAIL  $label"; fail=1; fi
}

merged=$(api "pulls/$N" .merged)
[ "$merged" = true ] || { echo "FAIL  PR #$N is not merged"; exit 1; }
S=$(api "pulls/$N" .head.sha)
M=$(api "pulls/$N" .merge_commit_sha)
base=$(api "pulls/$N" .base.ref)
if ! is_sha "$S" || ! is_sha "$M"; then
  echo "unexpected SHA shape: S=$S M=$M" >&2; exit 2
fi

if [ -z "$A" ]; then
  file=$(api "pulls/$N/files" '[.[].filename|select(test("AFFIRMATION";"i"))][0] // ""')
  [ -n "$file" ] || { echo "no AFFIRMATION file in PR #$N; pass ANCHOR_SHA" >&2; exit 2; }
  A=$(gh api "repos/$R/contents/$file?ref=$M" -H 'Accept: application/vnd.github.raw' 2>/dev/null \
      | grep -A2 -i 'Commit (HEAD)' | grep -oE '\b[0-9a-f]{40}\b' | head -1)
  is_sha "$A" || { echo "could not read the anchor from $file; pass ANCHOR_SHA" >&2; exit 2; }
fi
echo "repo=$R pr=#$N anchor=$A head(S)=$S landed(M)=$M base=$base"

s_parent=$(api "commits/$S" '.parents[0].sha')
s_verified=$(api "commits/$S" .commit.verification.verified)
s_author=$(api "commits/$S" '.author.login // ""')
check "1 S signed (verified=$s_verified, author=$s_author)" \
  test "$s_verified" = true -a "$s_author" = "$OWNER"
check "1 S parent is the anchor ($s_parent)" test "$s_parent" = "$A"

m_parent=$(api "commits/$M" '.parents[0].sha')
on_base=$(api "compare/$M...$base" .status)
check "2 M on $base (compare=$on_base)" test "$on_base" = ahead -o "$on_base" = identical
check "2 M first parent is the anchor ($m_parent)" test "$m_parent" = "$A"

check "3 tree(M) == tree(S)" \
  test "$(api "commits/$M" .commit.tree.sha)" = "$(api "commits/$S" .commit.tree.sha)"

pull_head=$(api "git/ref/pull/$N/head" .object.sha)
check "4 refs/pull/$N/head is S" test "$pull_head" = "$S"

[ $fail = 0 ] && echo "ANCHORED" || echo "DRAFT: the affirmation must be read as a draft"
exit $fail
