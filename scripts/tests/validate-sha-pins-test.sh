#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Tests for .githooks/validate-sha-pins.sh.
#
# ⚠ TESTS 1 AND 6 ARE THE POINT. Test 1 is the planted positive -- a fully pinned
# workflow must exit 0, which is the only way to tell a working gate from one that
# dies (or passes) on everything. Test 6 is the regression this file exists for: a
# fixture directory holding ONLY root-level *.yml workflows. The old find() bound
# -print to the *.yaml clause alone, so that exact shape printed zero paths and
# the hook certified "All workflow actions are SHA-pinned" while 26 refs in this
# repo floated on tags. Any gate that can scan nothing must fail loudly.
set -uo pipefail
HOOK="$(cd "$(dirname "$0")/../.." && pwd)/.githooks/validate-sha-pins.sh"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
mkdir -p "$T/.github/workflows" "$T/.github/actions"
pass=0; fail=0

ck() { # name expected_exit staged_files
  local out rc
  out="$(cd "$T" && INPUT_STAGED_FILES="$3" bash "$HOOK" 2>&1)"; rc=$?
  if [ "$rc" = "$2" ]; then printf '  ok    %s (exit %s)\n' "$1" "$rc"; pass=$((pass+1))
  else printf '  FAIL  %s (expected exit %s, got %s) output=%s\n' "$1" "$2" "$rc" "${out:-<none>}"; fail=$((fail+1)); fi
}
ckf() { # name expected_exit dir  -- fallback mode (no staged list), scanning $3
  local out rc
  out="$(cd "$3" && INPUT_PATH="$3" bash "$HOOK" 2>&1)"; rc=$?
  if [ "$rc" = "$2" ]; then printf '  ok    %s (exit %s)\n' "$1" "$rc"; pass=$((pass+1))
  else printf '  FAIL  %s (expected exit %s, got %s) output=%s\n' "$1" "$2" "$rc" "${out:-<none>}"; fail=$((fail+1)); fi
}

S40=3d3c42e5aac5ba805825da76410c181273ba90b1

printf 'name: pinned\non: push\njobs:\n  a:\n    steps:\n      - uses: actions/checkout@%s # v7.0.1\n' "$S40" \
  > "$T/.github/workflows/pinned.yml"
printf 'name: tag\non: push\njobs:\n  a:\n    steps:\n      - uses: actions/checkout@v7.0.1\n' \
  > "$T/.github/workflows/tag.yml"
printf 'name: mixed\non: push\njobs:\n  a:\n    steps:\n      - uses: actions/checkout@%s # v7.0.1\n      - uses: actions/cache@v6.1.0\n' "$S40" \
  > "$T/.github/workflows/mixed.yml"
printf 'name: branch\non: push\njobs:\n  a:\n    steps:\n      - uses: hyperpolymath/thing@main\n' \
  > "$T/.github/workflows/branch.yml"
printf 'name: local\non: push\njobs:\n  a:\n    steps:\n      - uses: ./local-action\n      - uses: ./.github/actions/signed-push\n' \
  > "$T/.github/workflows/local.yml"
printf 'name: docker\non:\n  a:\n    container:\n      image: node:20\n    steps:\n      - uses: docker://ghcr.io/org/img:1.2.3\n' \
  > "$T/.github/workflows/docker.yml"
printf 'name: short\non: push\njobs:\n  a:\n    steps:\n      - uses: actions/checkout@abc123 # not a full SHA\n' \
  > "$T/.github/workflows/short.yml"

echo "validate-sha-pins.sh"
ck "PLANTED POSITIVE: fully pinned workflow must PASS" 0 ".github/workflows/pinned.yml"
ck "moving tag FAILS"          1 ".github/workflows/tag.yml"
ck "one unpinned among pinned FAILS (whole-file whitening)" 1 ".github/workflows/mixed.yml"
ck "branch ref FAILS"          1 ".github/workflows/branch.yml"
ck "short hex ref FAILS (40 required)" 1 ".github/workflows/short.yml"
ck "local ./ action ignored"   0 ".github/workflows/local.yml"
ck "docker:// ref ignored"     0 ".github/workflows/docker.yml"
ck "non-workflow staged file ignored" 0 "README.adoc"
ck "two valid files pass together" 0 ".github/workflows/pinned.yml
.github/workflows/local.yml"
ck "one bad among good still fails"  1 ".github/workflows/pinned.yml
.github/workflows/tag.yml"
printf 'name: c\nruns:\n  using: composite\n  steps:\n    - uses: actions/github-script@v9.0.0\n' > "$T/.github/actions/c.yml"
ck "composite action under .github/actions is checked" 1 ".github/actions/c.yml"

# Test 6: the empty-scan regression -- root-level .yml only, fallback mode.
mkdir -p "$T/ymlonly/.github/workflows"
printf 'name: t\non: push\njobs:\n  a:\n    steps:\n      - uses: actions/checkout@v7.0.1\n' > "$T/ymlonly/.github/workflows/a.yml"
printf 'name: t\non: push\njobs:\n  a:\n    steps:\n      - uses: actions/checkout@%s # v7.0.1\n' "$S40" > "$T/ymlonly/.github/workflows/b.yml"
ckf "root-level *.yml IS scanned (find -print precedence)" 1 "$T/ymlonly"
mkdir -p "$T/nested/sub/.github/workflows"
printf 'name: t\non: push\njobs:\n  a:\n    steps:\n      - uses: actions/checkout@v7.0.1\n' > "$T/nested/sub/.github/workflows/a.yml"
ckf "nested .github/workflows is scanned too" 1 "$T/nested"
mkdir -p "$T/vend/rhodium-standard-repositories/satellites/x/.github/workflows"
printf 'name: t\non: push\njobs:\n  a:\n    steps:\n      - uses: actions/checkout@v7.0.1\n' > "$T/vend/rhodium-standard-repositories/satellites/x/.github/workflows/a.yml"
ckf "vendored RSR mirror is scoped out (see Debtfile)" 0 "$T/vend"
mkdir -p "$T/empty"
ckf "refuses to certify a tree with 0 workflow files" 1 "$T/empty"

printf '\n%s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
