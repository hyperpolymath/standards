#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Controls for the affirmation gate. Each control names the specific defect it
# would catch; a control that cannot fail against a broken gate proves nothing.

set -euo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
fixture=$(mktemp -d)
trap 'rm -rf -- "$fixture"' EXIT

pass() { printf '  ok   %s\n' "$1"; }
fail() { printf '  FAIL %s\n' "$1" >&2; exit 1; }

# `if` context is used so that set -e does not fire on an expected non-zero exit.
expect_pass() {
  local desc=$1; shift
  if env GITHUB_WORKSPACE="$fixture" "$@" "$here/check.sh" >/dev/null 2>&1
  then pass "$desc"; else fail "$desc (expected exit 0)"; fi
}
expect_fail() {
  local desc=$1; shift
  if env GITHUB_WORKSPACE="$fixture" "$@" "$here/check.sh" >/dev/null 2>&1
  then fail "$desc (expected non-zero exit)"; else pass "$desc"; fi
}

reset_fixture() { rm -rf -- "${fixture:?}"/* ; mkdir -p "$fixture/docs"; }

good_affirmation() {
  printf '%s\n' \
    '= AFFIRMATION — controlled fixture' \
    'This snapshot makes a falsifiable claim.' \
    'The claim is anchored to a named revision.' \
    'Tests were run and their scope is stated.' \
    'Unproved properties are not called proved.' \
    'Later revisions must be assessed separately.' \
    > "$1"
}

echo 'affirmation-check controls'

# --- baseline behaviour ------------------------------------------------------
reset_fixture
expect_pass 'absent + not required passes' AFFIRMATION_REQUIRED=false
expect_fail 'absent + required fails'      AFFIRMATION_REQUIRED=true

printf '= AFFIRMATION\n' > "$fixture/AFFIRMATION.adoc"
expect_fail 'stub fails' AFFIRMATION_REQUIRED=true

reset_fixture
good_affirmation "$fixture/AFFIRMATION.adoc"
expect_pass 'valid affirmation at root passes' AFFIRMATION_REQUIRED=true

# --- defect 1: the standard permits docs/, and the canonical rsr-template-repo
#     skeleton ships at docs/AFFIRMATION.adoc. A root-only gate FAILS here.
reset_fixture
good_affirmation "$fixture/docs/AFFIRMATION.adoc"
expect_pass 'valid affirmation at docs/ passes' AFFIRMATION_REQUIRED=true

# --- defect 2: AFFIRMATION.md is banned by the standard. A gate that lists it
#     as a fallback candidate FAILS both of these.
reset_fixture
good_affirmation "$fixture/AFFIRMATION.md"
expect_fail 'AFFIRMATION.md is rejected as banned' AFFIRMATION_REQUIRED=false

reset_fixture
good_affirmation "$fixture/docs/AFFIRMATION.md"
expect_fail 'docs/AFFIRMATION.md is rejected as banned' AFFIRMATION_REQUIRED=false

# --- placeholders ------------------------------------------------------------
reset_fixture
good_affirmation "$fixture/AFFIRMATION.adoc"
printf '= AFFIRMATION — {{PROJECT_NAME}}\n' >> "$fixture/AFFIRMATION.adoc"
expect_fail 'placeholders fail a normal repo' AFFIRMATION_REQUIRED=true

# --- defect 3: a template repo ships placeholders by design.
#     A gate with no exemption FAILS here.
expect_pass 'placeholders are exempt in a template repo' \
  AFFIRMATION_REQUIRED=true AFFIRMATION_IS_TEMPLATE=true

# unreplaced anchor fields are placeholders too
reset_fixture
good_affirmation "$fixture/AFFIRMATION.adoc"
printf '| Commit (HEAD)\n| `<full 40-character SHA — never abbreviated>`\n' >> "$fixture/AFFIRMATION.adoc"
expect_fail 'unreplaced anchor field fails' AFFIRMATION_REQUIRED=true

echo 'affirmation-check controls passed'
