#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proves check-language-guide.sh CAN FAIL, and fails for the right reasons.
#
# A per-language testing guide that silently omits a required section, the
# R1..R9 requirement mapping, or its SPDX header is a false-completeness hole.
# Each omission must be rejected on its own.
set -euo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/check-language-guide.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
cd "$WORK"

SECTIONS=("Requirement mapping" "Tools" "Recommended CI pipeline" "Best practices" "Known gaps" "Resources")

# guide <marker> [section-to-omit] [omit-r9] [omit-spdx]  → a guide on stdout
guide() {
  local mark="$1" omit="${2:-}" no_r9="${3:-}" no_spdx="${4:-}" s
  [ -n "$no_spdx" ] || echo "// SPDX-License-Identifier: CC-BY-SA-4.0"
  echo "= Example testing guide"
  echo
  for s in "${SECTIONS[@]}"; do
    [ "$s" = "$omit" ] && continue
    echo "$mark $s"
    [ "$s" = "Requirement mapping" ] && { echo "R1 unit tests"; [ -n "$no_r9" ] || echo "R9 mutation testing"; }
    echo "body"
  done
}

pass=0; fail=0
expect() { # expect <wanted-exit> <label> <file...>
  local want="$1" label="$2" got=0; shift 2
  bash "$SCRIPT" "$@" >out 2>&1 || got=$?
  if [ "$got" = "$want" ]; then pass=$((pass+1)); echo "  ok    $label"
  else fail=$((fail+1)); echo "  FAIL  $label (wanted exit $want, got $got)"; sed 's/^/        /' out; fi
}

guide "==" > ok.adoc
expect 0 "a complete AsciiDoc guide is valid" ok.adoc

guide "##" > ok.md
expect 0 "a complete Markdown-heading guide is valid" ok.md

for s in "${SECTIONS[@]}"; do
  f="no-${s// /-}.adoc"
  guide "==" "$s" > "$f"
  expect 1 "a guide missing '$s' is rejected" "$f"
done

guide "==" "" no-r9 > no-r9.adoc
expect 1 "a guide whose mapping never reaches R9 is rejected" no-r9.adoc

guide "==" "" "" no-spdx > no-spdx.adoc
expect 1 "a guide with no SPDX header is rejected" no-spdx.adoc

guide "==" | sed 's/^== Known gaps$/== Known gaps (none)/' > renamed.adoc
expect 1 "a renamed section heading does not satisfy the requirement" renamed.adoc

expect 1 "one bad guide among good ones fails the run" ok.adoc no-r9.adoc ok.md

expect 1 "a guide that does not exist is rejected" missing.adoc

echo "PASS=$pass FAIL=$fail"
[ "$fail" -eq 0 ]
