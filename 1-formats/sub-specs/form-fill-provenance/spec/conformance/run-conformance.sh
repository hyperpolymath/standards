#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# run-conformance.sh — run a detector over every FFP vector and diff the
# produced line against the .expected file.
#
# A conforming detector MUST pass this suite. The default detector is the
# reference probe (probe.awk); point FFP_DETECTOR at a product detector to test
# it, e.g.
#
#   FFP_DETECTOR="./target/debug/ffp-classify" bash run-conformance.sh
#
# The detector is invoked as: <detector> <path-to-vector.pdf>
# and MUST print exactly the canonical line (see probe.awk's header).
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VECTORS="$HERE/vectors"
EXPECTED="$HERE/expected"
PROBE="${FFP_DETECTOR:-awk -f $HERE/probe.awk}"

pass=0; fail=0; missing=0

# --- fixture reproducibility -------------------------------------------------
if bash "$HERE/make-fixtures.sh" --check >/dev/null 2>&1; then
  echo "  ok    vectors are byte-identical to the generator"
else
  echo "  FAIL  vectors have drifted from make-fixtures.sh (run: bash make-fixtures.sh)"
  fail=$((fail + 1))
fi

# --- every vector has an expectation, every expectation a vector -------------
for vec in "$VECTORS"/*.pdf; do
  name="$(basename "$vec" .pdf)"
  [ -f "$EXPECTED/$name.expected" ] || { echo "  FAIL  $name: no .expected file"; missing=$((missing + 1)); }
done
for exp in "$EXPECTED"/*.expected; do
  name="$(basename "$exp" .expected)"
  [ -f "$VECTORS/$name.pdf" ] || { echo "  FAIL  $name.expected: no vector"; missing=$((missing + 1)); }
done

# --- the vectors -------------------------------------------------------------
for vec in "$VECTORS"/*.pdf; do
  name="$(basename "$vec" .pdf)"
  exp="$EXPECTED/$name.expected"
  [ -f "$exp" ] || continue
  got="$($PROBE "$vec" 2>/dev/null | head -n1)"
  want="$(cat "$exp")"
  if [ "$got" = "$want" ]; then
    printf '  \033[32mok\033[0m    %s\n' "$name"
    pass=$((pass + 1))
  else
    printf '  \033[31mFAIL\033[0m  %s\n' "$name"
    printf '        want: %s\n' "$want"
    printf '        got:  %s\n' "$got"
    fail=$((fail + 1))
  fi
done

echo ""
echo "FFP conformance: $pass passed, $fail failed, $missing orphan expectation(s)"
[ "$fail" -eq 0 ] && [ "$missing" -eq 0 ]
