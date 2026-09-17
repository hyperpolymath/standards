#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -uo pipefail
#
# run-conformance.sh — run the reference DYADT verifier over every conformance
# vector and diff the produced verdicts against the .expected files.
#
# A conforming verifier (reference or production) MUST pass this suite.

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$HERE" rev-parse --show-toplevel)"
VERIFIER="${DYADT_VERIFIER:-$ROOT/scripts/verify-claims.sh}"

pass=0 fail=0
for vec in "$HERE"/*.a2ml; do
  name="$(basename "$vec" .a2ml)"
  exp="$HERE/$name.expected"
  [ -f "$exp" ] || { echo "  ❌ $name: no .expected file"; fail=$((fail+1)); continue; }
  # Run with unverifiable allowed so the verifier reports every verdict without
  # early-exiting; we assert on the per-claim verdicts, not the process code.
  got="$(cd "$ROOT" && DYADT_ALLOW_UNVERIFIABLE=1 bash "$VERIFIER" "$vec" 2>/dev/null \
        | grep -oE '[A-Z][0-9]+  (confirmed|REFUTED|unverifiable)' \
        | awk '{print $1, tolower($2)}' | sort)"
  want="$(sort "$exp")"
  if [ "$got" = "$want" ]; then
    echo "  ✅ $name"; pass=$((pass+1))
  else
    echo "  ❌ $name"; echo "     want: $(echo "$want" | tr '\n' ';')"; echo "     got:  $(echo "$got" | tr '\n' ';')"; fail=$((fail+1))
  fi
done

echo ""
echo "DYADT conformance: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
