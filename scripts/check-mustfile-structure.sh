#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# check-mustfile-structure.sh — structural validation of a Mustfile.a2ml.
#
# A Mustfile declares hard repository invariants as '### <id>' blocks. Every
# declared check MUST carry:
#   * a `- severity:` field, AND
#   * a means of discharge — either an executable `- run:` command OR an
#     explicit `- verification:` (governance / manually-verified) field.
#
# A check that declares neither run nor verification is a hollow assertion: it
# looks like enforcement but discharges nothing. This validator fails loudly on
# such a check rather than letting it pass as green. It is the structural half
# of Mustfile checking; scripts/run-mustfile.sh (Wave 1) executes the `- run:`
# commands.
#
# Usage: check-mustfile-structure.sh [path/to/Mustfile.a2ml]
#   Default path: .machine_readable/contractiles/must/Mustfile.a2ml
# Exit: 0 valid · 1 malformed/hollow · 2 file not found

set -euo pipefail

MUST="${1:-.machine_readable/contractiles/must/Mustfile.a2ml}"

if [ ! -f "$MUST" ]; then
  echo "error: Mustfile not found: $MUST" >&2
  exit 2
fi

awk '
  function flush() {
    if (name == "") return
    if (!(has_run || has_ver)) { printf "  ❌ %s: no `- run:` or `- verification:` (hollow check)\n", name; fails++ }
    if (!has_sev)              { printf "  ❌ %s: no `- severity:`\n", name; fails++ }
  }
  /^### / { flush(); name=$0; has_run=0; has_ver=0; has_sev=0; checks++; next }
  /^[[:space:]]*-[[:space:]]*run:/          { has_run=1 }
  /^[[:space:]]*-[[:space:]]*verification:/ { has_ver=1 }
  /^[[:space:]]*-[[:space:]]*severity:/     { has_sev=1 }
  END {
    flush()
    if (checks < 1) { print "❌ Mustfile declares no checks (expected one or more \"### <id>\" blocks)"; exit 1 }
    if (fails > 0)  { printf "❌ Malformed Mustfile: %d structural problem(s) across %d checks\n", fails, checks; exit 1 }
    printf "✅ Mustfile structurally valid (%d checks, each with severity + run/verification)\n", checks
  }
' "$MUST"
