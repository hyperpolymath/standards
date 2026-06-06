#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# tooling-integrity-lint.sh — enforces the Hyperpolymath Tooling Version
# Integrity Policy (see ../TOOLING-VERSION-INTEGRITY-POLICY.adoc).
#
# Scans .github/workflows/*.{yml,yaml} for:
#   R1  an unversioned rhyming-family tool install
#       (`tool: just` instead of `tool: just@<ver>`).  ALWAYS BLOCKING:
#       few instances, unambiguously wrong, the burble#39 root cause.
#   R4  an *unexplained* `continue-on-error: true`.  A soft-gate is
#       explained if, within 12 lines above it, there is EITHER
#         (a) `GATE DEACTIVATED <ISO-DATE>`  — a dated, root-caused,
#             temporary suppression of a known-failing gate, OR
#         (b) `by-design:` / `advisory:`     — a documented, intentional
#             best-effort step (e.g. upstream-outage resilience).
#       Bare, with neither, is a violation.  ADVISORY by default
#       (reported, non-blocking) per the estate "advisory first,
#       tighten later" gating doctrine; pass --strict to make R4 block.
#
# Exit 0 = clean (or only advisory R4 findings in default mode).
# Exit 1 = blocking violations.
# Pure bash + grep/awk; no external deps. Run from a repo root.

set -uo pipefail

STRICT=0
[ "${1:-}" = "--strict" ] && STRICT=1

FAMILY='just|must|trust|adjust|bust|dust|intend'
WF_DIR=".github/workflows"
r1=0
r4=0

# --- R0: installed `just` must satisfy the import? floor (>= 1.19.0) --------
# BLOCKING when `just` is present (this is the burble#39 invariant, and the
# execution-proof check an in-file guard structurally cannot do). Skipped,
# with a note, when `just` is absent (repo may not use it).
if command -v just >/dev/null 2>&1; then
  jv=$(just --version 2>/dev/null | cut -d' ' -f2)
  maj=${jv%%.*}; rest=${jv#*.}; min=${rest%%.*}
  if [ -z "$jv" ] || ! { [ "${maj:-0}" -gt 1 ] 2>/dev/null || { [ "${maj:-0}" -eq 1 ] && [ "${min:-0}" -ge 19 ]; }; }; then
    echo "::error::[R0] just ${jv:-?} < 1.19.0 — import? unsupported (Tooling Version Integrity Rule 1/3)"
    echo "tooling-integrity-lint: FAIL — just below the import? floor"
    exit 1
  fi
  echo "tooling-integrity-lint: R0 OK — just $jv satisfies >= 1.19.0"
else
  echo "tooling-integrity-lint: R0 skipped — just not on PATH"
fi

[ -d "$WF_DIR" ] || { echo "tooling-integrity-lint: no $WF_DIR — nothing more to check"; exit 0; }

while IFS= read -r -d '' wf; do
  # --- R1: unversioned family-tool install (BLOCKING) ----------------------
  while IFS=: read -r lineno _; do
    [ -n "${lineno:-}" ] || continue
    echo "::error file=$wf,line=$lineno::[R1] unversioned family-tool install — pin 'tool: <name>@<version>' (Tooling Version Integrity Rule 1)"
    r1=$((r1 + 1))
  done < <(grep -nE "^[[:space:]]*tool:[[:space:]]*(${FAMILY})[[:space:]]*$" "$wf" 2>/dev/null)

  # --- R4: unexplained continue-on-error (ADVISORY unless --strict) ---------
  while IFS=: read -r lineno _; do
    [ -n "${lineno:-}" ] || continue
    echo "::warning file=$wf,line=$lineno::[R4] unexplained continue-on-error — add a 'GATE DEACTIVATED <ISO-DATE>' suppression block OR a 'by-design:'/'advisory:' rationale (Tooling Version Integrity Rule 4)"
    r4=$((r4 + 1))
  done < <(awk '
    { line[NR] = $0 }
    /^[[:space:]]*continue-on-error:[[:space:]]*true[[:space:]]*$/ {
      ok = 0
      for (i = NR-1; i >= NR-12 && i >= 1; i--) {
        if (line[i] ~ /GATE DEACTIVATED[[:space:]]+[0-9]{4}-[0-9]{2}-[0-9]{2}/) { ok = 1; break }
        if (line[i] ~ /(by-design|advisory):/)                                 { ok = 1; break }
      }
      if (!ok) print NR ":"
    }' "$wf" 2>/dev/null)
done < <(find "$WF_DIR" -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) -print0 2>/dev/null)

echo "tooling-integrity-lint: R1(blocking)=$r1  R4(soft-gate)=$r4  strict=$STRICT"

if [ "$r1" -gt 0 ]; then
  echo "tooling-integrity-lint: FAIL — $r1 unversioned family-tool install(s). See TOOLING-VERSION-INTEGRITY-POLICY.adoc Rule 1"
  exit 1
fi
if [ "$STRICT" -eq 1 ] && [ "$r4" -gt 0 ]; then
  echo "tooling-integrity-lint: FAIL (--strict) — $r4 unexplained continue-on-error. See Rule 4"
  exit 1
fi
[ "$r4" -gt 0 ] && echo "tooling-integrity-lint: PASS with $r4 advisory R4 finding(s) (non-blocking; --strict to enforce)"
[ "$r4" -eq 0 ] && echo "tooling-integrity-lint: OK — no violations"
exit 0
