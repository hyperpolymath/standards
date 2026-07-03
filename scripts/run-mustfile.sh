#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# run-mustfile.sh — EXECUTE the checks declared in a Mustfile.a2ml.
#
# The Mustfile declares hard repository invariants. Each '### <id>' block has a
# severity and a means of discharge:
#   * `- run: <command>`      — executable; run it, pass iff exit 0.
#   * `- verification: <text>`— governance / manually-verified; reported as
#                               MANUAL (counted, never silently green).
#
# Blocking policy (fail loudly): a failing check of severity `critical` or
# `high` fails the run (exit 1). A failing `warning`/lower check is advisory
# (reported, non-blocking). This is the executable half of Mustfile checking;
# scripts/check-mustfile-structure.sh is the structural half.
#
# Usage: run-mustfile.sh [path/to/Mustfile.a2ml]
#   Default: .machine_readable/contractiles/must/Mustfile.a2ml
# Exit: 0 all blocking checks pass · 1 a blocking check failed · 2 file missing

set -uo pipefail

MUST="${1:-.machine_readable/contractiles/must/Mustfile.a2ml}"
if [ ! -f "$MUST" ]; then
  echo "error: Mustfile not found: $MUST" >&2
  exit 2
fi

name="" sev="" cmd="" kind=""
pass=0 warn=0 manual=0 blocking_fail=0

discharge() {
  [ -n "$name" ] || return 0
  if [ "$kind" = "run" ]; then
    if bash -c "$cmd" >/dev/null 2>&1; then
      printf '  ✅ PASS   [%-8s] %s\n' "$sev" "$name"; pass=$((pass + 1))
    else
      case "$sev" in
        critical|high)
          printf '  ❌ FAIL   [%-8s] %s\n' "$sev" "$name"; blocking_fail=$((blocking_fail + 1)) ;;
        *)
          printf '  ⚠️  WARN   [%-8s] %s\n' "$sev" "$name"; warn=$((warn + 1)) ;;
      esac
    fi
  elif [ "$kind" = "verification" ]; then
    printf '  🔎 MANUAL [%-8s] %s\n' "$sev" "$name"; manual=$((manual + 1))
  else
    # A block with neither run nor verification is a structural defect; the
    # structural validator owns that, but flag it here too rather than ignore.
    printf '  ❓ NODISCHARGE [%-4s] %s\n' "$sev" "$name"; blocking_fail=$((blocking_fail + 1))
  fi
}

# Trim leading whitespace so indented list items are still recognised.
while IFS= read -r raw; do
  line="${raw#"${raw%%[![:space:]]*}"}"
  case "$line" in
    '### '*)            discharge; name="${line:4}"; sev=""; cmd=""; kind="" ;;
    '- run: '*)         cmd="${line#- run: }"; kind="run" ;;
    '- verification: '*) cmd="${line#- verification: }"; [ "$kind" = "run" ] || kind="verification" ;;
    '- severity: '*)    sev="${line#- severity: }" ;;
  esac
done < "$MUST"
discharge  # flush last block

echo ""
echo "Mustfile: $pass passed · $warn warning · $manual manual · $blocking_fail blocking-fail"
if [ "$blocking_fail" -gt 0 ]; then
  echo "❌ Mustfile check FAILED ($blocking_fail blocking failure(s))" >&2
  exit 1
fi
echo "✅ Mustfile check passed (all critical/high checks green)"
