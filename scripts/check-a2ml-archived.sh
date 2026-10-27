#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-a2ml-archived.sh — A2ML is retired (owner rulings D99, D269c, D312,
# D313); no A2ML file may appear outside the archive or the legacy ledger.
#
# Every tracked A2ML file (*.a2ml, *.a2ml.in, *.a2ml.template,
# *.a2ml.vN.template) must either sit under 9-archive/ or be named, by exact
# path, in .machine_readable/a2ml-legacy-allow.txt. The check runs both ways:
#
#   * a file in neither place FAILS: a new or moved A2ML file is drift;
#   * a ledger entry whose file is gone FAILS: the entry must leave in the
#     same commit that archives, translates or deletes its file, so the
#     ledger only shrinks (the exemption ratchet forbids it growing).
#
# Ledger entries are exact paths. A glob or a 9-archive/ path in the ledger
# is a defect: a pattern would absorb files added later, and an archived
# file needs no exemption.
#
# In a git work tree the set is the tracked files plus untracked files that
# are not ignored, so a contributor sees a new file before committing it.
# Outside git (fixtures) every file under ROOT is scanned.
#
# Usage: check-a2ml-archived.sh [ROOT]     (default: the current directory)
# Exit:  0 clean · 1 violation · 2 setup error (no ledger, unreadable root)

set -euo pipefail

ROOT="${1:-.}"
LEDGER_REL=".machine_readable/a2ml-legacy-allow.txt"
LEDGER="$ROOT/$LEDGER_REL"
A2ML_RE='\.a2ml(\.in|\.template|\.v[0-9]+\.template)?$'

[ -d "$ROOT" ] || { echo "ERROR: $ROOT is not a directory" >&2; exit 2; }
[ -f "$LEDGER" ] || { echo "ERROR: ledger $LEDGER_REL not found under $ROOT" >&2; exit 2; }

list_files() {
  if git -C "$ROOT" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    git -C "$ROOT" ls-files -co --exclude-standard
  else
    (cd "$ROOT" && find . -type f ! -path './.git/*' | sed 's|^\./||')
  fi
}

declare -A listed=()
fail=0
lineno=0
while IFS= read -r raw || [ -n "$raw" ]; do
  lineno=$((lineno + 1))
  entry="${raw%%#*}"
  entry="${entry#"${entry%%[![:space:]]*}"}"
  entry="${entry%"${entry##*[![:space:]]}"}"
  [ -z "$entry" ] && continue
  case "$entry" in
    *'*'*|*'?'*|*'['*)
      echo "::error file=$LEDGER_REL,line=$lineno::ledger entry '$entry' is a pattern; list exact paths only"
      fail=1; continue ;;
    9-archive/*)
      echo "::error file=$LEDGER_REL,line=$lineno::ledger entry '$entry' is under 9-archive/, which needs no exemption"
      fail=1; continue ;;
  esac
  if [ -n "${listed[$entry]:-}" ]; then
    echo "::error file=$LEDGER_REL,line=$lineno::ledger entry '$entry' is listed twice"
    fail=1; continue
  fi
  listed[$entry]="$lineno"
done < "$LEDGER"

declare -A present=()
live=0
archived=0
while IFS= read -r f; do
  [ -n "$f" ] || continue
  [ -e "$ROOT/$f" ] || continue          # deleted in the work tree, not yet committed
  case "$f" in
    9-archive/*) archived=$((archived + 1)); continue ;;
  esac
  present[$f]=1
  if [ -n "${listed[$f]:-}" ]; then
    live=$((live + 1))
  else
    echo "::error file=$f::A2ML file outside 9-archive/ and not in $LEDGER_REL. A2ML is retired: write a .deed or .k9.ncl instead, or archive it under 9-archive/a2ml/ at its original path"
    fail=1
  fi
done < <(list_files | grep -E "$A2ML_RE" || true)

for entry in "${!listed[@]}"; do
  if [ -z "${present[$entry]:-}" ]; then
    echo "::error file=$LEDGER_REL,line=${listed[$entry]}::ledger entry '$entry' names no A2ML file; remove it (the ledger only shrinks)"
    fail=1
  fi
done

echo "a2ml archive gate: $live live (ledgered), $archived archived under 9-archive/"
if [ "$fail" -ne 0 ]; then
  echo "a2ml archive gate: FAIL"
  exit 1
fi
echo "a2ml archive gate: OK"
