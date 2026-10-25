#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-a2ml-archived.sh — A2ML may not sit outside the archive.
#
# A2ML is retired. The only legal home for a tracked .a2ml or .a2ml.template
# is under 9-archive/. The reserved shelf is 9-archive/a2ml/. Files already
# inside another 9-archive last-copy stay there; they are not live standards.
#
# Exit: 0 clean · 1 a live A2ML path was found

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

bad=0
while IFS= read -r f; do
  [ -z "$f" ] && continue
  case "$f" in
    9-archive/*) continue ;;
  esac
  echo "❌ $f: A2ML outside 9-archive/ — move it under 9-archive/a2ml/ (retired; not a live standard)"
  bad=$((bad + 1))
done < <(git ls-files '*.a2ml' '*.a2ml.template')

# A directory named a2ml outside the archive is the same confusion.
while IFS= read -r d; do
  [ -z "$d" ] && continue
  case "$d" in
    9-archive/*) continue ;;
  esac
  echo "❌ $d: directory named a2ml outside 9-archive/"
  bad=$((bad + 1))
done < <(git ls-files | awk -F/ '{
  p = ""
  for (i = 1; i < NF; i++) {
    p = (p == "" ? $i : p "/" $i)
    if ($i == "a2ml") print p
  }
}' | sort -u)

if [ "$bad" -eq 0 ]; then
  echo "✅ no A2ML outside 9-archive/"
  exit 0
fi
echo "A2ML archive check failed: $bad path(s)."
exit 1
