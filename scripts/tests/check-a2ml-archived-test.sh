#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Proves check-a2ml-archived.sh fails when a tracked .a2ml sits outside 9-archive/.
set -euo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/check-a2ml-archived.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
git -C "$WORK" init -q
git -C "$WORK" config user.email t@example.com
git -C "$WORK" config user.name T
mkdir -p "$WORK/9-archive/a2ml"
printf '# archived\n' > "$WORK/9-archive/a2ml/keep.a2ml"
git -C "$WORK" add -A
git -C "$WORK" commit -q -m base
( cd "$WORK" && bash "$SCRIPT" ) >/dev/null

printf '# live\n' > "$WORK/CLAIMS.a2ml"
git -C "$WORK" add CLAIMS.a2ml
set +e
( cd "$WORK" && bash "$SCRIPT" ) >/dev/null 2>&1
got=$?
set -e
if [ "$got" -ne 1 ]; then
  echo "FAIL a live .a2ml should exit 1, got $got"
  exit 1
fi
echo "ok a2ml archive guard can fail and can pass"
