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
# The isolated fixture above proves the rule can fire. The checked-out
# repository also must pass, and a mutation of ITS actual index must be caught.
ROOT="$(git -C "$(dirname "$SCRIPT")" rev-parse --show-toplevel)"
( cd "$ROOT" && bash "$SCRIPT" ) >/dev/null || {
  echo "FAIL: actual checked-out repository violates the A2ML archive policy"
  exit 1
}
# Overlay a temporary index; never modify the working tree or real git index.
INDEX="$WORK/checked-out-index"
GIT_INDEX_FILE="$INDEX" git -C "$ROOT" read-tree HEAD
BLOB="$(git -C "$ROOT" rev-parse HEAD:README.adoc)"
GIT_INDEX_FILE="$INDEX" git -C "$ROOT" update-index --add \
  --cacheinfo "100644,$BLOB,gate-firing-probe.a2ml"
got=0
( cd "$ROOT" && GIT_INDEX_FILE="$INDEX" bash "$SCRIPT" ) >/dev/null 2>&1 || got=$?
if [ "$got" -ne 1 ]; then
  echo "FAIL: adding a tracked A2ML file to the real repo's temporary index returned $got, expected 1"
  exit 1
fi
echo "ok checked-out tree is clean and an index-only live A2ML mutation is rejected"
echo "ok a2ml archive guard can fail and can pass"
