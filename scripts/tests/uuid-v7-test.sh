#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proves check-uuid-v7.sh CAN FAIL, and fails for the right reasons.
#
# The checker runs over this very repository (uuid-v7.yml), so every UUID in
# this file is ASSEMBLED AT RUNTIME from fragments. A literal non-v7 UUID here
# would turn the real gate red on its own test.
set -euo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/check-uuid-v7.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT

uuid() { printf '%s-%s-%s-%s-%s' "$@"; }
V7_8="$(uuid 01890a5d ac96 774b 8cce b302099a8057)"   # version 7, variant 8
V7_B="$(uuid 01890A5D AC96 774B BCCE B302099A8057)"   # version 7, variant b, upper case
V4="$(uuid 3f2504e0 4f89 41d3 9a0c 0305e82c3301)"     # version 4
V7_C="$(uuid 01890a5d ac96 774b ccce b302099a8057)"   # version 7, NON-RFC variant c

pass=0; fail=0
expect() { # expect <wanted-exit> <label>   (runs the checker over $WORK/t)
  local want="$1" label="$2" got=0
  bash "$SCRIPT" "$WORK/t" >"$WORK/out" 2>&1 || got=$?
  if [ "$got" = "$want" ]; then pass=$((pass+1)); echo "  ok    $label"
  else fail=$((fail+1)); echo "  FAIL  $label (wanted exit $want, got $got)"; sed 's/^/        /' "$WORK/out"; fi
}
fresh() { rm -rf "$WORK/t"; mkdir -p "$WORK/t"; }

fresh; printf 'id = "%s"\n' "$V7_8" > "$WORK/t/a.toml"
expect 0 "a v7 UUID (variant 8) is accepted"

fresh; printf 'id: %s\n' "$V7_B" > "$WORK/t/a.yml"
expect 0 "an upper-case v7 UUID (variant b) is accepted"

fresh; printf 'no identifiers here\n' > "$WORK/t/a.txt"
expect 0 "a file with no UUID is accepted"

fresh
expect 0 "an empty tree is accepted"

fresh; printf 'id = "%s"\n' "$V4" > "$WORK/t/a.toml"
expect 1 "a v4 UUID is rejected"
grep -q "a.toml: non-v7 UUID literal ($V4)" "$WORK/out" \
  && { pass=$((pass+1)); echo "  ok    the rejection names the file and the UUID"; } \
  || { fail=$((fail+1)); echo "  FAIL  the rejection does not name the file and the UUID"; }

fresh; printf 'id = "%s"\n' "$V7_C" > "$WORK/t/a.toml"
expect 1 "version 7 with a non-RFC variant nibble is rejected"

fresh; printf 'ok = "%s"\nbad = "%s"\n' "$V7_8" "$V4" > "$WORK/t/a.toml"
expect 1 "a v4 on a later line than a v7 is still rejected"

fresh; mkdir -p "$WORK/t/sub/deeper"; printf '%s\n' "$V4" > "$WORK/t/sub/deeper/x.json"
expect 1 "a v4 in a nested directory is rejected"

fresh; mkdir -p "$WORK/t/.git"; printf '%s\n' "$V4" > "$WORK/t/.git/packed-refs"
expect 0 "a v4 inside .git/ is ignored"

fresh; printf '\000%s\n' "$V4" > "$WORK/t/blob.bin"
expect 0 "a v4 inside a binary file is ignored"

echo "PASS=$pass FAIL=$fail"
[ "$fail" -eq 0 ]
