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

# Julia project files: registry-assigned dependency UUIDs are external IDs.
fresh; printf 'name = "P"\nuuid = "%s"\n\n[deps]\nX = "%s"\n\n[weakdeps]\nY = "%s"\n\n[extras]\nZ = "%s"\n' \
  "$V7_8" "$V4" "$V4" "$V4" > "$WORK/t/Project.toml"
expect 0 "v4 dependency UUIDs in Project.toml [deps]/[weakdeps]/[extras] are accepted"

fresh; printf 'name = "P"\nuuid = "%s"\n\n[deps]\nX = "%s"\n' "$V4" "$V7_8" > "$WORK/t/Project.toml"
expect 1 "a v4 package's own uuid in Project.toml is rejected"

fresh; printf 'uuid = "%s"\n[ deps ] # comment\nX = "%s"\n\n[compat]\n\n[sources]\nY = "%s"\n' \
  "$V7_8" "$V4" "$V4" > "$WORK/t/JuliaProject.toml"
expect 1 "the exemption ends at the next table header"

fresh; printf '[deps]\nX = "%s"\n' "$V4" > "$WORK/t/deps.toml"
expect 1 "a [deps] table outside a Julia project file is not exempt"

fresh; mkdir -p "$WORK/t/docs"; printf '[[deps.X]]\nuuid = "%s"\n' "$V4" > "$WORK/t/docs/Manifest-v1.12.toml"
expect 0 "a v4 in a Julia Manifest is accepted"

echo "PASS=$pass FAIL=$fail"
[ "$fail" -eq 0 ]
