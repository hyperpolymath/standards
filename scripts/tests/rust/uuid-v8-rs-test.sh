#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proves tools/uuid-v8 (the shared Rust ESTATE-UUID-V8 generator, ADR-008)
# emits the same ids as the independent oracle in
# scripts/tests/bun/uuid-v8-gen-test.sh (sha256sum and shell nibble
# arithmetic), the same ids as scripts/uuid-v8.js when bun is present, and ids
# that check-uuid-v8.sh --strict accepts.
#
# Lives below scripts/tests/ so run-shell-test-suite.sh (maxdepth 1) does not
# discover it; deed-conformance.yml runs it next to the bun generator test.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
MANIFEST="$ROOT/tools/uuid-v8/Cargo.toml"
CHECK="$ROOT/scripts/check-uuid-v8.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
pass=0; fail=0

cargo build --quiet --release --locked --manifest-path "$MANIFEST"
GEN="$ROOT/tools/uuid-v8/target/release/uuid-v8"

# Record one assertion: check <label> <command...>; runs the command errexit-safe.
check() {
  local label=$1; shift
  if "$@"; then pass=$((pass+1)); echo "  ok    $label"; else fail=$((fail+1)); echo "  FAIL  $label"; fi
}

# Independent profile C: first 16 bytes of SHA-256(domain:name), version and variant set by hand.
oracle_c() {
  local h v
  h=$(printf '%s:%s' "$1" "$2" | sha256sum | cut -c1-32)
  v=$(( (0x${h:16:1} & 0x3) | 0x8 ))
  h="${h:0:12}8${h:13:3}$(printf '%x' "$v")${h:17:15}"
  printf '%s-%s-%s-%s-%s\n' "${h:0:8}" "${h:8:4}" "${h:12:4}" "${h:16:4}" "${h:20:12}"
}

CASES='gv-clade-index|github.com/hyperpolymath/standards
berrywiki-import|Main Page
d|
d|a:b c
echidna-goal|é✓'

echo "profile C matches the independent oracle"
while IFS='|' read -r d n; do
  want=$(oracle_c "$d" "$n"); got=$("$GEN" c "$d" "$n")
  check "c $d:$n -> $want" [ "$got" = "$want" ]
done <<<"$CASES"
got=$("$GEN" deed "gv-clade-index:github.com/hyperpolymath/standards")
check "deed body splits on the FIRST colon" [ "$got" = "$(oracle_c gv-clade-index github.com/hyperpolymath/standards)" ]

if command -v bun >/dev/null 2>&1; then
  echo "profile C agrees with scripts/uuid-v8.js"
  while IFS='|' read -r d n; do
    check "rust = bun for $d:$n" [ "$("$GEN" c "$d" "$n")" = "$(bun "$ROOT/scripts/uuid-v8.js" c "$d" "$n")" ]
  done <<<"$CASES"
else
  echo "  skip  bun not installed: rust/bun agreement not checked"
fi

echo "profile C rejects bad domains"
for d in "" "a:b" "a b"; do
  rc=0; "$GEN" c "$d" x >/dev/null 2>&1 || rc=$?
  check "domain '$d' is rejected" [ "$rc" -eq 1 ]
done
rc=0; "$GEN" bogus >/dev/null 2>&1 || rc=$?
check "bad usage exits 2" [ "$rc" -eq 2 ]

echo "profile T"
before=$(( $(date +%s%3N) - 1000 ))
t=$("$GEN" t)
after=$(( $(date +%s%3N) + 1000 ))
check "version nibble is 8 ($t)" [ "${t:14:1}" = 8 ]
check "variant is 10" grep -qE '^[89ab]$' <<<"${t:19:1}"
ms=$(( 0x${t:0:8}${t:9:4} ))
check "timestamp is now (unix ms)" test "$ms" -ge "$before" -a "$ms" -le "$after"
check "two calls differ" [ "$t" != "$("$GEN" t)" ]

echo "the strict checker accepts every generated id"
{ "$GEN" t; "$GEN" c gv-clade-index github.com/o/n; } > "$WORK/ids.txt"
check "check-uuid-v8.sh --strict passes" "$CHECK" --strict "$WORK"

echo "PASS=$pass FAIL=$fail"
[ "$fail" -eq 0 ]
