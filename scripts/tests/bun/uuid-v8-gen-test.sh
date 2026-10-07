#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proves scripts/uuid-v8.js emits ADR-008 ids, against an oracle that does not
# share its code: profile C is recomputed here with sha256sum and shell nibble
# arithmetic, so a WebCrypto/CryptoHasher bug cannot agree with itself.
#
# Lives below scripts/tests/ so run-shell-test-suite.sh (maxdepth 1, no bun
# on that runner) does not discover it; deed-conformance.yml runs it after
# setup-bun.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
GEN="$ROOT/scripts/uuid-v8.js"
CHECK="$ROOT/scripts/check-uuid-v8.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
pass=0; fail=0

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

echo "profile C matches the independent oracle"
while IFS='|' read -r d n; do
  want=$(oracle_c "$d" "$n"); got=$(bun "$GEN" c "$d" "$n")
  check "c $d:$n -> $want" [ "$got" = "$want" ]
done <<'EOF'
gv-clade-index|github.com/hyperpolymath/standards
berrywiki-import|Main Page
d|
d|a:b c
EOF
got=$(bun "$GEN" deed "gv-clade-index:github.com/hyperpolymath/standards")
check "deed body splits on the FIRST colon" [ "$got" = "$(oracle_c gv-clade-index github.com/hyperpolymath/standards)" ]

echo "profile C rejects bad domains"
for d in "" "a:b" "a b"; do
  rc=0; bun "$GEN" c "$d" x >/dev/null 2>&1 || rc=$?
  check "domain '$d' is rejected" [ "$rc" -eq 1 ]
done

echo "profile T"
before=$(( $(date +%s%3N) - 1000 ))
t=$(bun "$GEN" t)
after=$(( $(date +%s%3N) + 1000 ))
check "version nibble is 8 ($t)" [ "${t:14:1}" = 8 ]
check "variant is 10" grep -qE '^[89ab]$' <<<"${t:19:1}"
ms=$(( 0x${t:0:8}${t:9:4} ))
check "timestamp is now (unix ms)" test "$ms" -ge "$before" -a "$ms" -le "$after"
check "two calls differ" [ "$t" != "$(bun "$GEN" t)" ]

echo "the strict checker accepts every generated id"
{ bun "$GEN" t; bun "$GEN" c gv-clade-index github.com/o/n; } > "$WORK/ids.txt"
check "check-uuid-v8.sh --strict passes" "$CHECK" --strict "$WORK"

echo "PASS=$pass FAIL=$fail"
[ "$fail" -eq 0 ]
