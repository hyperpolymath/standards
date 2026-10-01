#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# close-lock-test.sh — fixture suite for scripts/close-lock.sh. Offline: a stub
# `gh` (GH_BIN) answers the two API calls the script makes. The planted
# positives are the two ways a lookup goes wrong — an HTTP error and a body of
# the wrong shape — and both must SKIP with exit 3 and write NO record.
#
# Run: bash scripts/tests/close-lock-test.sh
# TARGET=<path> runs the suite against another copy (used for mutant checks).
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="${TARGET:-$SCRIPT_DIR/../close-lock.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
pass=0; fail=0
A=1111111111111111111111111111111111111111
B=2222222222222222222222222222222222222222
C=3333333333333333333333333333333333333333

# ok: record and print a passing assertion.
ok()  { echo "PASS: $1"; pass=$((pass + 1)); }
# bad: record and print a failing assertion.
bad() { echo "FAIL: $1"; fail=$((fail + 1)); }

# Stub: repos/<o>/<n> → "7 9"; commits/v1 → $C; anything under o/missing → an
# HTTP error body on stdout, exit 1; anything under o/garbled → a truncated body
# on stdout, exit 0 (the {"messa shape AGENTS.md §5 records).
cat > "$WORK/gh" <<EOF
#!/usr/bin/env bash
case "\$2" in
  repos/o/missing*) echo '{"message":"Not Found","status":"404"}'; exit 1 ;;
  repos/o/garbled*) echo '{"messa'; exit 0 ;;
  repos/o/*/commits/v1) echo $C ;;
  repos/o/*/commits/*) echo '{"message":"No commit found"}'; exit 1 ;;
  repos/o/*) echo '7 9' ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$WORK/gh"

# mklock <dir> <workflows-section-items...>; dependencies come from stdin
mklock() {
  local d="$WORK/$1/.github/workflows"; shift; mkdir -p "$d"
  { echo 'workflows:'; echo "    '.github/workflows/ci.yml':"
    for r in "$@"; do echo "        - '$r'"; done
    echo 'dependencies:'; cat; } > "$d/actions.lock"
  printf '%s' "$d"
}
# run: execute the target from the fixture repo root with the stub gh binary.
run() { (cd "$1/../.." && GH_BIN="$WORK/gh" bash "$TARGET") 2>&1; }

echo "=== closes a dangling workflows: edge ==="
d=$(mklock t1 "o/a@$A" </dev/null)
out=$(run "$d"); st=$?
[ "$st" = 0 ] && grep -qxF "    'o/a@$A':" "$d/actions.lock" && grep -qxF "        commit: 'sha1-$A'" "$d/actions.lock" \
  && grep -qxF '        owner_id: 7' "$d/actions.lock" && ok "SHA ref gets a record (exit 0)" || bad "SHA ref gets a record — st=$st $out"
out=$(run "$d"); grep -qF 'dangling refs: 0' <<<"$out" && ok "second run is a no-op" || bad "second run is a no-op — $out"

echo "=== resolves a tag through the commits endpoint ==="
d=$(mklock t2 "o/b@v1" </dev/null)
run "$d" >/dev/null
grep -qxF "        ref: 'v1'" "$d/actions.lock" && grep -qxF "        commit: 'sha1-$C'" "$d/actions.lock" \
  && ok "tag ref recorded with its resolved commit" || bad "tag ref recorded with its resolved commit"

echo "=== closes a nested edge of an existing record ==="
d=$(mklock t3 "o/a@$A" <<EOF
    'o/a@$A':
        ref: '$A'
        commit: 'sha1-$A'
        owner_id: 7
        repo_id: 9
        uses:
            - 'o/b@$B'
EOF
)
run "$d" >/dev/null
grep -qxF "    'o/b@$B':" "$d/actions.lock" && ok "nested ref gets a record" || bad "nested ref gets a record"
[ "$(grep -oE "^    'o/[^']+'" "$d/actions.lock" | tr -d " '" | tr '\n' ,)" = "o/a@$A,o/b@$B," ] \
  && ok "records stay in key order" || bad "records stay in key order"

echo "=== planted positives: a failed or malformed lookup writes nothing ==="
d=$(mklock t4 "o/missing@$A" </dev/null); out=$(run "$d"); st=$?
[ "$st" = 3 ] && grep -qF 'SKIP o/missing' <<<"$out" && ! grep -qF 'message' "$d/actions.lock" \
  && ok "HTTP error → SKIP, exit 3, no record" || bad "HTTP error → SKIP, exit 3, no record — st=$st $out"
d=$(mklock t5 "o/garbled@$A" </dev/null); out=$(run "$d"); st=$?
[ "$st" = 3 ] && ! grep -qF 'messa' "$d/actions.lock" \
  && ok "malformed body (exit 0) → SKIP, exit 3, no record" || bad "malformed body (exit 0) → SKIP, exit 3, no record — st=$st"
d=$(mklock t6 "o/c@nosuchtag" </dev/null); out=$(run "$d"); st=$?
[ "$st" = 3 ] && ! grep -qF "o/c@nosuchtag':" "$d/actions.lock" \
  && ok "unresolvable tag → SKIP, exit 3, no record" || bad "unresolvable tag → SKIP, exit 3, no record — st=$st"

echo "=== structural ==="
mkdir -p "$WORK/t7/.github/workflows"
(cd "$WORK/t7" && GH_BIN="$WORK/gh" bash "$TARGET") >/dev/null 2>&1; st=$?
[ "$st" = 1 ] && ok "no lockfile → exit 1" || bad "no lockfile → exit 1 (got $st)"

echo "close-lock-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
