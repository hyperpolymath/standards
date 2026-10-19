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
# on stdout, exit 0 (the {"messa shape AGENTS.md §5 records). A contents/ call
# serves the file planted under $WORK/contents (its path with / ? = as _), or a
# 404 body with exit 1 when none is planted.
mkdir -p "$WORK/contents"
cat > "$WORK/gh" <<EOF
#!/usr/bin/env bash
case "\$2" in
  repos/o/garbled/contents/*) echo '{"messa'; exit 0 ;;
  repos/*/contents/*)
    f="$WORK/contents/\$(printf '%s' "\$2" | tr '/?=' '___')"
    if [ -f "\$f" ]; then cat "\$f"; else echo '{"message":"Not Found","status":"404"}'; exit 1; fi ;;
  repos/o/missing*) echo '{"message":"Not Found","status":"404"}'; exit 1 ;;
  repos/o/garbled*) echo '{"messa'; exit 0 ;;
  repos/o/untagged/tags*) exit 0 ;;
  repos/o/tagerror/tags*) echo 'v1.2.3'; exit 1 ;;
  repos/o/badtag/tags*) echo '{"message":"bad response"}'; exit 0 ;;
  repos/o/laterpage/tags*)
    case " \$* " in *' --paginate '*) echo 'v2.3.4' ;; *) exit 0 ;; esac ;;
  repos/o/a/tags*|repos/o/b/tags*|repos/o/c/tags*) echo 'v1.0.0' ;;
  repos/o/buildmeta/tags*) echo 'v1.2.3+build.4' ;;
  repos/o/releaseat/tags*) echo 'release@2026' ;;
  repos/o/leaf/tags*) printf 'v4\nv4.38.0\n' ;;
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

echo "=== resolves a leaf SHA ref to its upstream semver tag (standards#1271) ==="
d=$(mklock t2b "o/leaf@$A" </dev/null)
run "$d" >/dev/null
grep -qxF "    'o/leaf@$A':" "$d/actions.lock" && grep -qxF "        ref: 'v4.38.0'" "$d/actions.lock" \
  && grep -qxF "        commit: 'sha1-$A'" "$d/actions.lock" \
  && ok "leaf SHA ref records upstream semver tag in ref: ('v4.38.0')" \
  || bad "leaf SHA ref records upstream semver tag in ref:"

echo "=== accepts a version tag with build metadata ==="
d=$(mklock buildmeta "o/buildmeta@$A" </dev/null)
out=$(run "$d"); st=$?
[[ "$st" == 0 ]] && grep -qxF "        ref: 'v1.2.3+build.4'" "$d/actions.lock" \
  && ok "build metadata tag is preserved" || bad "build metadata: st=$st $out"

echo "=== accepts a tag name containing @ ==="
d=$(mklock releaseat "o/releaseat@$A" </dev/null)
out=$(run "$d"); st=$?
[[ "$st" == 0 ]] && grep -qxF "        ref: 'release@2026'" "$d/actions.lock" \
  && ok "@ tag name is preserved" || bad "@ tag name: st=$st $out"

echo "=== leaf tag resolution fails closed ==="
for fixture in untagged tagerror badtag; do
  d=$(mklock "tag-$fixture" "o/$fixture@$A" </dev/null)
  cp "$d/actions.lock" "$WORK/tag-before"
  out=$(run "$d"); st=$?
  [ "$st" = 3 ] && cmp -s "$WORK/tag-before" "$d/actions.lock" \
    && ok "$fixture: exit 3 and no poisoned record" || bad "$fixture: st=$st $out"
done
d=$(mklock laterpage "o/laterpage@$A" </dev/null)
out=$(run "$d"); st=$?
[ "$st" = 0 ] && grep -qxF "        ref: 'v2.3.4'" "$d/actions.lock" \
  && ok "tag beyond first page is resolved" || bad "pagination: st=$st $out"

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

# mkwf <dir> <uses-values...>: a caller workflow ci.yml with one job-level
# reusable call per value.
mkwf() {
  local d="$WORK/$1/.github/workflows"; shift; mkdir -p "$d"
  { echo 'jobs:'; i=0; for u in "$@"; do i=$((i + 1)); printf '  j%s:\n    uses: %s\n' "$i" "$u"; done; } > "$d/ci.yml"
}
# plant <owner/repo> <path> <ref>: serve stdin as that file at that ref.
plant() { cat > "$WORK/contents/$(printf 'repos/%s/contents/%s?ref=%s' "$1" "$2" "$3" | tr '/?=' '___')"; }
# record <lock> <key>: print the record for key, header to last line.
record() { awk -v k="    '$2':" '/^dependencies:/{f=1} f && $0==k {p=1; print; next} f && p && /^    '\''/{p=0} p' "$1"; }

plant o/r .github/workflows/w.yml "$A" <<EOF
on: workflow_call
jobs:
  a:
    runs-on: ubuntu-latest
    steps:
      - uses: o/b@$B
      - uses: "o/c/sub@$C" # a sub-path and a comment
      - uses: ./local-action
      - uses: docker://alpine:3
EOF
WANT_R="    'o/r@$A':
        ref: '$A'
        commit: 'sha1-$A'
        owner_id: 7
        repo_id: 9
        uses:
            - 'o/b@$B'
            - 'o/c@$C'"

echo "=== a reusable-workflow record lists its called workflow's uses: ==="
d=$(mklock t8 "o/r@$A" </dev/null); mkwf t8 "o/r/.github/workflows/w.yml@$A"
out=$(run "$d"); st=$?
[ "$st" = 0 ] && [ "$(record "$d/actions.lock" "o/r@$A")" = "$WANT_R" ] \
  && ok "new reusable record carries the external uses:, keyed owner/repo@ref" || bad "new reusable record carries the external uses: — st=$st $out"
grep -qxF "    'o/b@$B':" "$d/actions.lock" && grep -qxF "    'o/c@$C':" "$d/actions.lock" \
  && ok "the refs it lists are closed" || bad "the refs it lists are closed"
cp "$d/actions.lock" "$WORK/t8.before"; out=$(run "$d")
grep -qF 'dangling refs: 0' <<<"$out" && cmp -s "$WORK/t8.before" "$d/actions.lock" \
  && ok "second run leaves the lock byte-identical" || bad "second run leaves the lock byte-identical — $out"

echo "=== a leaf record left by an earlier run is completed ==="
d=$(mklock t9 "o/r@$A" <<EOF
    'o/r@$A':
        ref: '$A'
        commit: 'sha1-$A'
        owner_id: 7
        repo_id: 9
EOF
)
mkwf t9 "o/r/.github/workflows/w.yml@$A"
out=$(run "$d"); st=$?
[ "$st" = 0 ] && [ "$(record "$d/actions.lock" "o/r@$A")" = "$WANT_R" ] && grep -qF "~ o/r@$A" <<<"$out" \
  && ok "leaf reusable record completed and reported" || bad "leaf reusable record completed and reported — st=$st $out"
grep -qxF "    'o/c@$C':" "$d/actions.lock" && ok "completed record's refs are closed" || bad "completed record's refs are closed"

echo "=== a reusable called from a reusable is read too ==="
plant o/r .github/workflows/w2.yml "$A" <<EOF
jobs:
  inner:
    uses: o/r2/.github/workflows/v.yml@$B
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: o/b@$B
EOF
plant o/r2 .github/workflows/v.yml "$B" <<EOF
jobs:
  a:
    runs-on: ubuntu-latest
    steps:
      - uses: o/c@$C
EOF
d=$(mklock t10 "o/r@$A" </dev/null); mkwf t10 "o/r/.github/workflows/w2.yml@$A"
out=$(run "$d"); st=$?
[ "$st" = 0 ] && record "$d/actions.lock" "o/r@$A" | grep -qxF "            - 'o/r2@$B'" \
  && record "$d/actions.lock" "o/r2@$B" | grep -qxF "            - 'o/c@$C'" \
  && grep -qxF "    'o/c@$C':" "$d/actions.lock" \
  && ok "nested reusable gets its own uses: list, and its refs are closed" || bad "nested reusable gets its own uses: list — st=$st $out"

echo "=== a reusable that uses no external action stays a leaf ==="
plant o/r .github/workflows/plain.yml "$A" <<EOF
jobs:
  a:
    runs-on: ubuntu-latest
    steps:
      - run: echo hi
EOF
d=$(mklock t11 "o/r@$A" </dev/null); mkwf t11 "o/r/.github/workflows/plain.yml@$A"
out=$(run "$d"); st=$?
[ "$st" = 0 ] && grep -qxF "    'o/r@$A':" "$d/actions.lock" && ! grep -qF 'uses:' "$d/actions.lock" \
  && ok "no external uses → leaf record (exit 0)" || bad "no external uses → leaf record — st=$st $out"

echo "=== planted positives: an unreadable called workflow writes and changes nothing ==="
d=$(mklock t12 "o/r@$A" </dev/null); mkwf t12 "o/r/.github/workflows/gone.yml@$A"
out=$(run "$d"); st=$?
[ "$st" = 3 ] && grep -qF "SKIP o/r@$A" <<<"$out" && ! grep -qxF "    'o/r@$A':" "$d/actions.lock" \
  && ok "404 on the called file → SKIP, exit 3, no leaf record" || bad "404 on the called file → SKIP, exit 3, no leaf record — st=$st $out"
d=$(mklock t13 "o/r@$A" <<EOF
    'o/r@$A':
        ref: '$A'
        commit: 'sha1-$A'
        owner_id: 7
        repo_id: 9
EOF
)
mkwf t13 "o/r/.github/workflows/gone.yml@$A"; cp "$d/actions.lock" "$WORK/t13.before"
out=$(run "$d"); st=$?
[ "$st" = 3 ] && cmp -s "$WORK/t13.before" "$d/actions.lock" \
  && ok "unreadable called file leaves an existing record untouched (exit 3)" || bad "unreadable called file leaves an existing record untouched — st=$st $out"
d=$(mklock t14 "o/garbled@$A" </dev/null); mkwf t14 "o/garbled/.github/workflows/w.yml@$A"
out=$(run "$d"); st=$?
[ "$st" = 3 ] && ! grep -qxF "    'o/garbled@$A':" "$d/actions.lock" \
  && ok "malformed called file (exit 0, no jobs:) → SKIP, exit 3" || bad "malformed called file → SKIP, exit 3 — st=$st $out"

echo "=== structural ==="
mkdir -p "$WORK/t7/.github/workflows"
(cd "$WORK/t7" && GH_BIN="$WORK/gh" bash "$TARGET") >/dev/null 2>&1; st=$?
[ "$st" = 1 ] && ok "no lockfile → exit 1" || bad "no lockfile → exit 1 (got $st)"

echo "close-lock-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
