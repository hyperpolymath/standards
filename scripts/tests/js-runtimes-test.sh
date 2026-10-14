#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Proves scripts/js-runtimes.sh CAN FAIL, for the right reason, under each of
# bun, pnpm and deno: a regenerated package passes; a non-canonical
# package.json, a missing lock, a stale lock and a failing test script each
# fail; a missing tool is "cannot run" (exit 2), never a pass.
#
# Needs bun, pnpm, deno and ijson-jcs (PATH or $IJSON_JCS), and the npm
# registry: the fixture depends on two tiny zero-dependency packages, because
# deno.lock does not record file: dependencies, so an offline fixture could
# never show a stale deno.lock.
set -uo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/js-runtimes.sh"
IJSON_JCS="${IJSON_JCS:-ijson-jcs}"
for t in bun pnpm deno "$IJSON_JCS"; do
  command -v "$t" >/dev/null 2>&1 || { echo "js-runtimes-test: $t is not on PATH (cannot run, not a pass)" >&2; exit 2; }
done
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT

pass=0; fail=0
expect() { # expect <wanted-exit> <label> <js-runtimes.sh args...>
  local want="$1" label="$2" got=0; shift 2
  (cd "$WORK/p" && bash "$SCRIPT" "$@") >"$WORK/out" 2>&1 || got=$?
  if [ "$got" = "$want" ]; then pass=$((pass+1)); echo "  ok    $label"
  else fail=$((fail+1)); echo "  FAIL  $label (wanted exit $want, got $got)"; sed 's/^/        /' "$WORK/out"; fi
}
names() { # names <fixed-string> <label>
  if grep -qF -- "$1" "$WORK/out"; then pass=$((pass+1)); echo "  ok    $2"
  else fail=$((fail+1)); echo "  FAIL  $2"; sed 's/^/        /' "$WORK/out"; fi
}
fixture() { # fixture <package.json text>
  rm -rf "$WORK/p"; mkdir -p "$WORK/p"; printf '%s\n' "$1" > "$WORK/p/package.json"
}
# Pretty-printed (not JCS) on purpose: regen must canonicalise it.
PKG='{
  "name": "fixture",
  "version": "0.0.0",
  "private": true,
  "scripts": { "test": "echo js-runtimes-test-ran" },
  "dependencies": { "is-number": "7.0.0" }
}'
STALE='{"dependencies":{"is-number":"7.0.0","isarray":"2.0.5"},"name":"fixture","private":true,"scripts":{"test":"echo js-runtimes-test-ran"},"version":"0.0.0"}'

echo "regen"
fixture "$PKG"
expect 0 "regen canonicalises and writes all three locks" regen
for f in bun.lock pnpm-lock.yaml deno.lock; do
  if [ -f "$WORK/p/$f" ]; then pass=$((pass+1)); echo "  ok    regen wrote $f"
  else fail=$((fail+1)); echo "  FAIL  regen did not write $f"; fi
done
if "$IJSON_JCS" check "$WORK/p/package.json" >/dev/null 2>&1; then pass=$((pass+1)); echo "  ok    package.json is JCS after regen"
else fail=$((fail+1)); echo "  FAIL  package.json is not JCS after regen"; fi
cp -r "$WORK/p" "$WORK/fresh"
reset() { rm -rf "$WORK/p"; cp -r "$WORK/fresh" "$WORK/p"; rm -rf "$WORK/p/node_modules"; }

for rt in bun pnpm deno; do
  echo "check $rt"
  reset
  expect 0 "$rt: a regenerated package passes" check "$rt"
  names "js-runtimes-test-ran" "$rt: the test script ran under $rt"

  reset; printf '%s\n' "$PKG" > "$WORK/p/package.json"
  expect 1 "$rt: a non-canonical package.json fails" check "$rt"
  names "not JCS-canonical" "$rt: and says why"

  reset; rm -f "$WORK/p/$(case $rt in bun) echo bun.lock;; pnpm) echo pnpm-lock.yaml;; deno) echo deno.lock;; esac)"
  expect 1 "$rt: a missing lock fails" check "$rt"
  names "is missing" "$rt: and says why"

  reset; printf '%s\n' "$STALE" > "$WORK/p/package.json"
  expect 1 "$rt: a lock stale against package.json fails" check "$rt"
  names "is stale" "$rt: and says why"

  reset; "$IJSON_JCS" canon < "$WORK/fresh/package.json" | sed 's/echo js-runtimes-test-ran/exit 3/' > "$WORK/p/package.json"
  expect 1 "$rt: a failing test script fails" check "$rt"
  names "the test script failed" "$rt: and says why"
done

echo "cannot run"
reset
IJSON_JCS=/nonexistent/ijson-jcs expect 2 "a missing ijson-jcs is exit 2, not a pass" check bun
expect 2 "an unknown runtime is exit 2" check yarn
reset; : > "$WORK/p/bun.lockb"
expect 1 "regen refuses a binary bun.lockb" regen

echo
echo "js-runtimes-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
