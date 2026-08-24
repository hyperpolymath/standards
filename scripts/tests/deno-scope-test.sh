#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

mkdir -p "$WORK/scripts" "$WORK/lol"
cp "$ROOT/deno.json" "$WORK/deno.json"
printf 'const clean = 1;\nconsole.log(clean);\n' > "$WORK/scripts/check-ts-allowlist.ts"
printf 'const ignored: any = 1\n' > "$WORK/lol/product.ts"

(cd "$WORK" && deno lint >/dev/null)
echo "PASS: nested product source does not expand root lint scope"

printf 'const broken: any = 1;\n' > "$WORK/scripts/check-ts-allowlist.ts"
if (cd "$WORK" && deno lint >/dev/null 2>&1); then
  echo "FAIL: root-owned lint violation did not block" >&2
  exit 1
fi
echo "PASS: root-owned lint violation blocks"

printf 'const clean = 1;\nconsole.log(clean);\n' > "$WORK/scripts/check-ts-allowlist.ts"
(cd "$WORK" && deno test --permit-no-files >/dev/null)
echo "PASS: explicit empty test scope does not discover nested product tests"
