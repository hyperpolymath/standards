#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Tests for scripts/filter-sarif-by-scope.sh.
#
# Weighted towards the failures that would be INVISIBLE: over-exclusion (a real
# finding dropped by a prefix that should not have matched) and a filter that
# exits 0 having silently written nothing. A filter that under-excludes is
# merely noisy and is covered by one positive case.
#
# Run: bash scripts/tests/filter-sarif-by-scope-test.sh
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SCRIPT="$ROOT/scripts/filter-sarif-by-scope.sh"

PASS=0; FAIL=0
ok()  { printf '  PASS  %s\n' "$1"; PASS=$((PASS + 1)); }
bad() { printf '  FAIL  %s -- %s\n' "$1" "${2:-}"; FAIL=$((FAIL + 1)); }
check(){ if eval "$2"; then ok "$1"; else bad "$1" "${3:-}"; fi; }

TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

mk_sarif() { # mk_sarif FILE  (uri tab ...)
  local out="$1"; shift
  local results="[]"
  for uri in "$@"; do
    [ "$uri" = "NONE" ] && uri=""
    results=$(printf '%s' "$results" | jq --arg u "$uri" '. + [{
      ruleId: "hypatia/content_patterns/http_in_docs",
      level: "warning",
      message: {text: "t"},
      locations: (if $u == "" then [] else [{physicalLocation: {artifactLocation: {uri: $u}}}] end)
    }]')
  done
  jq -n --argjson r "$results" '{version: "2.1.0", runs: [{tool: {driver: {name: "Hypatia"}}, results: $r}]}' > "$out"
}

uris_of() { jq -r '[.runs[].results[].locations[]?.physicalLocation.artifactLocation.uri] | join(",")' "$1"; }

echo "== a prefix removes exactly the results under it =="
mk_sarif "$TMP/a.sarif" "9-archive/x.adoc" "9-archive/deep/y.adoc" "scripts/a.sh" "docs/b.adoc"
bash "$SCRIPT" "$TMP/a.sarif" "$TMP/a.out" "9-archive/" >/dev/null 2>&1
check "only the two archive results remain out" "[ \"$(uris_of "$TMP/a.out")\" = 'scripts/a.sh,docs/b.adoc' ]" "$(uris_of "$TMP/a.out")"

echo "== prefix matching is on a path boundary, not a substring =="
mk_sarif "$TMP/b.sarif" "9-archive-not-really/x.adoc" "scripts/9-archive/x.sh" "9-archive/x.adoc"
bash "$SCRIPT" "$TMP/b.sarif" "$TMP/b.out" "9-archive/" >/dev/null 2>&1
check "a sibling directory named 9-archive-not-really is KEPT" \
  "uris_of '$TMP/b.out' | grep -q '9-archive-not-really/x.adoc'"
check "a file merely containing the word 9-archive is KEPT" \
  "uris_of '$TMP/b.out' | grep -q 'scripts/9-archive/x.sh'"
check "the real archive path is removed" \
  "! uris_of '$TMP/b.out' | grep -qx '9-archive/x.adoc'"

echo "== leading ./ and / are normalised =="
mk_sarif "$TMP/c.sarif" "./9-archive/x.adoc" "/9-archive/y.adoc" "9-archive/z.adoc"
bash "$SCRIPT" "$TMP/c.sarif" "$TMP/c.out" "9-archive/" >/dev/null 2>&1
check "all three spellings are removed" "[ -z \"$(uris_of "$TMP/c.out")\" ]" "$(uris_of "$TMP/c.out")"

echo "== a location-less result is KEPT (cannot be shown out of scope) =="
mk_sarif "$TMP/d.sarif" "NONE" "9-archive/x.adoc"
bash "$SCRIPT" "$TMP/d.sarif" "$TMP/d.out" "9-archive/" >/dev/null 2>&1
check "the aggregate result survives" \
  "[ \"$(jq '[.runs[].results[]] | length' "$TMP/d.out")\" = 1 ]"
check "...and it is the location-less one" \
  "[ \"$(jq -r '[.runs[].results[] | (.locations|length)] | @csv' "$TMP/d.out" | tr -d '\"')\" = '0' ]"

echo "== multiple prefixes =="
mk_sarif "$TMP/e.sarif" "9-archive/a.adoc" "vendor/b.go" "src/c.rs"
bash "$SCRIPT" "$TMP/e.sarif" "$TMP/e.out" "9-archive/" "vendor/" >/dev/null 2>&1
check "both prefixes apply" "[ \"$(uris_of "$TMP/e.out")\" = 'src/c.rs' ]" "$(uris_of "$TMP/e.out")"

echo "== fail-open =="
printf 'not json at all' > "$TMP/bad.sarif"
bash "$SCRIPT" "$TMP/bad.sarif" "$TMP/bad.out" "9-archive/" >/dev/null 2>&1
check "unparseable input is copied through unchanged" "cmp -s '$TMP/bad.sarif' '$TMP/bad.out'"
check "...with exit 0 (an upload of unreadable input is a later failure, not this one)" \
  "bash '$SCRIPT' '$TMP/bad.sarif' '$TMP/bad2.out' '9-archive/' >/dev/null 2>&1"

echo "== the output is always a complete SARIF document =="
mk_sarif "$TMP/f.sarif" "9-archive/x.adoc"
bash "$SCRIPT" "$TMP/f.sarif" "$TMP/f.out" "9-archive/" >/dev/null 2>&1
check "runs array survives even when every result is dropped" \
  "jq -e '.runs | type == \"array\" and length == 1' '$TMP/f.out' >/dev/null"
check "the tool driver is preserved" \
  "jq -e '.runs[0].tool.driver.name == \"Hypatia\"' '$TMP/f.out' >/dev/null"

echo "== usage =="
check "no prefixes exits 2" "bash '$SCRIPT' '$TMP/a.sarif' '$TMP/z.out' >/dev/null 2>&1; [ \$? -eq 2 ]"
check "missing arguments exits 2" "bash '$SCRIPT' >/dev/null 2>&1; [ \$? -eq 2 ]"

echo "== an empty input is not an error =="
: > "$TMP/empty.sarif"
check "empty SARIF exits 0" "bash '$SCRIPT' '$TMP/empty.sarif' '$TMP/empty.out' '9-archive/' >/dev/null 2>&1"

printf '\n%s passed, %s failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
