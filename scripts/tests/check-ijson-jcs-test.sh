#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proves check-ijson-jcs.sh CAN FAIL, and classifies each file for the right
# reason: canonical, not canonical and invalid, for both .json and .jsonl, with
# the JSONC carve-out skipped. Report-only mode must exit 0 whatever it finds;
# --enforce must exit 1 on any finding; a missing tool or path must exit 2.
#
# Needs the ijson-jcs binary on PATH or in $IJSON_JCS.
set -euo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/check-ijson-jcs.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT

pass=0; fail=0
# Run the checker (with any extra flags) over $WORK/t and compare its exit code.
expect() { # expect <wanted-exit> <label> [checker flags...]
  local want="$1" label="$2" got=0; shift 2
  (cd "$WORK" && "$SCRIPT" "$@" t) >"$WORK/out" 2>&1 || got=$?
  if [ "$got" = "$want" ]; then pass=$((pass+1)); echo "  ok    $label"
  else fail=$((fail+1)); echo "  FAIL  $label (wanted exit $want, got $got)"; sed 's/^/        /' "$WORK/out"; fi
}
# Empty the scratch tree.
fresh() { rm -rf "$WORK/t"; mkdir -p "$WORK/t"; }
# Assert the last checker output contains a fixed string.
names() { # names <fixed-string> <label>
  if grep -qF -- "$1" "$WORK/out"; then pass=$((pass+1)); echo "  ok    $2"
  else fail=$((fail+1)); echo "  FAIL  $2"; sed 's/^/        /' "$WORK/out"; fi
}

echo ".json"
fresh; printf '{"a":1,"b":[true,null]}\n' > "$WORK/t/a.json"
expect 0 "a canonical .json passes --enforce" --enforce
names "1 canonical, 0 not canonical, 0 invalid" "it is counted canonical"
fresh; printf '{"b":1,"a":2}\n' > "$WORK/t/a.json"
expect 0 "an unsorted .json is reported, exit 0 (report-only)"
names "NOT CANONICAL t/a.json" "the report names the unsorted file"
expect 1 "an unsorted .json fails --enforce" --enforce
fresh; printf '{\n  "a": 1\n}\n' > "$WORK/t/a.json"
expect 1 "a pretty-printed .json fails --enforce" --enforce
fresh; printf '{"a":1}' > "$WORK/t/a.json"
expect 1 "a .json without a final newline fails --enforce" --enforce
names "NOT CANONICAL t/a.json" "the missing newline is classed non-canonical"
fresh; printf '{"a":1}\n\n' > "$WORK/t/a.json"
expect 1 "a .json with two final newlines fails --enforce" --enforce
fresh; printf '{"a":1,"a":2}\n' > "$WORK/t/a.json"
expect 1 "a duplicate-key .json fails --enforce" --enforce
names "INVALID t/a.json" "the duplicate key is classed invalid, not merely non-canonical"
fresh; printf '{"a":' > "$WORK/t/a.json"
expect 0 "a truncated .json is reported, exit 0 (report-only)"
names "INVALID t/a.json" "the truncated file is classed invalid"

echo ".jsonl"
fresh; printf '{"a":1}\n{"b":[1,2]}\n' > "$WORK/t/a.jsonl"
expect 0 "a canonical .jsonl passes --enforce" --enforce
names "1 canonical" "it is counted canonical"
fresh; printf '{"a":1}\n{"z":1,"b":2}\n' > "$WORK/t/a.jsonl"
expect 1 "a .jsonl whose SECOND line is unsorted fails --enforce" --enforce
names "NOT CANONICAL t/a.jsonl" "the report names the .jsonl"
fresh; printf '{"a":1}\n{"b": 2}\n' > "$WORK/t/a.jsonl"
expect 1 "a .jsonl line with insignificant whitespace fails --enforce" --enforce
fresh; printf '{"a":1}\n{"b":2}' > "$WORK/t/a.jsonl"
expect 1 "a .jsonl without a final newline fails --enforce" --enforce
fresh; printf '{"a":1}\n\n{"b":2}\n' > "$WORK/t/a.jsonl"
expect 1 "a .jsonl with a blank line fails --enforce" --enforce
names "INVALID t/a.jsonl" "the blank line is classed invalid"
fresh; printf '{"a":1}\n{"b":\n' > "$WORK/t/a.jsonl"
expect 1 "a .jsonl with an unparseable line fails --enforce" --enforce
names "INVALID t/a.jsonl" "the unparseable line is classed invalid"

echo "scope"
fresh; mkdir -p "$WORK/t/.devcontainer" "$WORK/t/.vscode"
printf '// SPDX-License-Identifier: MPL-2.0\n{ "name": "x" }\n' > "$WORK/t/.devcontainer/devcontainer.json"
printf '// comment\n{ "b": 1, "a": 2 }\n' > "$WORK/t/.vscode/settings.json"
expect 0 "tool-demanded JSONC is skipped under --enforce" --enforce
names "2 JSONC carve-out(s) skipped" "the skips are counted, not hidden"
fresh; mkdir -p "$WORK/t/sub/deeper"; printf '{"b":1,"a":2}\n' > "$WORK/t/sub/deeper/x.json"
expect 1 "a non-canonical .json in a nested directory fails --enforce" --enforce
fresh; mkdir -p "$WORK/t/node_modules/p" "$WORK/t/.git"
printf '{"b":1,"a":2}\n' > "$WORK/t/node_modules/p/package.json"
printf '{"b":1,"a":2}\n' > "$WORK/t/.git/x.json"
expect 0 "node_modules/ and .git/ are not scanned" --enforce
fresh
expect 0 "an empty tree passes --enforce" --enforce
names "0 file(s) checked" "the empty tree reports zero files"

echo "cannot look"
fresh
got=0; "$SCRIPT" "$WORK/does-not-exist" >"$WORK/out" 2>&1 || got=$?
if [ "$got" = 2 ]; then pass=$((pass+1)); echo "  ok    a missing path exits 2"
else fail=$((fail+1)); echo "  FAIL  a missing path exits 2 (got $got)"; fi
got=0; IJSON_JCS=ijson-jcs-not-installed "$SCRIPT" "$WORK/t" >"$WORK/out" 2>&1 || got=$?
if [ "$got" = 2 ]; then pass=$((pass+1)); echo "  ok    a missing ijson-jcs binary exits 2"
else fail=$((fail+1)); echo "  FAIL  a missing ijson-jcs binary exits 2 (got $got)"; fi

echo "passed $pass, failed $fail"
[ "$fail" -eq 0 ]
