#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Reject GitHub Actions workflows containing duplicate YAML keys.
#
# WHY THIS EXISTS AS A SEPARATE CHECK
# -----------------------------------
# GitHub Actions rejects a workflow with duplicate keys outright. The run is
# recorded as `failure` with NO jobs, NO log and NO check run — a red mark on
# the board with nothing behind it to read, and `gh pr checks` shows no row.
#
# Nothing else in the toolchain sees this, because ordinary YAML parsers
# SILENTLY KEEP THE LAST duplicate and report success. The file "parses".
# Linters, formatters and sweep scripts are all structurally blind to it.
#
# Measured 2026-08-05: nine workflows in `hypatia` were in this state,
# including a CodeQL workflow with 18 failures, 12 startup_failures and ZERO
# successes in its lifetime — the repository had never once been scanned by
# its own scanner.
#
# WHY SHELL. This check first shipped in Python, which estate language policy
# bans with no exceptions, so it blocked the very pull requests it was meant to
# protect. A Deno port was the obvious second choice — the language-policy gate
# has that precedent — but Deno is being retired from the estate, so it would
# have been a port onto a disappearing runtime. Shell is what 29 of the 33
# scripts here already use, needs no runtime beyond awk, and adds no setup step
# to any job.
#
# WHY A STRUCTURAL SCANNER AND NOT A PARSER. A parser that rejects duplicates
# is what is wanted, but the available YAML libraries do the opposite by
# design: they resolve duplicates silently, which is the whole reason this
# check exists. So this walks the document tracking sibling keys per mapping
# level. The constructs that would otherwise produce FALSE positives are
# handled explicitly, because a duplicate-key checker that cries wolf gets
# switched off:
#
#   * BLOCK SCALARS — a `run: |` body routinely contains `foo: bar` shell
#     lines that are not YAML keys at all.
#   * LIST ITEMS — every step in a job legitimately repeats `name:`.
#   * DOCUMENT SEPARATORS — `---` starts a fresh sibling scope.
#   * QUOTED KEYS — `"on":` and `on:` are the same key.
set -uo pipefail

scan_one() {
  awk '
    function clear_deeper(ind,   k) {
      for (k in seen) { split(k, p, SUBSEP); if (p[1] + 0 > ind) delete seen[k] }
    }
    function clear_from(ind,   k) {
      for (k in seen) { split(k, p, SUBSEP); if (p[1] + 0 >= ind) delete seen[k] }
    }
    BEGIN { block = -1; dupes = 0 }
    {
      line = $0
      sub(/\r$/, "", line)
      # indent width
      match(line, /^ */); ind = RLENGTH
      trimmed = line; sub(/^ +/, "", trimmed); sub(/ +$/, "", trimmed)

      # inside a block scalar: body text, not keys
      if (block >= 0) {
        if (trimmed == "" || ind > block) next
        block = -1
      }
      if (trimmed == "" || substr(trimmed, 1, 1) == "#") next

      if (trimmed == "---") { delete seen; next }
      if (trimmed == "...") { delete seen; next }

      # a list item opens a fresh mapping scope
      if (substr(trimmed, 1, 2) == "- " || trimmed == "-") {
        clear_from(ind)
        rest = substr(trimmed, 3)
        if (match(rest, /^("[^"]*"|\x27[^\x27]*\x27|[^ #][^:]*):( |$)/)) {
          key = substr(rest, RSTART, RLENGTH)
          sub(/:.*$/, "", key); gsub(/^["\x27]|["\x27]$/, "", key)
          seen[ind + 2, key] = 1
        }
        if (match(trimmed, /:[ ]*[|>][-+0-9]*$/)) block = ind
        next
      }

      if (!match(trimmed, /^("[^"]*"|\x27[^\x27]*\x27|[^ #][^:]*):( |$)/)) {
        if (match(trimmed, /:[ ]*[|>][-+0-9]*$/)) block = ind
        next
      }
      key = substr(trimmed, RSTART, RLENGTH)
      sub(/:.*$/, "", key); gsub(/^["\x27]|["\x27]$/, "", key)
      sub(/ +$/, "", key)

      clear_deeper(ind)
      if ((ind, key) in seen) { printf "%s|%d\n", key, NR; dupes++ }
      seen[ind, key] = 1

      if (match(trimmed, /:[ ]*[|>][-+0-9]*$/)) block = ind
    }
    END { exit (dupes > 0 ? 1 : 0) }
  ' "$1"
}

targets=("$@")
[ ${#targets[@]} -eq 0 ] && targets=(".github/workflows")

files=()
for t in "${targets[@]}"; do
  if [ -d "$t" ]; then
    while IFS= read -r f; do files+=("$f"); done < <(
      find "$t" -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) | sort)
  elif [ -e "$t" ]; then
    files+=("$t")
  fi
done

# Succeed when the file is a flow-style (KYAML) document: its first line that is
# not blank, a comment or a `---` marker opens a `{` mapping or `[` sequence.
is_flow_document() {
  awk '
    { line = $0; sub(/\r$/, "", line); sub(/^[ \t]+/, "", line) }
    line == "" || substr(line, 1, 1) == "#" || line ~ /^---[ \t]*$/ { next }
    { exit (substr(line, 1, 1) == "{" || substr(line, 1, 1) == "[") ? 0 : 1 }
    END { if (NR == 0) exit 1 }
  ' "$1"
}

# WHY FLOW DOCUMENTS ARE NORMALISED FIRST. scan_one walks BLOCK structure by
# indentation; a KYAML file (YAML-POLICY Y-3) puts every sibling on its own
# line inside `{ … }`, so the walker sees each step's keys as repeats of the
# previous step's and reports phantom duplicates (14 on the provisioning pilot,
# measured 2026-10-01). `yq -P` rewrites flow as block while KEEPING duplicate
# keys (it works on the node tree, not a map), so the unchanged scanner then
# answers the same question it answers for block files. Reported line numbers
# refer to that normalised form, and the message says so.
failed=0
norm="$(mktemp)"
trap 'rm -f "$norm"' EXIT
for f in "${files[@]}"; do
  src="$f" where=""
  if is_flow_document "$f"; then
    if ! yq -P '.' "$f" > "$norm" 2> "$norm.err"; then
      echo "::error file=${f}::not parseable as YAML: $(head -c 300 "$norm.err")"
      echo "FAIL ${f}: not parseable as YAML (yq -P): $(head -c 300 "$norm.err")"
      rm -f "$norm.err"
      failed=$((failed + 1))
      continue
    fi
    rm -f "$norm.err"
    src="$norm" where=" of the block-normalised form (yq -P)"
  fi
  out="$(scan_one "$src")" || {
    detail="$(printf '%s' "$out" | awk -F'|' -v w="$where" '{printf "%s\x27%s\x27 (line %s%s)", sep, $1, $2, w; sep=", "}')"
    echo "::error file=${f}::duplicate key(s): ${detail}"
    echo "FAIL ${f}: duplicate key(s): ${detail}"
    failed=$((failed + 1))
  }
done

if [ "$failed" -gt 0 ]; then
  echo
  echo "${failed} of ${#files[@]} workflow file(s) contain duplicate keys."
  echo "GitHub Actions rejects these before any job is created — they fail with"
  echo "no log and no check run. An ordinary YAML parser does NOT catch this;"
  echo "it keeps the last duplicate and reports success."
  exit 1
fi
echo "duplicate-key check: ${#files[@]} workflow file(s) clean"
