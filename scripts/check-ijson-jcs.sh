#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Report JSON and JSON Lines files that are not I-JSON (RFC 7493) + JCS
# (RFC 8785) canonical, per 3-practice/JSON-POLICY.adoc (owner ruling D306).
#
# Usage: check-ijson-jcs.sh [--enforce] [PATH...]
#
# Default is REPORT-ONLY: it lists every non-canonical or invalid file and a
# summary, then exits 0. --enforce exits 1 when any file is non-canonical or
# invalid; it exists for repositories that have finished converting, and for
# this script's own test. Exit 2 always means the check could not run (missing
# tool, missing path): a gate that cannot look must not report a pass.
#
# .json files go through `ijson-jcs check`. .jsonl files are checked one line
# at a time with `ijson-jcs canon`, because the tool reads whole documents only.
# Tool-demanded JSONC (devcontainer.json, .vscode/*.json) is skipped and counted:
# canonicalising would delete its comments, including SPDX headers.
set -euo pipefail

IJSON_JCS="${IJSON_JCS:-ijson-jcs}"
enforce=0
if [ "${1:-}" = "--enforce" ]; then
  enforce=1
  shift
fi
if [ "$#" -eq 0 ]; then
  set -- .
fi
if ! command -v "$IJSON_JCS" >/dev/null 2>&1; then
  printf '%s: not found; install hyperpolymath/ijson-jcs or set IJSON_JCS\n' "$IJSON_JCS" >&2
  exit 2
fi
# A mistyped path must not turn into an empty, passing scan.
for path in "$@"; do
  if [ ! -e "$path" ]; then
    printf '%s: scan path does not exist\n' "$path" >&2
    exit 2
  fi
done

# List the JSON and JSON Lines files under the given paths, NUL-separated,
# skipping VCS metadata and regenerable build trees.
list_files() {
  find "$@" \( -name .git -o -name node_modules -o -name target -o -name _build \) -prune \
    -o -type f \( -name '*.json' -o -name '*.jsonl' \) -print0
}

# Succeed when a file is tool-demanded JSONC that the policy carves out.
is_jsonc_carve_out() {
  case "$1" in
    */devcontainer.json | devcontainer.json | */.vscode/*.json | .vscode/*.json) return 0 ;;
  esac
  return 1
}

# Classify one .json file; print CANONICAL, NOT_CANONICAL or INVALID.
classify_json() {
  local rc=0
  "$IJSON_JCS" check "$1" >/dev/null 2>&1 || rc=$?
  case "$rc" in
    0) echo CANONICAL ;;
    1) echo NOT_CANONICAL ;;
    *) echo INVALID ;;
  esac
}

# Classify one .jsonl file line by line; print CANONICAL, NOT_CANONICAL or
# INVALID. A blank line or an unparseable line is INVALID; a line whose
# canonical form differs, or a missing final newline, is NOT_CANONICAL.
classify_jsonl() {
  local file="$1" line canon verdict=CANONICAL
  if [ -s "$file" ] && [ "$(tail -c 1 "$file" | od -An -c | tr -d ' ')" != '\n' ]; then
    verdict=NOT_CANONICAL
  fi
  while IFS= read -r line || [ -n "$line" ]; do
    if [ -z "$line" ]; then
      echo INVALID
      return
    fi
    if ! canon="$(printf '%s' "$line" | "$IJSON_JCS" canon 2>/dev/null)"; then
      echo INVALID
      return
    fi
    if [ "$canon" != "$line" ]; then
      verdict=NOT_CANONICAL
    fi
  done <"$file"
  echo "$verdict"
}

total=0 canonical=0 noncanonical=0 invalid=0 skipped=0
while IFS= read -r -d '' file; do
  file="${file#./}"
  if is_jsonc_carve_out "$file"; then
    skipped=$((skipped + 1))
    continue
  fi
  total=$((total + 1))
  case "$file" in
    *.jsonl) verdict="$(classify_jsonl "$file")" ;;
    *) verdict="$(classify_json "$file")" ;;
  esac
  case "$verdict" in
    CANONICAL) canonical=$((canonical + 1)) ;;
    NOT_CANONICAL) noncanonical=$((noncanonical + 1)); printf 'NOT CANONICAL %s\n' "$file" ;;
    *) invalid=$((invalid + 1)); printf 'INVALID %s\n' "$file" ;;
  esac
done < <(list_files "$@" | sort -z)

summary="ijson-jcs: ${total} file(s) checked, ${canonical} canonical, ${noncanonical} not canonical, ${invalid} invalid; ${skipped} JSONC carve-out(s) skipped"
echo "$summary"
if [ -n "${GITHUB_ACTIONS:-}" ]; then
  echo "::notice title=ijson-jcs (report-only)::${summary}. Fix with: ijson-jcs fix <file>"
fi
if [ "$enforce" -eq 1 ] && [ $((noncanonical + invalid)) -gt 0 ]; then
  exit 1
fi
exit 0
