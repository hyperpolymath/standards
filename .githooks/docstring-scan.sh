#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# docstring-scan.sh — report the functions a change touches, and whether each is documented.
#
# The canonical docstring predicate for the estate. Three consumers call it (or re-implement it
# against the same fixtures): the ~/.claude Stop hook, the .githooks pre-commit validator, and the
# CI backstop. Keep ONE copy of this logic; share the fixture corpus, not the code.
#
# It asks the question CodeRabbit's "Docstring Coverage" pre-merge check asks — "of the functions
# this diff touches, how many carry a docstring?" — and calibrates against a known answer:
# hyperpolymath/standards PR #1034 at 1cc72cdc80c9 ⇒ 2 files, 13 functions, 0 documented, 3 skipped.
#
# Usage:
#   docstring-scan.sh --worktree            uncommitted work vs HEAD (untracked files count as added)
#   docstring-scan.sh --staged              the index vs HEAD (pre-commit)
#   docstring-scan.sh --range BASE..HEAD    a commit range (CI, calibration)
#   add --check to exit 1 when a NEWLY-ADDED function is undocumented.
#
# Output (stdout): one TSV row per touched function, then one SUMMARY line.
#   path<TAB>line<TAB>symbol<TAB>added|modified<TAB>documented|undocumented
#   path<TAB>-<TAB>-<TAB>-<TAB>skipped          (a changed source file in an unsupported language)
#   SUMMARY files=N functions=N documented=N undocumented=N added_undocumented=N skipped=N coverage=P%
# The denominator is always printed: "0 undocumented" out of 0 functions is a vacuous pass and must
# not read the same as a real one.
#
# Exit: 0 report produced (and, with --check, no added undocumented function); 1 --check found an
# added undocumented function; 2 usage or git error.
#
# Tier 1 (this version): shell. Every other source extension reports as SKIPPED — never as
# documented. Documentation files (.adoc/.md/.rst/.txt-less docs) are ignored outright, as
# CodeRabbit ignores them.

set -uo pipefail

MODE=""; RANGE=""; CHECK=0
while [ $# -gt 0 ]; do
  case "$1" in
    --worktree) MODE=worktree ;;
    --staged)   MODE=staged ;;
    --range)    MODE=range; RANGE="${2:-}"; shift ;;
    --check)    CHECK=1 ;;
    -h|--help)  sed -n '2,31p' "$0"; exit 0 ;;
    *) printf 'docstring-scan: unknown argument: %s\n' "$1" >&2; exit 2 ;;
  esac
  shift
done
[ -n "$MODE" ] || { printf 'docstring-scan: one of --worktree, --staged, --range BASE..HEAD is required\n' >&2; exit 2; }
git rev-parse --git-dir >/dev/null 2>&1 || { printf 'docstring-scan: not inside a git repository\n' >&2; exit 2; }

G=(git -c core.quotePath=false)

# Resolve the base revision and the revision (or source) the new content is read from.
if [ "$MODE" = range ]; then
  case "$RANGE" in *..*) ;; *) printf 'docstring-scan: --range needs BASE..HEAD\n' >&2; exit 2 ;; esac
  BASE="${RANGE%%..*}"; HEADREV="${RANGE##*..}"
  "${G[@]}" rev-parse --verify -q "$BASE^{commit}" >/dev/null || { printf 'docstring-scan: bad base %s\n' "$BASE" >&2; exit 2; }
  "${G[@]}" rev-parse --verify -q "$HEADREV^{commit}" >/dev/null || { printf 'docstring-scan: bad head %s\n' "$HEADREV" >&2; exit 2; }
else
  # A repo with no commits yet has no HEAD: everything is added.
  if "${G[@]}" rev-parse --verify -q HEAD >/dev/null; then BASE=HEAD; else BASE=""; fi
fi

# Print the new content of a path for the active mode.
new_content() {
  case "$MODE" in
    worktree) cat -- "$1" ;;
    staged)   "${G[@]}" show ":$1" ;;
    range)    "${G[@]}" show "$HEADREV:$1" ;;
  esac
}

# Print the base content of a path, or nothing when it did not exist at the base.
old_content() {
  [ -n "$BASE" ] || return 0
  "${G[@]}" show "$BASE:$1" 2>/dev/null || true
}

# List changed paths (NUL-separated) with a status letter: A (whole file new) or M (touched).
changed_paths() {
  local diffargs=()
  case "$MODE" in
    worktree) diffargs=("$BASE") ;;
    staged)   diffargs=(--cached "$BASE") ;;
    range)    diffargs=("$BASE" "$HEADREV") ;;
  esac
  if [ -z "$BASE" ]; then
    # No base commit: every tracked or staged file is new.
    "${G[@]}" ls-files -z | while IFS= read -r -d '' p; do printf 'A\t%s\0' "$p"; done
  else
    "${G[@]}" diff --no-renames --name-status -z --diff-filter=AM "${diffargs[@]}" -- \
      | while IFS= read -r -d '' st && IFS= read -r -d '' p; do printf '%s\t%s\0' "$st" "$p"; done
  fi
  if [ "$MODE" = worktree ]; then
    "${G[@]}" ls-files -z --others --exclude-standard | while IFS= read -r -d '' p; do printf 'A\t%s\0' "$p"; done
  fi
}

# Print the changed new-side line ranges of a touched file as "start end" lines.
touched_ranges() {
  local diffargs=()
  case "$MODE" in
    worktree) diffargs=("$BASE") ;;
    staged)   diffargs=(--cached "$BASE") ;;
    range)    diffargs=("$BASE" "$HEADREV") ;;
  esac
  "${G[@]}" diff --no-renames -U0 "${diffargs[@]}" -- "$1" \
    | sed -nE 's/^@@ -[0-9,]+ \+([0-9]+)(,([0-9]+))? @@.*/\1 \3/p' \
    | awk '{ n = ($2 == "") ? 1 : $2; if (n > 0) print $1, $1 + n - 1; else print $1, $1 }'
}

# Classify a path: shell | doc | other (unsupported source, reported as skipped) | ignore.
classify() {
  local p="$1" base="${1##*/}"
  case "$base" in
    *.sh|*.bash|*.bats) echo shell; return ;;
    *.adoc|*.md|*.rst|*.org|*.txt|*.json|*.toml|*.yml|*.yaml|*.scm|*.lock|*.a2ml|*.csv|*.tsv|*.svg|*.png|*.jpg|*.gif|*.ico)
      # CodeRabbit reports data files as "unsupported" (skipped) and prose as nothing at all.
      case "$base" in *.adoc|*.md|*.rst|*.org) echo doc ;; *) echo other ;; esac; return ;;
  esac
  # An extensionless file is shell when its shebang says so.
  case "$base" in
    *.*) echo other ;;
    *) if [ "$MODE" = worktree ] && [ -f "$p" ]; then
         head -c 64 -- "$p" 2>/dev/null | head -1 | grep -qE '^#!.*\b(ba|z|k|da)?sh\b' && { echo shell; return; }
       fi
       echo other ;;
  esac
}

# Emit "line<TAB>end<TAB>name<TAB>documented|undocumented" for every shell function in stdin.
# Heredoc bodies are skipped; a same-line trailing comment is NOT a docstring (PR #1034 proves it);
# shebang, shellcheck directives, SPDX headers and bare "#" lines do not count as documentation.
shell_functions() {
  awk '
    function flush_doc() { doc = 0; seen = 0 }
    BEGIN { hd = ""; doc = 0; seen = 0; open_n = 0 }
    {
      line = $0
      if (hd != "") {
        chk = line; if (hd_strip) sub(/^\t+/, "", chk)
        if (chk == hd) hd = ""
        flush_doc(); next
      }
      # Close an open multi-line function at a line that is exactly its indent plus "}".
      if (open_n > 0 && line ~ ("^" open_indent "}")) {
        print open_start "\t" NR "\t" open_name "\t" open_doc; open_n = 0
      }
      if (line ~ /^[ \t]*#/) {
        c = line; sub(/^[ \t]*#+[ \t]*/, "", c)
        if (line ~ /^#!/ || c ~ /^shellcheck[ \t]/ || c ~ /^SPDX-/ || c == "") { seen = 1 }
        else { doc = 1; seen = 1 }
        next
      }
      name = ""
      if (match(line, /^[ \t]*function[ \t]+[A-Za-z_][A-Za-z0-9_:.-]*/)) {
        name = substr(line, RSTART, RLENGTH); sub(/^[ \t]*function[ \t]+/, "", name)
      } else if (match(line, /^[ \t]*[A-Za-z_][A-Za-z0-9_:.-]*[ \t]*\(\)/)) {
        name = substr(line, RSTART, RLENGTH); sub(/[ \t]*\(\)$/, "", name); sub(/^[ \t]*/, "", name)
      }
      if (name != "") {
        status = doc ? "documented" : "undocumented"
        ind = line; sub(/[^ \t].*$/, "", ind)
        rest = line; o = gsub(/\{/, "{", rest); cl = gsub(/\}/, "}", rest)
        if (o > 0 && o == cl) { print NR "\t" NR "\t" name "\t" status }
        else {
          if (open_n > 0) print open_start "\t" (NR - 1) "\t" open_name "\t" open_doc
          open_n = 1; open_start = NR; open_name = name; open_doc = status; open_indent = ind
        }
      }
      # Enter a heredoc (not a <<< herestring) after the function check, so "f() { cat <<EOF" works.
      if (match(line, /<<-?[ \t]*["\047]?[A-Za-z_][A-Za-z0-9_]*["\047]?/) && line !~ /<<</) {
        tok = substr(line, RSTART, RLENGTH); hd_strip = (tok ~ /^<<-/)
        sub(/^<<-?[ \t]*/, "", tok); gsub(/["\047]/, "", tok); hd = tok
      }
      flush_doc()
    }
    END { if (open_n > 0) print open_start "\t" NR "\t" open_name "\t" open_doc }
  '
}

TMP="$(mktemp -d -t docscan.XXXXXX)" || exit 2
trap 'rm -rf "$TMP"' EXIT

files=0; functions=0; documented=0; undocumented=0; added_undoc=0; skipped=0

while IFS= read -r -d '' rec; do
  st="${rec%%$'\t'*}"; p="${rec#*$'\t'}"
  kind="$(classify "$p")"
  case "$kind" in
    doc|ignore) continue ;;
    other) printf '%s\t-\t-\t-\tskipped\n' "$p"; skipped=$((skipped + 1)); continue ;;
  esac
  new_content "$p" > "$TMP/new" 2>/dev/null || continue
  old_content "$p" | shell_functions | cut -f3 | sort -u > "$TMP/oldnames"
  shell_functions < "$TMP/new" > "$TMP/fns"
  if [ "$st" = A ]; then : > "$TMP/ranges"; else touched_ranges "$p" > "$TMP/ranges"; fi
  hit=0
  while IFS=$'\t' read -r start end name status; do
    if [ "$st" != A ]; then
      awk -v s="$start" -v e="$end" '$1 <= e && $2 >= s { f = 1 } END { exit !f }' "$TMP/ranges" || continue
    fi
    if [ "$st" = A ] || ! grep -qxF -- "$name" "$TMP/oldnames"; then origin=added; else origin=modified; fi
    printf '%s\t%s\t%s\t%s\t%s\n' "$p" "$start" "$name" "$origin" "$status"
    hit=1; functions=$((functions + 1))
    if [ "$status" = documented ]; then documented=$((documented + 1)); else
      undocumented=$((undocumented + 1)); [ "$origin" = added ] && added_undoc=$((added_undoc + 1))
    fi
  done < "$TMP/fns"
  [ "$hit" = 1 ] && files=$((files + 1))
done < <(changed_paths)

if [ "$functions" -gt 0 ]; then
  cov="$(awk -v d="$documented" -v n="$functions" 'BEGIN { printf "%.2f", 100 * d / n }')"
else
  cov="n/a"
fi
printf 'SUMMARY files=%d functions=%d documented=%d undocumented=%d added_undocumented=%d skipped=%d coverage=%s%%\n' \
  "$files" "$functions" "$documented" "$undocumented" "$added_undoc" "$skipped" "$cov"

[ "$CHECK" = 1 ] && [ "$added_undoc" -gt 0 ] && exit 1
exit 0
