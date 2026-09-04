#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan Jewell <j.d.a.jewell@open.ac.uk>
#
# check-ts-allowlist.sh — fail when hand-authored TypeScript appears outside
# the allowlist. Runs against the current working directory.
#
# WHY SHELL. This replaces check-ts-allowlist.deno.js, which put a Deno install
# step on a REQUIRED context on every estate repo. The obvious replacement was
# AffineScript compiled to Bun, but scripts/check-ts-allowlist.affine is
# TypeScript wearing an .affine extension: it has never compiled, so the .js
# beside it was never generated from it, and stdlib/Bun.affine declares no
# filesystem capability to port onto. Shell needs no runtime beyond the tools
# every runner already has — the same reasoning recorded in the header of
# scripts/check-workflow-duplicate-keys.sh.
#
# Behaviour is pinned by scripts/tests/check-ts-allowlist-test.sh, which
# asserts identical verdicts to the Deno implementation on every case.

set -uo pipefail

DIR_NAMES_ALLOWED=(bindings tests test scripts mcp-adapter cli vendor examples ffi node_modules benchmarks)

# Strip leading "." and "/" characters, as the Deno implementation does, so
# "./src/a.ts", "/src/a.ts" and "src/a.ts" are one path.
normalize_repo_path() {
  local out="$1"
  out="${out#"${out%%[![:space:]]*}"}"   # ltrim
  out="${out%"${out##*[![:space:]]}"}"   # rtrim
  while [ -n "$out" ]; do
    case "$out" in
      .*|/*) out="${out#?}" ;;
      *)     break ;;
    esac
  done
  printf '%s' "$out"
}

# Glob -> anchored ERE. '*' is any run, '?' is one character, everything else
# regex-significant is escaped.
glob_to_regex() {
  local g out="" i c
  g="$(normalize_repo_path "$1")"
  for (( i = 0; i < ${#g}; i++ )); do
    c="${g:i:1}"
    case "$c" in
      '*') out+='.*' ;;
      '?') out+='.'  ;;
      '.'|'+'|'('|')'|'{'|'}'|'['|']'|'^'|'$'|'|') out+="\\$c" ;;
        $'\\') out+=$'\\\\' ;;
      *)   out+="$c" ;;
    esac
  done
  printf '^%s$' "$out"
}

# --- exemption sources -------------------------------------------------------
# Layer 2:   a "TypeScript Exemptions" table in .claude/CLAUDE.md
# Layer 2.5: one path per line in .governance-allowlist
EX_RAW=()

load_exemptions_from_claude_md() {
  [ -f .claude/CLAUDE.md ] || return 0
  local ts_heading='^#{1,4}[[:space:]]+.*(TypeScript|JavaScript|TS|JS|\.tsx?)\b[^#]*[Ee]xemption'
  local any_heading='^#{1,4}[[:space:]]'
  local in_table=0 line rest raw
  while IFS= read -r line || [ -n "$line" ]; do
    if [[ $line =~ $ts_heading ]]; then in_table=1; continue; fi
    if [ "$in_table" -eq 1 ] && [[ $line =~ $any_heading ]]; then in_table=0; continue; fi
    [ "$in_table" -eq 1 ] || continue
    [ -n "$line" ] || continue
    # a row whose first cell is a backticked path
    [[ $line =~ ^[[:space:]]*\|[[:space:]]*\`[^\`]+\` ]] || continue
    rest="${line#*\`}"          # drop up to the first backtick
    raw="${rest%%\`*}"          # take up to the next one
    [ -n "$raw" ] && EX_RAW+=("$raw")
  done < .claude/CLAUDE.md
}

load_exemptions_from_allowlist_file() {
  [ -f .governance-allowlist ] || return 0
  local line raw
  while IFS= read -r line || [ -n "$line" ]; do
    raw="$(normalize_repo_path "$line")"
    [ -n "$raw" ] || continue
    case "$raw" in '#'*) continue ;; esac
    EX_RAW+=("$raw")
  done < .governance-allowlist
}

is_exempt() {
  local target rx bare e
  target="$(normalize_repo_path "$1")"
  for e in ${EX_RAW+"${EX_RAW[@]}"}; do
    rx="$(glob_to_regex "$e")"
    [[ $target =~ $rx ]] && return 0
    bare="$(normalize_repo_path "$e")"
    [ "$target" = "$bare" ] && return 0
    case "$bare" in */) case "$target" in "$bare"*) return 0 ;; esac ;; esac
  done
  return 1
}

# --- builtin allowlist -------------------------------------------------------
builtin_allowed() {
  local p="$1" base seg d
  base="${p##*/}"
  case "$p"    in *.d.ts) return 0 ;; esac
  case "$base" in
    mod.ts|lsp-server.ts|lsp_server.ts|lsp.ts) return 0 ;;
    *-lsp.ts|*.bench.ts|*_bench.ts)            return 0 ;;
  esac
  # any DIRECTORY segment (every segment but the last)
  local dirpart="${p%/*}"
  [ "$dirpart" = "$p" ] && return 1
  local IFS='/'
  for seg in $dirpart; do
    [ -n "$seg" ] || continue
    for d in "${DIR_NAMES_ALLOWED[@]}"; do
      [ "$seg" = "$d" ] && return 0
    done
    case "$seg" in *vscode*) return 0 ;; deno-*) return 0 ;; esac
  done
  return 1
}

# A path is skipped entirely when any segment is a dotfile/dotdir (but "." and
# ".." are not dotfiles).
has_hidden_segment() {
  local p="$1" seg
  local IFS='/'
  for seg in $p; do
    [ -n "$seg" ] || continue
    [ "$seg" = "." ] && continue
    [ "$seg" = ".." ] && continue
    case "$seg" in .*) return 0 ;; esac
  done
  return 1
}

# --- main --------------------------------------------------------------------
load_exemptions_from_claude_md
load_exemptions_from_allowlist_file

bad=()
while IFS= read -r f; do
  [ -n "$f" ] || continue
  has_hidden_segment "$f" && continue
  n="$(normalize_repo_path "$f")"
  builtin_allowed "$n" && continue
  is_exempt "$n" && continue
  bad+=("$n")
done < <(find . -type f \( -name '*.ts' -o -name '*.tsx' -o -name '*.ts.bak' -o -name '*.tsx.bak' \) 2>/dev/null | LC_ALL=C sort)

if [ "${#bad[@]}" -gt 0 ]; then
  printf '%s\n' "❌ TypeScript files detected outside the allowlist." >&2
  printf '\n' >&2
  for f in "${bad[@]}"; do printf '  %s\n' "$f" >&2; done
  printf '\n' >&2
  printf '%s\n' "To resolve, choose one:" >&2
  printf '%s\n' "  (a) migrate the file to AffineScript" >&2
  printf '%s\n' "  (b) move to an allowlisted bridge path" >&2
  printf '%s\n' "  (c) add an entry to a 'TypeScript Exemptions' table in .claude/CLAUDE.md (Layer 2)" >&2
  printf '%s\n' "  (d) add a line to .governance-allowlist at the repo root (Layer 2.5 — typed infrastructure file)" >&2
  printf '\n' >&2
  printf '%s\n' "See docs/EXEMPTION-MECHANISMS.adoc for the full mechanism reference." >&2
  if [ "${#EX_RAW[@]}" -gt 0 ]; then
    printf '\n(Currently %d exemption(s) parsed across both layers.)\n' "${#EX_RAW[@]}" >&2
  fi
  exit 1
fi

printf '✅ No TypeScript files outside allowlist (%d per-repo exemption(s) parsed across CLAUDE.md + .governance-allowlist).\n' "${#EX_RAW[@]}"
