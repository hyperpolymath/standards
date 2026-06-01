#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# spdx-inject-copyright.sh
#
# Idempotent injector for SPDX-License-Identifier + SPDX-FileCopyrightText
# (Copyright (c) ...) blocks at the top of source / doc files.
#
# Comment style is autodetected from file extension; override with --style.
#
# Behaviour by file:
#   - If the file already contains the configured Copyright line: skip.
#   - If it has the SPDX-License-Identifier line but not the Copyright
#     line: insert the Copyright line on the line immediately after the
#     SPDX line.
#   - Otherwise: prepend a fresh block (2 lines for line-comment styles,
#     4 lines for block-comment HTML/Markdown style).
#
# Modes:
#   --apply   (default) — write changes.
#   --check   — exit non-zero if any file would change; do not write.
#   --dry-run — print what would change; do not write.
#
# Input: one path per line on stdin, OR --files <list-file>, OR positional
# args. Files are processed in the order listed.
#
# Customisation env vars (or flags):
#   SPDX_LICENSE          (default: MPL-2.0) — license identifier.
#   SPDX_COPYRIGHT_HOLDER (required) — copyright holder.
#                         Example: "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>"
#
# Exit codes:
#   0 — success.  In --check mode: no file would change.
#   1 — In --check or --dry-run mode: at least one file would change.
#       In --apply mode: a hard error occurred (missing file, write failure).
#   2 — invocation / environment error (missing required env var, bad flag).
#
# Stats line (always emitted on stderr at end):
#   STATS: processed=N already_has=N added_copyright=N inserted_block=N \
#          missing=N skipped_unknown_style=N
#
# Wired into governance-reusable.yml as the optional `spdx-inject-check`
# job (calls this script with --check and a file list from caller).
#
# Portability: avoids `sed -i` (BSD vs GNU divergence); uses tempfile + mv.
#   Avoids `grep -qF "$pat"` and `printf "$str"` when $pat / $str can
#   begin with `--` (the `-F` / `printf` option parser would mis-classify
#   it as a flag terminator); always uses `grep -F -e "$pat"` and
#   `printf '%s\n' "$str"` form.

set -u

# --- defaults -----------------------------------------------------------------

LICENSE="${SPDX_LICENSE:-MPL-2.0}"
HOLDER="${SPDX_COPYRIGHT_HOLDER:-}"
MODE="apply"
STYLE=""
FILES_LIST=""
declare -a ARG_FILES=()

# --- argument parsing ---------------------------------------------------------

usage() {
  cat >&2 <<'EOF'
Usage: spdx-inject-copyright.sh [OPTIONS] [FILE...]

  --apply              (default) write changes
  --check              exit 1 if any file would change; do not write
  --dry-run            print what would change; do not write
  --style STYLE        force comment style (html|slash|dash|hash|semi)
                       overrides extension autodetection
  --license ID         license identifier (default: MPL-2.0)
                       can also use SPDX_LICENSE env var
  --holder NAME        copyright holder (required if SPDX_COPYRIGHT_HOLDER unset)
                       can also use SPDX_COPYRIGHT_HOLDER env var
  --files LIST_FILE    read paths from LIST_FILE (one per line)
                       reads stdin if LIST_FILE is "-"
  --help               this message

Comment style autodetection:
  html      .md .html .htm .xml .svg .vue .astro
  slash     .rs .c .h .cpp .hpp .cc .hh .zig .js .ts .tsx .jsx .go
            .swift .kt .scala .java .cs .dart .v .sv .adoc .asciidoc
            .css .scss .sass .less
  dash      .idr .hs .lhs .sql .lean .elm .purs .ada .vhdl
  hash      .ex .exs .py .rb .pl .pm .sh .bash .zsh .fish .yml .yaml
            .toml .r .tcl .jl .nix .dockerfile .makefile .mk
            (also: filenames Makefile, Dockerfile, .gitignore, .envrc)
  semi      .lisp .scm .ss .el .clj .cljs .cljc .rkt
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --apply)   MODE="apply"; shift ;;
    --check)   MODE="check"; shift ;;
    --dry-run) MODE="dry-run"; shift ;;
    --style)   STYLE="$2"; shift 2 ;;
    --license) LICENSE="$2"; shift 2 ;;
    --holder)  HOLDER="$2"; shift 2 ;;
    --files)   FILES_LIST="$2"; shift 2 ;;
    --help|-h) usage; exit 0 ;;
    --) shift; while [[ $# -gt 0 ]]; do ARG_FILES+=("$1"); shift; done ;;
    -*) echo "ERROR: unknown flag: $1" >&2; usage; exit 2 ;;
    *)  ARG_FILES+=("$1"); shift ;;
  esac
done

if [[ -z "$HOLDER" ]]; then
  echo "ERROR: no copyright holder set. Use --holder NAME or set SPDX_COPYRIGHT_HOLDER." >&2
  exit 2
fi

SPDX_LINE_TEXT="SPDX-License-Identifier: $LICENSE"
COPY_LINE_TEXT="Copyright (c) $HOLDER"

# --- comment style helpers ----------------------------------------------------

# Return the comment style for a file based on its extension.
# Sets the global STYLE_FOR_FILE on success; returns 0 on success, 1 on unknown.
detect_style() {
  local file="$1"
  local base ext lower
  base="$(basename -- "$file")"
  lower="$(printf '%s' "$base" | tr '[:upper:]' '[:lower:]')"

  # Filename-based first (no extension required).
  case "$lower" in
    makefile|dockerfile|.gitignore|.envrc|.editorconfig)
      STYLE_FOR_FILE="hash"; return 0 ;;
  esac

  ext="${lower##*.}"
  case "$ext" in
    md|html|htm|xml|svg|vue|astro)
      STYLE_FOR_FILE="html"; return 0 ;;
    rs|c|h|cpp|hpp|cc|hh|zig|js|ts|tsx|jsx|go|swift|kt|scala|java|cs|dart|v|sv|adoc|asciidoc|css|scss|sass|less)
      STYLE_FOR_FILE="slash"; return 0 ;;
    idr|hs|lhs|sql|lean|elm|purs|ada|vhdl)
      STYLE_FOR_FILE="dash"; return 0 ;;
    ex|exs|py|rb|pl|pm|sh|bash|zsh|fish|yml|yaml|toml|r|tcl|jl|nix)
      STYLE_FOR_FILE="hash"; return 0 ;;
    lisp|scm|ss|el|clj|cljs|cljc|rkt)
      STYLE_FOR_FILE="semi"; return 0 ;;
  esac
  STYLE_FOR_FILE=""
  return 1
}

# Compute the SPDX line and Copyright line as they would appear in the file
# (prefixed by the comment marker for line-comment styles, raw for html
# block style which has its own surrounding markers).
emit_style_lines() {
  local s="$1"
  case "$s" in
    html)
      BLOCK_OPEN="<!--"
      BLOCK_CLOSE="-->"
      SPDX_FORMATTED="$SPDX_LINE_TEXT"
      COPY_FORMATTED="$COPY_LINE_TEXT"
      ;;
    slash)
      BLOCK_OPEN=""
      BLOCK_CLOSE=""
      SPDX_FORMATTED="// $SPDX_LINE_TEXT"
      COPY_FORMATTED="// $COPY_LINE_TEXT"
      ;;
    dash)
      BLOCK_OPEN=""
      BLOCK_CLOSE=""
      SPDX_FORMATTED="-- $SPDX_LINE_TEXT"
      COPY_FORMATTED="-- $COPY_LINE_TEXT"
      ;;
    hash)
      BLOCK_OPEN=""
      BLOCK_CLOSE=""
      SPDX_FORMATTED="# $SPDX_LINE_TEXT"
      COPY_FORMATTED="# $COPY_LINE_TEXT"
      ;;
    semi)
      BLOCK_OPEN=""
      BLOCK_CLOSE=""
      SPDX_FORMATTED=";; $SPDX_LINE_TEXT"
      COPY_FORMATTED=";; $COPY_LINE_TEXT"
      ;;
    *) return 1 ;;
  esac
  return 0
}

# Insert COPY_FORMATTED on the line after the first occurrence of SPDX_LINE_TEXT.
# Portable: writes to a tempfile, then mv over the original.
insert_copyright_after_spdx() {
  local file="$1"
  local tmp
  tmp="$(mktemp)" || return 1
  local line_num
  line_num="$(grep -nF -e "$SPDX_LINE_TEXT" "$file" | head -1 | cut -d: -f1)"
  if [[ -z "$line_num" ]]; then
    rm -f "$tmp"
    return 1
  fi
  awk -v ln="$line_num" -v new="$COPY_FORMATTED" \
      'NR==ln { print; print new; next } { print }' \
      "$file" > "$tmp" && mv "$tmp" "$file"
}

# Prepend a fresh block at the top of the file.
prepend_block() {
  local file="$1"
  local tmp
  tmp="$(mktemp)" || return 1
  {
    if [[ -n "$BLOCK_OPEN" ]]; then
      printf '%s\n' "$BLOCK_OPEN"
      printf '%s\n' "$SPDX_FORMATTED"
      printf '%s\n' "$COPY_FORMATTED"
      printf '%s\n' "$BLOCK_CLOSE"
    else
      printf '%s\n' "$SPDX_FORMATTED"
      printf '%s\n' "$COPY_FORMATTED"
    fi
    cat -- "$file"
  } > "$tmp" && mv "$tmp" "$file"
}

# --- counters ----------------------------------------------------------------

processed=0
already_has=0
added_copyright=0
inserted_block=0
missing=0
skipped_unknown_style=0
would_change=0

# --- file iteration ----------------------------------------------------------

process_file() {
  local file="$1"
  if [[ ! -f "$file" ]]; then
    missing=$((missing + 1))
    return 0
  fi
  processed=$((processed + 1))

  # Determine style.
  local s
  if [[ -n "$STYLE" ]]; then
    s="$STYLE"
  else
    if detect_style "$file"; then
      s="$STYLE_FOR_FILE"
    else
      skipped_unknown_style=$((skipped_unknown_style + 1))
      [[ "$MODE" == "dry-run" ]] && echo "SKIP (unknown style): $file"
      return 0
    fi
  fi
  emit_style_lines "$s" || return 1

  if grep -qF -e "$COPY_LINE_TEXT" -- "$file" 2>/dev/null; then
    already_has=$((already_has + 1))
    [[ "$MODE" == "dry-run" ]] && echo "SKIP (already has copyright): $file"
    return 0
  fi

  if grep -qF -e "$SPDX_LINE_TEXT" -- "$file" 2>/dev/null; then
    would_change=$((would_change + 1))
    case "$MODE" in
      check)
        echo "WOULD-INSERT-COPYRIGHT: $file"
        ;;
      dry-run)
        echo "WOULD-INSERT-COPYRIGHT: $file"
        ;;
      apply)
        if ! insert_copyright_after_spdx "$file"; then
          echo "ERROR: insertion failed for $file" >&2
          return 1
        fi
        added_copyright=$((added_copyright + 1))
        ;;
    esac
  else
    would_change=$((would_change + 1))
    case "$MODE" in
      check)
        echo "WOULD-PREPEND-BLOCK: $file"
        ;;
      dry-run)
        echo "WOULD-PREPEND-BLOCK: $file"
        ;;
      apply)
        if ! prepend_block "$file"; then
          echo "ERROR: prepend failed for $file" >&2
          return 1
        fi
        inserted_block=$((inserted_block + 1))
        ;;
    esac
  fi
}

# Read files from sources, in priority order: positional > --files > stdin.
if [[ ${#ARG_FILES[@]} -gt 0 ]]; then
  for f in "${ARG_FILES[@]}"; do
    process_file "$f" || exit 1
  done
elif [[ -n "$FILES_LIST" ]]; then
  if [[ "$FILES_LIST" == "-" ]]; then
    while IFS= read -r f; do
      [[ -z "$f" ]] && continue
      process_file "$f" || exit 1
    done
  else
    while IFS= read -r f; do
      [[ -z "$f" ]] && continue
      process_file "$f" || exit 1
    done < "$FILES_LIST"
  fi
else
  while IFS= read -r f; do
    [[ -z "$f" ]] && continue
    process_file "$f" || exit 1
  done
fi

# --- stats ------------------------------------------------------------------

printf 'STATS: processed=%d already_has=%d added_copyright=%d inserted_block=%d missing=%d skipped_unknown_style=%d\n' \
  "$processed" "$already_has" "$added_copyright" "$inserted_block" "$missing" "$skipped_unknown_style" >&2

# --- exit code --------------------------------------------------------------

case "$MODE" in
  check|dry-run)
    if [[ $would_change -gt 0 ]]; then
      exit 1
    fi
    exit 0
    ;;
  apply)
    exit 0
    ;;
esac
