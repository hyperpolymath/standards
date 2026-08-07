#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# fill-placeholders.sh — fill template placeholders without destroying the
# machinery that fills them.
#
# WHY THIS EXISTS. A previous estate-wide sweep filled `{{TOKEN}}` placeholders
# by plain text replacement across every file. It produced 141 review findings
# across 90 repositories, and one of them is severe:
#
#     sed "s/{{PROJECT_NAME}}/$name/g"   became   sed "s/Conative Gating/$name/g"
#     sed -e "s/{{DATE}}/$DATE/g"        became   sed -e "s/2026-08-05/$DATE/g"
#
# Those are the scripts whose JOB is to perform template substitution. Filling
# the left-hand side of their own `sed` expressions means template application
# silently stops working — and stays invisible until someone mints a new
# repository from the template and gets a half-substituted tree.
#
# THE GENERAL RULE, which plain replacement cannot express: a placeholder is
# sometimes a VALUE TO FILL and sometimes the SUBJECT BEING DISCUSSED. Only the
# first should be substituted. Three contexts make it the subject:
#
#   1. THE LEFT-HAND SIDE OF A SUBSTITUTION — `s/{{X}}/.../`, `s|{{X}}|...|`.
#      The token is a search pattern, not a value.
#   2. FILES WHOSE TOPIC IS THE TOKEN — REQUIRES_INITIALISATION.md lists the
#      tokens still to fill; QUICKSTART docs say "Replace {{DEPS}} with actuals".
#      Filling those produced "Replace laminar, laminar, {{DEPS}} with actuals".
#   3. ANOTHER TOOL'S OWN DELIMITERS — `just` uses `{{ARGS}}` natively in recipe
#      bodies. It is not a template placeholder and never was; substituting or
#      reporting it produces a permanently "uninitialised" repository.
#
# ⚠ THE EXCLUSIONS ARE THE POINT. A version of this that fills everything is
# what already ran, and it is why 289 pull requests had to be closed. If you
# extend this script, extend the exclusions with it and add a test.
#
# ⚠ SHELL, NOT PYTHON OR A JS RUNTIME. Estate policy bans Python outright, and
# the JS runtime has already moved once (Deno to Bun). The tool that mints every
# repository must not be invalidated by the next runtime change.
#
# Usage: fill-placeholders.sh ROOT --map MAP.json [--apply]
# Exit 1 if any value was refused as unsafe for an identifier/URL slot.
set -uo pipefail

ROOT=""; MAP=""; APPLY=0
while [ $# -gt 0 ]; do
  case "$1" in
    --map)   MAP="${2:?--map needs a file}"; shift 2 ;;
    --apply) APPLY=1; shift ;;
    -h|--help) sed -n '2,42p' "$0"; exit 0 ;;
    *)       ROOT="$1"; shift ;;
  esac
done
[ -n "$ROOT" ] || { echo "usage: $0 ROOT --map MAP.json [--apply]" >&2; exit 2; }
[ -n "$MAP" ]  || { echo "usage: $0 ROOT --map MAP.json [--apply]" >&2; exit 2; }
[ -d "$ROOT" ] || { echo "no such directory: $ROOT" >&2; exit 2; }
[ -f "$MAP" ]  || { echo "no such map file: $MAP" >&2; exit 2; }

# jq, not an interpreter — the map is JSON and jq is already an estate CI
# dependency. Tab-separated so a value may contain spaces.
MAPTSV="$(mktemp)"; trap 'rm -f "$MAPTSV"' EXIT
jq -r 'to_entries[] | "\(.key)\t\(.value)"' "$MAP" > "$MAPTSV" || {
  echo "map is not a JSON object of {TOKEN: value}: $MAP" >&2; exit 2; }

# Files whose SUBJECT is the placeholder set, and template sources whose tokens
# must survive. Matched on the path relative to ROOT.
skip_reason() {
  case "$1" in
    REQUIRES_INITIALISATION.md|REQUIRES_INITIALISATION.adoc|*/REQUIRES_INITIALISATION.md|*/REQUIRES_INITIALISATION.adoc)
      echo "file's subject IS the placeholder set"; return ;;
  esac
  local base="${1##*/}" lower
  lower="$(printf '%s' "$base" | tr '[:upper:]' '[:lower:]')"
  case "$lower" in
    quickstart*.md|quickstart*.adoc|placeholder.md|placeholders.md|placeholder.adoc|placeholders.adoc|template*.md|template*.adoc)
      echo "file's subject IS the placeholder set"; return ;;
  esac
  local lpath
  lpath="$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')"
  case "$lpath" in
    *.template|template/*|templates/*|*/template/*|*/templates/*)
      echo "template source — its tokens must survive"; return ;;
  esac
}

CHANGED=0; SKIPPED=0; PROTECTED=0; REFUSALS="$(mktemp)"
trap 'rm -f "$MAPTSV" "$REFUSALS"' EXIT
CHANGED_LIST="$(mktemp)"; SKIPPED_LIST="$(mktemp)"
trap 'rm -f "$MAPTSV" "$REFUSALS" "$CHANGED_LIST" "$SKIPPED_LIST"' EXIT

while IFS= read -r -d '' f; do
  rel="${f#"$ROOT"/}"
  why="$(skip_reason "$rel")"
  if [ -n "$why" ]; then
    SKIPPED=$((SKIPPED + 1)); printf '%s\t%s\n' "$rel" "$why" >> "$SKIPPED_LIST"; continue
  fi
  grep -q '{{' "$f" 2>/dev/null || continue

  out="$(awk -v mapfile="$MAPTSV" -v refusals="$REFUSALS" '
    function slug_safe(v) { return v ~ /^[A-Za-z0-9][A-Za-z0-9._-]*$/ }

    # A value can be correct AND wrong depending on the slot it lands in. These
    # patterns mark lines where the text must be a machine-safe identifier or
    # URL component — a Guix channel/package name, a BibTeX cite key, a URL
    # path, a container tag. Substituting a human display name here produced,
    # verbatim: (name (quote BoJ Server Mk2), a URL with spaces in it, and
    # @software{BoJ Server Mk2_2026 — all syntactically invalid.
    function slug_slot(l,   lo) {
      lo = tolower(l)
      if (lo ~ /\(name[ \t]+['"'"'"]/)            return 1
      if (lo ~ /@[a-z]+\{/)                        return 1
      if (lo ~ /https?:\/\//)                      return 1
      if (lo ~ /-t[ \t]+[^ \t]*$/)                 return 1
      if (lo ~ /^[ \t]*(name|package|slug|id)[ \t]*[:=]/) return 1
      return 0
    }

    # `s<delim>{{TOKEN}}<delim>` — the token is a SEARCH PATTERN here, not a
    # value. Detected positionally rather than with a backreference: an
    # occurrence starting at p is a substitution LHS exactly when the character
    # before it is a delimiter, the one before that is `s`, and the character
    # immediately after the token is the SAME delimiter.
    function is_lhs(l, p, len,   d, before) {
      if (p < 3) return 0
      d = substr(l, p - 1, 1)
      if (index("/|#,!@", d) == 0) return 0
      before = substr(l, p - 2, 1)
      if (before != "s") return 0
      return (substr(l, p + len, 1) == d)
    }

    BEGIN {
      FS = "\t"
      n = 0
      while ((getline line < mapfile) > 0) {
        i = index(line, "\t")
        if (i == 0) continue
        t = substr(line, 1, i - 1); v = substr(line, i + 1)
        # `just` uses these natively in recipe bodies; not template tokens.
        if (t == "ARGS" || t == "invocation_directory" || t == "justfile" ||
            t == "os" || t == "arch") continue
        n++; tok[n] = t; val[n] = v
        if (t == "PROJECT_SLUG" || t == "REPO_SLUG") if (slug_safe(v)) slug = v
      }
      close(mapfile)
    }

    {
      l = $0
      ss = slug_slot(l)
      for (k = 1; k <= n; k++) {
        needle = "{{" tok[k] "}}"
        nlen = length(needle)
        if (index(l, needle) == 0) continue

        use = val[k]
        if (ss && !slug_safe(val[k])) {
          if (slug != "") {
            use = slug
          } else {
            # Emitting invalid syntax silently is worse than leaving the token
            # visible: an unfilled {{PROJECT_NAME}} is obviously unfinished,
            # whereas a name with spaces in a URL looks plausible and fails
            # later, somewhere else.
            ctx = l; sub(/^[ \t]+/, "", ctx); sub(/[ \t]+$/, "", ctx)
            print tok[k] "\t" val[k] "\t" substr(ctx, 1, 90) >> refusals
            continue
          }
        }

        outl = ""; rest = l
        base = 0
        while ((p = index(rest, needle)) > 0) {
          abs = base + p
          outl = outl substr(rest, 1, p - 1)
          outl = outl (is_lhs(l, abs, nlen) ? needle : use)
          rest = substr(rest, p + nlen)
          base = abs + nlen - 1
        }
        l = outl rest
      }
      print l
    }
  ' "$f")"

  # awk drops a trailing newline distinction; restore the original ending.
  if [ -s "$f" ] && [ "$(tail -c1 "$f" | wc -l)" -gt 0 ]; then
    out="$out
"
  fi

  if [ "$out" != "$(cat "$f")" ] || { [ -n "$out" ] && ! cmp -s <(printf '%s' "$out") "$f"; }; then
    if ! cmp -s <(printf '%s' "$out") "$f"; then
      CHANGED=$((CHANGED + 1)); printf '%s\n' "$rel" >> "$CHANGED_LIST"
      [ "$APPLY" = "1" ] && printf '%s' "$out" > "$f"
    fi
  fi

  # occurrences left in place because they sit on a substitution LHS
  for t in $(cut -f1 "$MAPTSV"); do
    case "$t" in ARGS|invocation_directory|justfile|os|arch) continue ;; esac
    c=$(grep -c "s[/|#,!@]{{$t}}" "$f" 2>/dev/null || true)
    PROTECTED=$((PROTECTED + ${c:-0}))
  done
done < <(find "$ROOT" \( -name .git -o -name node_modules \) -prune -o -type f -print0)

if [ "$APPLY" = "1" ]; then echo "  changed: $CHANGED file(s)"; else echo "  would change: $CHANGED file(s)"; fi
head -20 "$CHANGED_LIST" 2>/dev/null | sed 's/^/    /'
echo "  skipped (subject/template): $SKIPPED"
head -10 "$SKIPPED_LIST" 2>/dev/null | awk -F'\t' '{printf "    %s  — %s\n", $1, $2}'
echo "  occurrences left in place (substitution LHS): $PROTECTED"

if [ -s "$REFUSALS" ]; then
  echo "  REFUSED — value not slug-safe for an identifier/URL slot: $(wc -l < "$REFUSALS")"
  head -10 "$REFUSALS" | awk -F'\t' '{printf "    {{%s}} = %s\n      in: %s\n", $1, $2, $3}'
  echo "    Supply PROJECT_SLUG (or REPO_SLUG) in the map so these slots"
  echo "    get a machine-safe value; the display name stays in prose."
  exit 1
fi
exit 0
